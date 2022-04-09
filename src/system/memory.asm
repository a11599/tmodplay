;==============================================================================
; System library - memory management
;------------------------------------------------------------------------------
; Manages conventional and extended memory and enables 32-bit memory access in
; real mode (flat real mode, see https://en.wikipedia.org/wiki/Unreal_mode).
;------------------------------------------------------------------------------
; TODO:
; - Flat real mode under VCPI
; - Extended memory allocation & A20 line enable/disable without HIMEM.SYS
;==============================================================================

cpu 386

%include "system/consts/public.inc"
%include "system/consts/memory.inc"
%include "system/structs/memory.inc"
%include "system/api/pic.inc"
%include "debug/log.inc"

segment system public use16 class=CODE align=16
segment system


;------------------------------------------------------------------------------
; Setup memory management. This must be the first thing to call from the main
; application! Requires DOSSEG (stack be the last segment) and an untouched SP
; register value.
;------------------------------------------------------------------------------
; -> ES - PSP segment
;    SS:SP - Top of application stack
; <- CF - Set if error
;    EAX - Error code if CF is set or amount of available conventional memory
;          in bytes if CF is not set
;    EBX - Size of available extended memory if CF is not set
;------------------------------------------------------------------------------

global sys_mem_setup
sys_mem_setup:
	push ebx
	push ecx

	;----------------------------------------------------------------------
	; Enable flat real mode

	call setup_flat_real_mode
	jc .error

.setup_memory:

	;----------------------------------------------------------------------
	; Determine base address and size of available conventional memory
	; allocated for the program

	xor eax, eax
	mov ax, ss
	shl eax, 4
	movzx ebx, sp
	add ebx, 0x0f + 12		; + 12 = CS, IP (ret. addr), EAX, EBX
	add eax, ebx
	and al, 0xf0			; EAX: address of first available byte
	mov cs:[cmb_base], eax
	xor ebx, ebx
	mov bx, es:[0x02]
	shl ebx, 4
	sub ebx, eax			; EBX: size of available memory
	mov cs:[cmb_size], ebx

	; Initialize conventional memory area

	mov ebx, cs:[cmb_base]
	mov ecx, cs:[cmb_size]
	call init_memory_block_area

	; Allocate extended memory

	call setup_xms

	pop ecx
	add sp, 4			; Discard EBX from stack
	mov eax, cs:[cmb_size]
	mov ebx, cs:[xmb_size]
	clc

.exit:
	retf

.error:
	pop ecx
	pop ebx
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Setup extended memory when XMS manager is present.
;------------------------------------------------------------------------------
; <- CF - Set if XMS manager is not present
;------------------------------------------------------------------------------

setup_xms:
	push eax
	push ebx
	push ecx
	push edx
	push es

	;----------------------------------------------------------------------
	; Allocate largest XMS memory block

	; Get XMS dispatcher address

	mov ax, 0x4300			; Check presence of XMS
	int 0x2f
	cmp al, 0x80
	jne .no_xms
	mov ax, 0x4310			; Get XMS dispatcher address
	int 0x2f
	mov word cs:[xms_dispatcher], bx
	mov word cs:[xms_dispatcher + 2], es

	; Enable the A20 line

	mov ah, 0x05			; Local enable A20 line
	call far [cs:xms_dispatcher]
	test ax, ax
	jz .done

	; Allocate largest XMS block

	mov ah, 0x08			; Get largest block
	call far [cs:xms_dispatcher]
	cmp bl, 0x80
	jae .done
	mov dx, ax
	movzx ecx, ax
	shl ecx, 10
	mov cs:[xmb_size], ecx
	mov ah, 0x09			; Allocate largest block
	call far [cs:xms_dispatcher]
	test ax, ax
	jz .done
	mov cs:[xmb_handle], dx
	mov ah, 0x0c			; Get linear memory address
	call far [cs:xms_dispatcher]
	test ax, ax
	jz .xms_failed
	mov cs:[xmb_base], bx
	mov cs:[xmb_base + 2], dx
	jmp .xms_ready

.xms_failed:

	; XMS memory block already allocated, but failed to get linear address,
	; reset resources; only conventional memory will be available.

	call shutdown_xms
	jmp .done

.xms_ready:

	; Initialize XMS memory block area

	mov ebx, cs:[xmb_base]
	mov ecx, cs:[xmb_size]
	call init_memory_block_area

.done:
	clc

.exit:
	pop es
	pop edx
	pop ecx
	pop ebx
	pop eax
	retn

.no_xms:
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Shutdown memory management. Should be called before exiting to DOS. Memory
; management won't be available after this call and all previously allocated
; memory should be treated as free and should not be written to anymore.
;------------------------------------------------------------------------------
; <- CF - Cleared
;------------------------------------------------------------------------------

global sys_mem_shutdown
sys_mem_shutdown:
	call shutdown_xms
	call shutdown_flat_real_mode

	clc
	retf


;------------------------------------------------------------------------------
; Free up extended memory locked by memory management.
;------------------------------------------------------------------------------

shutdown_xms:
	push ax
	push dx

	cmp dword cs:[xms_dispatcher], 0
	je .exit
	cmp word cs:[xmb_handle], 0
	je .disable_a20

	mov ah, 0x0d			; Lock XMS block
	mov dx, cs:[xmb_handle]
	call far [cs:xms_dispatcher]
	mov ah, 0x0a			; Free XMS block
	call far [cs:xms_dispatcher]
	mov word cs:[xmb_handle], 0

.disable_a20:
	mov ah, 0x06			; Local disable A20 line
	call far [cs:xms_dispatcher]
	mov dword cs:[xms_dispatcher], 0

.exit:
	pop dx
	pop ax
	retn


;------------------------------------------------------------------------------
; Allocate a paragraph-aligned memory block.
;------------------------------------------------------------------------------
; -> AL- Allocation mode (SYS_MEM_*)
;    EBX - Size of memory block in bytes
; <- CF - Set if error
;    EAX - Linear address of allocated memory or error code if CF is set
;------------------------------------------------------------------------------

global sys_mem_alloc
sys_mem_alloc:
	push ebx
	push ecx
	push dx

	mov ecx, ebx

	cmp al, SYS_MEM_LO_HI
	jbe .alloc_cmb			; SYS_MEM_LO or SYS_MEM_LO_HI modes
	cmp al, SYS_MEM_HI_LO
	jbe .alloc_xmb			; SYS_MEM_HI or SYS_MEM_HI_LO modes

	; SYS_MEM_DMA: allocate memory block for ISA DMA

	mov dl, MEM_ALLOC_DMA
	cmp al, SYS_MEM_DMA_LO
	je .alloc_cmb_dma		; Allocate conventional memory only
	mov ebx, cs:[xmb_base]		; Try extended memory first
	test ebx, ebx
	jz .alloc_cmb_dma		; Extended memory not available
	call allocate_memory_block
	jnc .exit

.alloc_cmb_dma:
	mov ebx, cs:[cmb_base]		; Try conventional memory next
	call allocate_memory_block
	jmp .exit

.alloc_cmb:

	; Allocate conventional memory block

	mov ebx, cs:[cmb_base]
	mov dl, MEM_ALLOC_BTM
	call allocate_memory_block
	jnc .exit
	cmp al, SYS_MEM_LO_HI
	je .alloc_xmb			; Attempt extended memory allocation
	stc				; Memory allocation failed
	jmp .exit

.alloc_xmb:

	; Allocate extended memory block

	mov ebx, cs:[xmb_base]
	test ebx, ebx
	jz .check_hi_lo			; Extended memory not available
	mov dl, MEM_ALLOC_TOP
	call allocate_memory_block
	jnc .exit

.check_hi_lo:
	cmp al, SYS_MEM_HI_LO
	je .alloc_cmb			; Attempt conventional memory allocation
	stc				; Memory allocation failed

.exit:
	pop dx
	pop ecx
	pop ebx
	retf


;------------------------------------------------------------------------------
; Release a previously allocated memory block.
;------------------------------------------------------------------------------
; -> EAX - Linear address of allocated memory
; <- CF - Set if error
;    EAX - Error code if CF is set
;------------------------------------------------------------------------------

global sys_mem_free
sys_mem_free:
	call free_memory_block
	retf


;------------------------------------------------------------------------------
; Initialize memory block area.
;------------------------------------------------------------------------------
; -> EBX - Linear address of memory area
;    ECX - Size of memory area in bytes
; <- EBX - Actual start of memory area (paragraph aligned)
;    ECX - Actual size of memory area available after initialization
;------------------------------------------------------------------------------

init_memory_block_area:
	push eax
	push ds

	xor ax, ax
	mov ds, ax			; DS: zeropage

	; Address/memory size alignments

	mov eax, ebx
	add ebx, 15
	and bl, 0xf0			; Align start address to paragraph
	sub eax, ebx
	add ecx, eax			; Decrease by start alignment bytes
	and cl, 0xf0			; Align size to paragraph
	sub ecx, mcb.strucsize		; Decrease by MCB size

	; Initialize MCB in memory area

	mov word [ebx + mcb.signature], MCB_SIGNATURE
	mov word [ebx + mcb.status], 0
	mov dword [ebx + mcb.size], ecx
	mov dword [ebx + mcb.prev], -1
	mov dword [ebx + mcb.next], -1

	pop ds
	pop eax
	retn


;------------------------------------------------------------------------------
; Allocate memory block in a specific memory block area.
;------------------------------------------------------------------------------
; -> EBX - Pointer to memory block area
;    ECX - Size of memory area to allocate in bytes (max. 64 KB for DMA)
;    DL - Memory allocation control value (MEM_ALLOC_*)
; <- CF - Set if allocation failed
;    EAX - Linear address of memory block reserved or error code if CF set
;------------------------------------------------------------------------------

allocate_memory_block:
	push ebx
	push ecx
	push esi
	push ebp
	push ds

	xor ax, ax
	mov ds, ax			; DS: zeropage
	add ecx, 15
	and cl, 0xf0			; Align size to paragraph

	;----------------------------------------------------------------------
	; Find a memory block of suitable size
	; - MEM_ALLOC_BTM: Bottom-up, allocate starting from beginning of memory
	;   area
	; - MEM_ALLOC_TOP: Top-down, allocate starting from top of memory area
	; - MEM_ALLOC_DMA: Allocate block below 16 MB, not crossing DMA page
	;   boundary (64 KB)

	xor ebp, ebp			; EBP: address of matching block
	cmp dl, MEM_ALLOC_TOP		; Top-down: EBP = 0
	je .check_block
	not ebp				; Bottom-up/DMA: EBP = 0xffffff

.check_block:

	; Check if the memory block is large enough and available

	cmp word [ebx + mcb.signature], MCB_SIGNATURE
	mov eax, 0x07
	jne .error			; MCB chain corrupted
	cmp word [ebx + mcb.status], MCB_FREE
	jne .next_block			; Block already in use
	cmp [ebx + mcb.size], ecx
	jb .next_block			; Block too small

	; Check allocation control

	cmp dl, MEM_ALLOC_DMA
	je .check_dma			; Allocating for DMA; special checks
	cmp dl, MEM_ALLOC_TOP
	je .top_down

	cmp ebx, ebp			; Bottom-up allocation
	ja .next_block			; Block address higher than found, skip
	jmp .save_block

.top_down:
	cmp ebx, ebp			; Top-down allocation
	jb .next_block			; Block address lower, than found, skip

.save_block:
	mov ebp, ebx			; Save possible block address

.next_block:

	; Check next block

	mov ebx, [ebx + mcb.next]	; Next MCB
	cmp ebx, -1
	jne .check_block

	; No more blocks, did we found something?

	mov eax, 0x08
	test ebp, ebp			; Top-down: no block found
	jz .error
	cmp ebp, 0xffffffff		; Bottom-up/DMA: no block found
	je .error
	mov ebx, ebp

	; Yes, bottom-up allocation: split and use lower part of the block

	cmp dl, MEM_ALLOC_BTM
	je .split_lower

	; Top-down allocation: split and use upper part of the block

	lea eax, [ecx + mcb.strucsize]	; EAX: memory required for upper part
	cmp eax, [ebx + mcb.size]
	jae .allocate_current_block	; Block fits perfectly, don't split
	mov eax, [ebx + mcb.size]
	sub eax, ecx
	sub eax, mcb.strucsize
	mov ecx, eax			; ECX: size of lower split part
	cmp ecx, MIN_BLOCK_SIZE
	jb .allocate_current_block	; New lower part too small, allocate all
	mov dh, MEM_SPLIT_FORCE
	call split_memory_block		; Split block
	cmp eax, -1
	je .allocate_current_block	; No split, allocate current block
	jmp .allocate_block		; Split, allocate upper block

.check_dma:

	;----------------------------------------------------------------------
	; Check if memory block is suitable for DMA
	; - Must be below 16 MB
	; - Cannot cross 64 KB boundary (page)
	; - Use first block which fits

	cmp ebx, DMA_MAX_ADDRESS
	jae .next_block			; Block non-DMA addressable

	; Check if DMA page would be crossed

	lea eax, [ebx + mcb.strucsize]
	and eax, 0xffff0000
	add eax, 0x00010000		; EAX: start of next DMA page
	lea esi, [ebx + ecx + mcb.strucsize]
	cmp esi, eax			; ESI: end address of allocation
	jbe .split_lower		; DMA addressable, block found

	; DMA page crossed

	lea esi, [eax + ecx]		; ESI: end address of page-aligned block
	cmp esi, DMA_MAX_ADDRESS
	jae .next_block			; Block at next page non-DMA addressable

	; Check if block is large enough for DMA page-aligned allocation

	sub esi, mcb.strucsize
	sub esi, ebx			; ESI: minimum block size required
	cmp esi, [ebx + mcb.size]
	ja .next_block			; Block too small

	; Force split block to create a page-aligned upper block. The lower
	; block is used as padding for page-alignment and the upper part is
	; split further as necessary.

	push ecx
	mov ecx, eax
	sub ecx, ebx
	sub ecx, mcb.strucsize * 2	; ECX: size of lower (padding) block
	mov dh, MEM_SPLIT_FORCE
	call split_memory_block
	mov ebx, eax			; Use the page-aligned upper block
	pop ecx				; ECX: restore wanted block size

.split_lower:

	; Split block and use lower part for allocation

	mov dh, MEM_SPLIT_NOR
	call split_memory_block

.allocate_current_block:
	mov eax, ebx

.allocate_block:

	; Mark split block as used an return its address

	mov word [eax + mcb.status], MCB_USED
	add eax, mcb.strucsize		; Skip MCB
	clc
	jmp .exit

.error:
	stc

.exit:
	pop ds
	pop ebp
	pop esi
	pop ecx
	pop ebx
	retn


;------------------------------------------------------------------------------
; Free memory block.
;------------------------------------------------------------------------------
; -> EAX - Linear address of memory block
; <- CF - Set if failed
;    EAX - Error code if failed
;------------------------------------------------------------------------------

free_memory_block:
	push ebx
	push ecx
	push edx
	push ds
	push eax

	lea ebx, [eax - mcb.strucsize]	; EBX: MCB address
	xor ax, ax
	mov ds, ax			; DS: zeropage

	; Free memory block

	mov eax, 0x07
	cmp word [ebx + mcb.signature], MCB_SIGNATURE
	jne .error			; MCB chain destroyed
	mov eax, 0x09
	cmp word [ebx + mcb.status], MCB_USED
	jne .error			; Invalid memory block address
	mov word [ebx + mcb.status], MCB_FREE

	; Merge with neighbour blocks when possible

	mov ecx, [ebx + mcb.next]
	call merge_memory_blocks
	jc .error
	mov ecx, eax
	mov ebx, [eax + mcb.prev]
	call merge_memory_blocks
	jc .error

	pop eax
	clc
	jmp .exit

.error:
	add sp, 4			; Discard EAX from stack
	stc

.exit:
	pop ds
	pop edx
	pop ecx
	pop ebx
	retn


;------------------------------------------------------------------------------
; Split memory block.
;------------------------------------------------------------------------------
; -> EBX - Address of memory block MCB to split
;    ECX - Size of lower split area in bytes, must be smaller than the size of
;          memory block being split. When forcing split, the block must
;          also have enough space for the new MCB.
;    DH - Memory split control value (MEM_SPLIT_*)
;    DS - Zeropage
; <- EAX - Linear address of new upper memory block MCB or -1 if split was not
;          done since the new block would have been too small (not with
;          MEM_SPLIT_FORCE)
;------------------------------------------------------------------------------

split_memory_block:
	push esi

	mov eax, [ebx + mcb.size]
	sub eax, ecx			; EAX: size of new upper block

	cmp dh, MEM_SPLIT_FORCE
	je .split			; Force memory block split
	cmp eax, MIN_BLOCK_SIZE + mcb.strucsize
	jae .split			; New block size reasonable, split

	mov eax, -1			; New block too small, don't split
	jmp .exit

.split:

	; EBX: B1 = lower block (block being split)
	; ESI: B2 = new upper block
	; EAX: B3 = next block

	mov [ebx + mcb.size], ecx	; B1.size = new lower block size

	; Create new upper block

	lea esi, [ebx + ecx + mcb.strucsize]
	sub eax, mcb.strucsize		; EAX: size of new upper block

	mov word [esi + mcb.signature], MCB_SIGNATURE
	mov word [esi + mcb.status], MCB_FREE
	mov [esi + mcb.size], eax
	mov [esi + mcb.prev], ebx	; B2.prev = B1
	mov eax, [ebx + mcb.next]
	mov [esi + mcb.next], eax	; B2.next = B3

	; Adjust block pointers

	mov [ebx + mcb.next], esi	; B1.next = B2
	mov [eax + mcb.prev], esi	; B3.prev = B2

	mov eax, esi			; Return new upper block MCB

.exit:
	pop esi
	retn


;------------------------------------------------------------------------------
; Merge memory blocks (when possible).
;------------------------------------------------------------------------------
; -> EBX - Address of first memory block MCB to merge or -1
;    ECX - Address of second memory block MCB to merge or -1
;    DS - Zeropage
; <- CF - Set if error
;    EAX - Error code if CF set or linear address of merged memory block
;------------------------------------------------------------------------------

merge_memory_blocks:
	push edx

	; Both EBX and ECX must point to valid MCBs and have the same status
	; for merge

	cmp ebx, -1
	je .done
	cmp ecx, -1
	je .done
	mov dx, [ebx + mcb.status]
	cmp [ecx + mcb.status], dx
	jne .done
	mov eax, 0x07			; EAX: MCB chain destroyed error code
	cmp word [ebx + mcb.signature], MCB_SIGNATURE
	jne .error
	cmp word [ecx + mcb.signature], MCB_SIGNATURE
	jne .error

	; Check if blocks are really neighbours

	mov edx, ecx
	sub edx, [ebx + mcb.size]
	sub edx, mcb.strucsize
	cmp edx, ebx
	jne .done

	; Merge blocks

	mov edx, [ebx + mcb.size]
	add edx, [ecx + mcb.size]
	add edx, mcb.strucsize		; EDX: size of merged block
	mov [ebx + mcb.size], edx

	; EBX: B1 - first block
	; ECX: B2 - second block
	; EDX: B3 - next block or -1

	mov edx, [ecx + mcb.next]
	mov [ebx + mcb.next], edx	; B1.next = B3
	cmp edx, -1
	je .done			; No B3 (next) block
	cmp word [edx + mcb.signature], MCB_SIGNATURE
	jne .error			; Invalid B3 (next) block
	mov [edx + mcb.prev], ebx	; B3.prev = B1
	jmp .done

.error:
	stc
	jmp .exit

.done:
	mov eax, ebx
	clc

.exit:
	pop edx
	retn


;------------------------------------------------------------------------------
; Log and verify memory blocks to console (debug tool).
;------------------------------------------------------------------------------

%ifdef __DEBUG__

global log_mcbs
log_mcbs:
	push eax
	push ebx
	push edx
	push ds

	xor bx, bx
	mov ds, bx

	log {'Conventional memory blocks', 13, 10, '--------------------------', 13, 10, 13, 10}
	mov ebx, cs:[cmb_base]
	call .log_area_mcbs

	mov ebx, cs:[xmb_base]
	test ebx, ebx
	jz .done
	log {13, 10, 'Extended memory blocks', 13, 10, '----------------------', 13, 10, 13, 10}
	call .log_area_mcbs

.done:
	pop ds
	pop edx
	pop ebx
	pop eax
	retf

.log_area_mcbs:
	mov edx, -1

.loop_mcb:
	add ebx, mcb.strucsize
	log {'@{X}: '}, ebx
	sub ebx, mcb.strucsize
	cmp word [ebx + mcb.signature], MCB_SIGNATURE
	jne .chain_broken
	cmp word [ebx + mcb.status], MCB_FREE
	jne .used
	log {'free, '}
	jmp .log_size
.used:	log {'used, '}

.log_size:
	mov eax, [ebx + mcb.size]
	log {'{X} bytes'}, eax

	cmp edx, [ebx + mcb.prev]
	je .check_next
	log {' !! BAD PREV: expecting @{X}, got @'}, edx
	mov edx, [ebx + mcb.prev]
	log {'{X}'}, edx

.check_next:
	cmp dword [ebx + mcb.next], -1
	je .exit

	lea eax, [ebx + eax + mcb.strucsize]
	cmp eax, [ebx + mcb.next]
	je .next_mcb
	log {' !! BAD NEXT: expecting @{X}, got @'}, eax
	mov edx, [ebx + mcb.next]
	log {'{X}'}, edx

.next_mcb:
	log {13, 10}
	mov edx, ebx
	mov ebx, [ebx + mcb.next]
	jmp .loop_mcb

.chain_broken:
	log {'!! MCB CHAIN BROKEN !!'}

.exit:
	log {13, 10}

	retn

%endif


;==============================================================================
; Flat real mode handling
;==============================================================================

;------------------------------------------------------------------------------
; Initialize flat real mode
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - Error code if CF is set
;------------------------------------------------------------------------------

setup_flat_real_mode:
	push es
	push eax

	; Flat real mode is not (yet ;) possible in a V86 task

	smsw ax
	test al, 0x01
	jz .setup_gdt
	mov eax, SYS_ERR_V86
	jmp .error

.setup_gdt:

	; Setup minimal GDT for later

	xor eax, eax
	mov ax, cs
	shl eax, 4
	add eax, gdt
	mov cs:[gdt_base], eax

	; Install handler for General Protection Exception to update the
	; descriptor cache when an attempt is made to read/write beyond 64 KB.
	; This won't run if the CPU is already in flat real mode (for example,
	; HIMEM.SYS puts it into flat real mode and keeps it that way).

	xor ax, ax
	mov es, ax
	mov eax, es:[0x0d * 4]
	mov cs:[int13_prev_handler], eax
	mov ax, cs
	shl eax, 16
	mov ax, int13_handler
	mov es:[0x0d * 4], eax

	pop eax
	clc

.exit:
	pop es
	retn

.error:
	add sp, 4			; Discard EAX from stack
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Shutdown flat real mode
;------------------------------------------------------------------------------

shutdown_flat_real_mode:
	push eax
	push bx
	push es

	; Remove General Protection Exception handler

	xor ax, ax
	mov es, ax
	mov eax, cs:[int13_prev_handler]
	mov es:[0x0d * 4], eax

	; Restore segment limit to 64KB if memory management changed it to 4 GB

	cmp byte cs:[gpe_raised], 1
	jne .exit
	mov bx, SEG_LIMIT_64K
	call set_segment_limit
	mov byte cs:[gpe_raised], 0

.exit:
	pop es
	pop bx
	pop eax
	retn


;------------------------------------------------------------------------------
; General protection exception handler: this is raised upon the first attempt
; to read/write above 64 KB. Setup segment registers for flat real mode memory
; access in such cases.
;------------------------------------------------------------------------------

	alignb 4

int13_handler:
	push ax

	; Check if the interrupt originates from IRQ5

	sys_pic_irq_serviced 5
	jnz .irq5

	; No, it should be a GP exception, setup descriptor cache

	push bx
	mov bx, SEG_LIMIT_4G
	call set_segment_limit
	mov byte cs:[gpe_raised], 1
	pop bx

.irq5:
	pop ax

	; Continue with previous interrupt handler

	jmp 0x1234:0x1234
	int13_prev_handler EQU $ - 4


;------------------------------------------------------------------------------
; Set segment limit to 4 GB or 64 KB.
;------------------------------------------------------------------------------
; -> BX - Wanted segment descriptor to set limit (SEG_LIMIT_*)
;------------------------------------------------------------------------------

set_segment_limit:
	push eax

	cli
	push ds
	push es
	push fs
	push gs

	mov ax, cs
	mov ds, ax

	lgdt [gdt_register]

	mov eax, cr0
	or al, 1
	mov cr0, eax
	jmp $ + 2

	mov ds, bx
	mov es, bx
	mov fs, bx
	mov gs, bx

	and al, 0xfe
	mov cr0, eax
	jmp $ + 2

	pop gs
	pop fs
	pop es
	pop ds
	sti

	pop eax
	retn


;==============================================================================
; Data area
;==============================================================================

gpe_raised	db 0			; Flag to track GP exceptions

		alignb 2
xmb_handle	dw 0			; XMS memory block handle
		alignb 4
xms_dispatcher	dd 0			; Address of XMS dispatcher
cmb_base	dd 0			; Conventional memory area linear addr.
cmb_size	dd 0			; Size of conventional memory area
xmb_base	dd 0			; Extended memory area linear address
xmb_size	dd 0			; Size of extended memory area

gdt_register	dw GDT_LIMIT
gdt_base	dd 0
gdt		dd 0, 0			; Entry 0 is unused
gdt_data4g	gdt_entry 0, 0xfffff, GDT_FLG_GRANUL | GDT_FLG_BIG | GDT_FLG_PRESENT | GDT_FLG_TYPE_DS | GDT_FLG_WRITE
gdt_data64k	gdt_entry 0, 0xffff, GDT_FLG_PRESENT | GDT_FLG_TYPE_DS | GDT_FLG_WRITE
		GDT_LIMIT EQU $ - gdt - 1

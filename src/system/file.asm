;==============================================================================
; System library - file management
;------------------------------------------------------------------------------
; Extends DOS by adding support to 32-bit addresses and counts (can read/write
; more, than 64KB of data and can read/write to/from extended memory).
;==============================================================================

cpu 386

%include "system/api/memory.inc"
%include "system/consts/file.inc"
%include "debug/log.inc"

segment system public use16 class=CODE align=16
segment system


;------------------------------------------------------------------------------
; Setup file management. This must be called before doing any file operations
; with the functions exported from this module.
;------------------------------------------------------------------------------
; Reserves a low memory buffer for disk IO operations that target extended
; memory. Requires system/memory.
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - Error code if CF is set
;------------------------------------------------------------------------------

global sys_file_setup
sys_file_setup:
	push ebx
	push eax

	mov ebx, BUF_SIZE		; Reserve conventional memory IO buffer
	mov al, SYS_MEM_LO
	call far sys_mem_alloc
	jc .error
	shr eax, 4
	mov cs:[buffer_seg], ax
	pop eax

.exit:
	pop ebx
	retf

.error:
	add sp, 4
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Shutdown file management. Further operations cannot be made before
; sys_file_setup is called again.
;------------------------------------------------------------------------------

global sys_file_shutdown
sys_file_shutdown:
	push eax

	movzx eax, word cs:[buffer_seg]	; Deallocate conventional memory buffer
	shl eax, 4
	call far sys_mem_free

	pop eax
	clc
	retf


;------------------------------------------------------------------------------
; Open file.
;------------------------------------------------------------------------------
; -> AL - Mode (0 = read, 1 = write, 2 = both, 3 = create)
;    DS:ESI - Pointer to ASCIIZ filename
; <- CF - Set if error
;    EAX - File handle or error code if CF is set
;------------------------------------------------------------------------------

global sys_file_open
sys_file_open:
	push cx
	push edx
	push ds

	cld

	; Check if filename is in extended memory

	xor edx, edx
	mov dx, ds
	shl edx, 4
	add edx, esi
	cmp edx, 0x100000
	ja .copy_filename

	; Filename is in conventional memory, convert to 16-bit segment:offset

	push eax
	mov eax, edx
	shr eax, 4
	mov ds, ax
	pop eax
	and edx, 0x0f			; DS:DX: pointer to filename
	jmp .call_dos

.copy_filename:

	; Filename is in extended memory, copy to conventional memory buffer

	push ax
	push esi
	push di
	push es

	mov es, cs:[buffer_seg]
	xor di, di			; ES:DI: conventional memory buffer
	mov cx, BUF_SIZE		; CX: size of buffer (prevent overflow)

.loop_copy_filename:
	a32 lodsb			; Copy next character
	stosb
	test al, al			; Check end of string
	loopnz .loop_copy_filename

	test cx, cx			; Check buffer overflow
	pop es
	pop di
	pop esi
	pop ax
	jz .invalid_filename		; Buffer overflow, filename too long

	mov ds, cs:[buffer_seg]
	xor dx, dx			; DS:DX: buffer with copied filename

.call_dos:
	mov ah, 0x3d
	cmp al, 3
	jne .open
	mov ah, 0x3c			; Create file
	mov cx, 0x20			; Set default file attribute

.open:
	int 0x21
	movzx eax, ax			; Extend return code

.exit:
	pop ds
	pop edx
	pop cx
	retf

.invalid_filename:
	mov eax, 0x02			; Filename too long, return file not
	stc				; found error code
	jmp .exit


;------------------------------------------------------------------------------
; Close file.
;------------------------------------------------------------------------------
; -> EBX - File handle
; <- CF - Set if error
;    EAX - Error code if CF is set
;------------------------------------------------------------------------------

global sys_file_close
sys_file_close:
	push ax

	mov ah, 0x3e
	int 0x21
	jc .error

	pop ax
	retf

.error:
	add sp, 2			; Discard AX from stack
	movzx eax, ax
	stc
	retf


;------------------------------------------------------------------------------
; Read from file.
;------------------------------------------------------------------------------
; -> EBX - File handle
;    ECX - Number of bytes to read
;    EDI - Linear address of target buffer receiving file data
; <- CF - Set if error
;    EAX - Number of bytes read or error code if CF is set
;------------------------------------------------------------------------------

global sys_file_read
sys_file_read:
	push ecx
	push edx
	push esi
	push edi
	push ebp
	push ds
	push es

	cld

	xor eax, eax			; Zero high word
	mov es, ax			; ES: zeropage
	xor edx, edx			; EDX: number of bytes read
	mov ebp, ecx			; EBP: number of bytes to read

.loop_read:
	cmp edi, 0x100000
	jb .direct_read

.loop_read_xmb:

	; Read chunk into conventional memory IO buffer

	mov ecx, ebp			; ECX: number of bytes to read
	cmp ecx, BUF_SIZE
	jbe .read_to_buffer
	mov ecx, BUF_SIZE

.read_to_buffer:
	push dx
	mov ah, 0x3f
	mov ds, cs:[buffer_seg]
	xor dx, dx
	int 0x21			; Read chunk from file
	pop dx
	jc .exit			; Operation failed
	add edx, eax			; Increase number of bytes read
	cmp ax, cx
	jne .eof_buffer			; End of file
	sub ebp, eax			; Decrease number of bytes to read
	jmp .copy_from_buffer

.eof_buffer:
	xor ebp, ebp			; EOF, copy data to target, then return

.copy_from_buffer:

	; Copy from conventional memory IO buffer to target memory address, use
	; doubleword copy to speed things up, then copy rest of remaining bytes

	xor esi, esi
	mov cx, ax
	shr cx, 2			; ECX: number of bytes read / 4
	jz .copy_bytes
	a32 rep movsd			; DS:[ESI] -> ES:[EDI]

.copy_bytes:
	mov cx, ax
	and cx, 0x03			; ECX: number of bytes left (0 - 3)
	jz .next_chunk_to_buffer
	a32 rep movsb			; DS:[ESI] -> ES:[EDI]

.next_chunk_to_buffer:
	test ebp, ebp			; Read next chunk
	jz .done
	jmp .loop_read_xmb

.direct_read:

	; Read directly into target buffer if it is in conventional memory

	push edx
	mov edx, edi
	shr edx, 4
	mov ds, dx
	mov dx, di
	and dx, 0x0f			; DS:DX: segmented address of target

	mov ecx, ebp			; ECX: number of bytes to read
	cmp ecx, 0xfff0
	jbe .read_to_target
	mov ecx, 0xfff0

.read_to_target:
	mov ah, 0x3f
	int 0x21
	pop edx
	jc .exit
	add edx, eax			; Increase number of bytes read
	add edi, eax			; Advance target address
	cmp ax, cx
	jne .done			; End of file
	sub ebp, eax			; Decrease number of bytes to read
	jnz .loop_read			; Read next chunk, may be in high mem.

.done:
	mov eax, edx			; Return number of bytes read
	clc

.exit:
	pop es
	pop ds
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	retf


;------------------------------------------------------------------------------
; Write to file.
;------------------------------------------------------------------------------
; -> EBX - File handle
;    ECX - Number of bytes to write
;    ESI - Linear address of source buffer containing file data
; <- CF - Set if error
;    EAX - Number of bytes written or error code if CF is set
;------------------------------------------------------------------------------

global sys_file_write
sys_file_write:
	push ecx
	push edx
	push esi
	push edi
	push ebp
	push ds
	push es

	cld

	xor eax, eax			; Zero high word
	xor edx, edx			; EDX: number of bytes written
	mov ebp, ecx			; EBP: number of bytes to write

.loop_write:
	cmp esi, 0x100000
	jb .direct_write

.loop_write_xmb:
	mov ecx, ebp			; ECX: number of bytes to write
	cmp ecx, BUF_SIZE
	jbe .copy_to_buffer
	mov ecx, BUF_SIZE

.copy_to_buffer:

	; Copy from conventional memory IO buffer to target memory address, use
	; doubleword copy to speed things up, then copy rest of remaining bytes

	xor ax, ax
	mov ds, ax			; DS: zeropage
	mov es, cs:[buffer_seg]
	xor edi, edi			; ES:EDI: conventional memory IO buffer
	mov ax, cx
	shr cx, 2			; ECX: number of bytes to write / 4
	jz .copy_bytes
	a32 rep movsd			; DS:[ESI] -> ES:[EDI]

.copy_bytes:
	mov cx, ax
	and cx, 0x03			; ECX: number of bytes left (0 - 3)
	jz .write_from_buffer
	a32 rep movsb			; DS:[ESI] -> ES:[EDI]

.write_from_buffer:

	; Write chunk from conventional memory IO buffer

	push dx
	mov cx, ax
	mov ah, 0x40
	mov ds, cs:[buffer_seg]
	xor dx, dx
	int 0x21			; Write chunk to file
	pop dx
	jc .exit			; Operation failed
	add edx, eax			; Increase number of bytes written
	cmp ax, cx
	jne .done			; End of file
	sub ebp, eax			; Decrease number of bytes to write

.next_chunk_from_buffer:
	test ebp, ebp			; Write next chunk
	jz .done
	jmp .loop_write_xmb

.direct_write:

	; Write directly from source buffer if it is in conventional memory

	push edx
	mov edx, esi
	shr edx, 4
	mov ds, dx
	mov dx, si
	and dx, 0x0f			; DS:DX: segmented address of source

	mov ecx, ebp			; ECX: number of bytes to write
	cmp ecx, 0xfff0
	jbe .write_from_source
	mov ecx, 0xfff0

.write_from_source:
	mov ah, 0x40
	int 0x21
	pop edx
	jc .exit
	add edx, eax			; Increase number of bytes written
	add esi, eax			; Advance source address
	cmp ax, cx
	jne .done			; End of file
	sub ebp, eax			; Decrease number of bytes to write
	jnz .loop_write			; Write next chunk, may be in high mem.

.done:
	mov eax, edx			; Return number of bytes written
	clc

.exit:
	pop es
	pop ds
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	retf


;------------------------------------------------------------------------------
; Move file read/write position (seek).
;------------------------------------------------------------------------------
; -> AL - Seek origin (0 = start of file, 1 = current position, 2 = end of file)
;    EBX - File handle
;    ECX - New position
; <- CF - Set if error
;    EAX - Error code if CF is set or actual new position
;------------------------------------------------------------------------------

global sys_file_set_pos
sys_file_set_pos:
	push ecx			; Keep this in sync with
	push dx				; sys_file_get_pos!

file_seek:
	mov ah, 0x42
	mov dx, cx
	shr ecx, 16			; CX:DX: new position
	int 0x21
	movzx eax, ax
	jc .exit

	movzx ecx, dx
	shl ecx, 16
	add eax, ecx			; EAX: actual new position

.exit:
	pop dx
	pop ecx
	retf


;------------------------------------------------------------------------------
; Get the current read/write position of a file.
;------------------------------------------------------------------------------
; -> EBX - File handle
; <- CF - Set if error
;    EAX - Error code if CF is set or current position
;------------------------------------------------------------------------------

global sys_file_get_pos
sys_file_get_pos:
	push ecx			; Keep this in sync with
	push dx				; sys_file_set_pos!

	mov al, 1
	xor ecx, ecx
	jmp file_seek


;------------------------------------------------------------------------------
; Get the size of a file.
;------------------------------------------------------------------------------
; -> EBX - File handle
; <- CF - Set if error
;    EAX - Error code if CF set or size of file
;------------------------------------------------------------------------------

global sys_file_get_size
sys_file_get_size:
	push ecx
	push edx

	call far sys_file_get_pos	; Get current position
	jc .exit
	mov edx, eax			; EDX: current position

	mov al, 2			; Move to end of file
	xor ecx, ecx
	call far sys_file_set_pos
	jc .exit

	mov ecx, edx			; ECX: current position
	mov edx, eax			; EDX: size of file
	mov al, 0			; Restore original file position
	call far sys_file_set_pos
	jc .exit

	mov eax, edx			; EAX: size of file

.exit:
	pop edx
	pop ecx
	retf


;------------------------------------------------------------------------------
; Get the linear address of the conventional memory file I/O buffer.
;------------------------------------------------------------------------------
; <- EBX - Linear address of the I/O buffer
;------------------------------------------------------------------------------

global sys_file_get_buf_addr
sys_file_get_buf_addr:
	movzx ebx, word cs:[buffer_seg]
	shl ebx, 4
	retf


;==============================================================================
; Data area
;==============================================================================

		alignb 2
buffer_seg	dw 0			; Segment of convetional memory buffer

;==============================================================================
; System library - memory handling operations
;------------------------------------------------------------------------------
; Constants
;==============================================================================

MCB_SIGNATURE	EQU 'MB'		; Memory control block signature
MIN_BLOCK_SIZE	EQU 1024		; Minimum block split size
DMA_MAX_ADDRESS	EQU 0x01000000		; Maximum DMA memory address (16 MB)

; System features

FEAT_V86	EQU 0x01		; V86 mode
FEAT_XMS	EQU 0x02		; XMS memory manager present
FEAT_VCPI	EQU 0x04		; VCPI manager present
FEAT_FAST_A20	EQU 0x08		; Fast A20 gate control available

; Extended memory modes

XMEM_NA		EQU 0			; Extended memory not available
XMEM_RAW	EQU 1			; Raw memory management (INT 15h)
XMEM_XMS	EQU 2			; XMS memory management

; A20 line control modes

A20_NA		EQU 0			; A20 not available
A20_ENABLED	EQU 1			; A20 already enabled
A20_XMS		EQU 2			; XMS manager
A20_FAST	EQU 3			; I/O port 92h
A20_BIOS	EQU 4			; PS/2 BIOS services
A20_KBD_CTRLR	EQU 5			; 8042 keyboard controller

; Memory control block status

MCB_FREE	EQU 0			; Block available
MCB_USED	EQU 1			; Block used

; Memory allocation control values

MEM_ALLOC_BTM	EQU 0			; Bottom-up memory allocation
MEM_ALLOC_TOP	EQU 1			; Top-down memory allocation
MEM_ALLOC_DMA	EQU 2			; Allocate memory for DMA

; Memory split control values

MEM_SPLIT_NOR	EQU 0			; Don't split if new block too small
MEM_SPLIT_FORCE	EQU 1			; Force split regardless new block size

; Selector constants

SEL_FLAT	EQU 1 << 3		; 32-bit 0-base data segment selector
SEL_64K		EQU 2 << 3		; 16-bit 0-base data segment selector
SEL_VCPI	EQU 3 << 3		; VCPI code segment selector
SEL_CS		EQU 6 << 3		; 16-bit code segment selector
SEL_DS		EQU 7 << 3		; 32-bit data segment selector
SEL_SS		EQU 8 << 3		; 16-bit stack segment selector
SEL_TSS		EQU 9 << 3		; TSS selector

; Descriptor flags (generic for each type)

DESC_PAGE_GRAN	EQU 0x8000		; Granularity: 4 KB (page)
DESC_AVAILABLE	EQU 0x1000		; Available = 1
DESC_DPL_MASK	EQU 0x0060		; Destination privilege level bitmask
DESC_DPL_0	EQU 0x0000		; Destination privilege level 0
DESC_DPL_1	EQU 0x0020		; Destination privilege level 1
DESC_DPL_2	EQU 0x0040		; Destination privilege level 2
DESC_DPL_3	EQU 0x0060		; Destination privilege level 3

; Segment descriptors

SEG_PRESENT	EQU 0x0080		; Code/data segment: segment present
SEG_CS		EQU 0x0018		; Code segment
SEG_CS_32BIT	EQU 0x4000		; Code segment: 32 bit
SEG_CS_CONFRMNG	EQU 0x0004		; Code segment: conforming (caller CPL)
SEG_CS_READABLE	EQU 0x0002		; Code segment: readable
SEG_CS_ACCESSED	EQU 0x0001		; Code segment: accessed
SEG_DS		EQU 0x0010		; Data segment
SEG_DS_BIG	EQU 0x4000		; Data segment: big (use ESP for stack)
SEG_DS_EXP_DOWN	EQU 0x0004		; Data segment: expand down
SEG_DS_WRITABLE	EQU 0x0002		; Data segment: writable
SEG_DS_ACCESSED	EQU 0x0001		; Data segment: accessed
SEG_TSS		EQU 0x0009		; Task state segment
SEG_TSS_BUSY	EQU 0x0002		; Task state segment: busy (active)

; Gate descriptors

GATE_PRESENT	EQU 0x0080		; Gate: present
GATE_CALL	EQU 0x000c		; Call gate
GATE_TASK	EQU 0x0005		; Task gate
GATE_INTERRUPT	EQU 0x000e		; Interrupt gate (CLI on enter)
GATE_TRAP	EQU 0x000f		; Trap gate

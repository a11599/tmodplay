;==============================================================================
; System library - memory handling operations
;------------------------------------------------------------------------------
; Constants
;==============================================================================

MCB_SIGNATURE	EQU 'MB'		; Memory control block signature
MIN_BLOCK_SIZE	EQU 1024		; Minimum block split size
DMA_MAX_ADDRESS	EQU 0x01000000		; Maximum DMA memory address (16 MB)

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

; Segment limit constants

SEG_LIMIT_64K	EQU 16			; 16-bit segment limit (64 KB)
SEG_LIMIT_4G	EQU 8			; 32-bit segment limit (4 GB)

; GDT flag constants

GDT_FLG_GRANUL	EQU 0x8000		; Granularity: 4 KB
GDT_FLG_BIG	EQU 0x4000		; Big (data segments)
GDT_FLG_DEFAULT	EQU 0x4000		; Default (code segments)
GDT_FLG_AVL_1	EQU 0x1000		; Available = 1
GDT_FLG_AVL_0	EQU 0x0000		; Available = 0
GDT_FLG_PRESENT	EQU 0x0080		; Segment present
GDT_FLG_DPL_0	EQU 0x0000		; Destination privilege level 0
GDT_FLG_DPL_1	EQU 0x0020		; Destination privilege level 1
GDT_FLG_DPL_2	EQU 0x0040		; Destination privilege level 2
GDT_FLG_DPL_3	EQU 0x0060		; Destination privilege level 3
GDT_FLG_TYPE_DS	EQU 0x0010		; Data segment descriptor
GDT_FLG_TYPE_CS	EQU 0x0010		; Code segment descriptor
GDT_FLG_TYPE_S	EQU 0x0000		; System segment descriptor
GDT_FLG_EXP_DWN	EQU 0x0008		; Expand down
GDT_FLG_CONFRMG	EQU 0x0004		; Conforming
GDT_FLG_WRITE	EQU 0x0002		; Writeable (data segments)
GDT_FLG_READ	EQU 0x0002		; Readable (code segments)
GDT_FLG_ACCESSD	EQU 0x0001		; Accessed


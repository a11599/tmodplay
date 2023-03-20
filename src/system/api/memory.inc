;==============================================================================
; System library - memory handling operations
;------------------------------------------------------------------------------
; API
;==============================================================================

%include "system/consts/public.inc"

extern sys_mem_setup
extern sys_mem_shutdown
extern sys_mem_alloc
extern sys_mem_free

extern sys_get_int_handler
extern sys_set_int_handler


;------------------------------------------------------------------------------
; Calculate the linear address of a segment
;------------------------------------------------------------------------------
; -> %1 - Segment address (register or memory, can't be %2)
;    %2 - Target register receiving linear memory address
;------------------------------------------------------------------------------

%macro	segment_address 2

	xor %2, %2
	%ifidni %2, eax
	mov ax, %1
	%elifidni %2, ebx
	mov bx, %1
	%elifidni %2, ecx
	mov cx, %1
	%elifidni %2, edx
	mov dx, %1
	%elifidni %2, esi
	mov si, %1
	%elifidni %2, edi
	mov di, %1
	%elifidni %2, ebp
	mov bp, %1
	%endif
	shl %2, 4

%endmacro

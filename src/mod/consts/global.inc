;==============================================================================
; MOD player
;------------------------------------------------------------------------------
; Global constants
;==============================================================================

; Maximum number of channels supported by the player.

MOD_MAX_CHANS	EQU 32

;------------------------------------------------------------------------------
; Macro to reserve memory for a union of structures.
;------------------------------------------------------------------------------
; -> Name of structures. Each must have a .strucsize label that contains the
;    number of bytes required by the structure.
;------------------------------------------------------------------------------

%ifnmacro mod_union
%macro	mod_union 1-*

	%assign strucsize 0

	%rep %0
	%if %1.strucsize > strucsize
	%assign strucsize %1.strucsize
	%endif
	%rotate 1
	%endrep

	resb strucsize

%endmacro
%endif

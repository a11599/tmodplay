;==============================================================================
; MOD player - Data conversions
;------------------------------------------------------------------------------
; API
;==============================================================================

extern mod_note_finetune
extern mod_convert_period_to_note
extern mod_period_floor_seminote


;------------------------------------------------------------------------------
; Convert a MOD period value such as slide to internal period (period * 16).
; Use mod_period() for constants and mod_convert_period for memory/register
; values.
;------------------------------------------------------------------------------
; -> %1 - Register/memory address containing MOD period value
;    %2 - Optional bitshift value applied during conversion on period value,
;         use positive value for left and negative for right bitshift
; <- %1 - Register/memory address containing internal period value
;------------------------------------------------------------------------------

%define	mod_period(period) period * 16
%macro	mod_convert_period 1-2 0
	%assign bitshift 4 + %2

	%if bitshift > 0
	shl %1, bitshift
	%elif bitshift < 0
	shr %1, -1 * bitshift
	%endif
%endmacro

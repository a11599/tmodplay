;==============================================================================
; MOD player GUI
;------------------------------------------------------------------------------
; Public constants
;==============================================================================

; Error codes

GUI_ERR_NOVGA	EQU 0x200		; No supported graphical mode available
GUI_ERR_FONT	EQU 0x201		; Unsupported font

; Alignment

GUI_AL_LEFT	EQU 0			; Align left to render origin
GUI_AL_RIGHT	EQU 1			; Align right to render origin
GUI_AL_CENTER	EQU 2			; Center around render origin

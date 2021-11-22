;==============================================================================
; MOD player
;------------------------------------------------------------------------------
; Public constants
;==============================================================================

; Error codes

MOD_ERR_INVALID	EQU -1			; Invalid file format
MOD_ERR_DEV_UNK	EQU -2			; Unknown device
MOD_ERR_NB_CHN	EQU -3			; Too many or no channels
MOD_ERR_PLAYING	EQU -4			; Already playing
MOD_ERR_DEVICE	EQU -5			; Output device not responding

; MOD file flags

MOD_FLG_PAN	EQU 0x00000001		; MOD uses 8xx or E8x effect
MOD_FLG_EXT_OCT	EQU 0x00000002		; MOD uses non-ProTracker periods

; Output devices

MOD_OUT_NONE	EQU 0			; No sound (but still run the player)
MOD_OUT_DAC	EQU 1			; PC speaker/LPT DAC/no sound output
MOD_OUT_SB	EQU 2			; Sound Blaster output

; Output device types

MOD_NONE	EQU 0			; No sound (but still runs the player)

MOD_DAC_SPEAKER	EQU 1			; PC speaker
MOD_DAC_LPT	EQU 2			; Single mono LPT DAC
MOD_DAC_LPTST	EQU 3			; Stereo LPT DAC on one port
MOD_DAC_LPTDUAL	EQU 4			; Dual mono LPT DACs on two ports

MOD_SB_2	EQU 0			; Sound Blaster 2.0
MOD_SB_PRO	EQU 1			; Sound Blaster Pro
MOD_SB_16	EQU 2			; Sound Blaster 16

; Stereo panning methods

MOD_PAN_MONO	EQU 0			; Force mono rendering on stereo device
MOD_PAN_HARD	EQU 1			; Hard panning left/right (Amiga-style)
MOD_PAN_CROSS	EQU 2			; Crossfade 20%, ignore panning commands
MOD_PAN_REAL	EQU 3			; Real panning (use effects 8xx and E8x)

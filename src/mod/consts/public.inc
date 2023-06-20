;==============================================================================
; MOD player
;------------------------------------------------------------------------------
; Public constants
;==============================================================================

; Error codes

MOD_ERR_INVALID	EQU 0x100		; Invalid file format
MOD_ERR_DEV_UNK	EQU 0x101		; Unknown device
MOD_ERR_NB_CHN	EQU 0x102		; Too many or no channels
MOD_ERR_STATE	EQU 0x103		; Invalid MOD player state for action
MOD_ERR_DEVICE	EQU 0x104		; Output device not responding

; MOD file flags

MOD_FLG_PAN	EQU 0x00000001		; MOD uses 8xx or E8x effect
MOD_FLG_EXT_OCT	EQU 0x00000002		; MOD uses non-ProTracker periods

; Output devices

MOD_OUT_NONE	EQU 0			; No sound (but still run the player)
MOD_OUT_DAC	EQU 1			; PC speaker/LPT DAC/no sound output
MOD_OUT_SB	EQU 2			; Sound Blaster output

; Output device types

MOD_NONE	EQU 0			; No sound (but still runs the player)

MOD_DAC_SPEAKER	EQU 0			; PC speaker
MOD_DAC_LPT	EQU 1			; Single mono LPT DAC
MOD_DAC_LPTST	EQU 2			; Stereo LPT DAC on one port
MOD_DAC_LPTDUAL	EQU 3			; Dual mono LPT DACs on two ports

MOD_SB_1	EQU 0			; Sound Blaster (single-cycle DMA only)
MOD_SB_2	EQU 1			; Sound Blaster 2.0
MOD_SB_PRO	EQU 2			; Sound Blaster Pro
MOD_SB_16	EQU 3			; Sound Blaster 16

; Stereo panning methods

MOD_PAN_MONO	EQU 0			; Force mono rendering on stereo device
MOD_PAN_HARD	EQU 1			; Hard panning left/right (Amiga-style)
MOD_PAN_CROSS	EQU 2			; Crossfade 20%, ignore panning commands
MOD_PAN_REAL	EQU 3			; Real panning (use effects 8xx and E8x)

; Sample interpolation methods

MOD_IPOL_NN	EQU 0			; Nearest neighbour interpolation
MOD_IPOL_LINEAR	EQU 1			; Linear interpolation
MOD_IPOL_WATTE	EQU 2			; Watte tri-linear interpolation

; Output buffer formats

MOD_BUF_DEPTH	EQU 0x03		; Bitdepth mask
MOD_BUF_8BIT	EQU 0x00		; 8-bit output buffer in 8-bit bytes
MOD_BUF_16BIT	EQU 0x01		; 16-bit output buffer in 16-bit words
MOD_BUF_1632BIT	EQU 0x02		; 16-bit output buffer in 32-bit words

MOD_BUF_CHANNEL	EQU 0x0C		; Channel mask
MOD_BUF_1CHN	EQU 0x00		; 1 channel buffer (mono)
MOD_BUF_2CHN	EQU 0x04		; 2 channel buffer (stereo)
MOD_BUF_2CHNL	EQU 0x08		; 2 channel, only left used (mono)

MOD_BUF_RANGE	EQU 0x10		; Value range mask
MOD_BUF_INT	EQU 0x00		; Signed sample data
MOD_BUF_UINT	EQU 0x10		; Unsigned sample data

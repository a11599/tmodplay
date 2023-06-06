;==============================================================================
; MOD player - Output device
;------------------------------------------------------------------------------
; Constants
;==============================================================================

; Output device bitstream format

FMT_BITDEPTH	EQU 0x01		; Bitdepth mask
FMT_8BIT	EQU 0x00		; 8 bit output
FMT_16BIT	EQU 0x01		; 16 bit output

FMT_CHANNELS	EQU 0x02		; Channel mask
FMT_MONO	EQU 0x00		; Mono
FMT_STEREO	EQU 0x02		; Stereo

FMT_RANGE	EQU 0x04		; Range mask
FMT_SIGNED	EQU 0x00		; Signed bitstream format
FMT_UNSIGNED	EQU 0x04		; Unsigned bitstream format

; Flags for set_sample

SMP_CHANGE	EQU 0x01
SMP_SET_POS	EQU 0x02

; Flags for set_mixer

MIX_SET_VOL	EQU 0x01
MIX_SET_PAN	EQU 0X02
MIX_SET_SPD	EQU 0X04

; Buffer rendering status

BUF_READY	EQU 0
BUF_RENDERING	EQU 1
BUF_RENDER_1	EQU 2
BUF_RENDER_2	EQU 3
BUF_RENDER_3	EQU 4

;==============================================================================
; MOD player - software wavetable
;------------------------------------------------------------------------------
; Structures
;==============================================================================

%include "mod/consts/global.inc"

;------------------------------------------------------------------------------
; Internal data structure for audio channel status.
;------------------------------------------------------------------------------

struc		channel

.sample_pos_int	resd 1			; Sample playback position
.speed_int	resd 1			; Integer part of playback speed
.sample_hdr_ofs	resw 1			; Sample data
.sample_pos_fr	resw 1			; Fractional part of sample position
.speed_fr	resw 1			; Fractional part of playback speed
.volume		resb 1			; Volume (0 - 64)
.pan		resb 1			; Panning (0 - 255)
.left_volume	resb 1			; Left channel volume (0 - 64)
.left_bitshift	resb 1			; Left channel bitshift (0 - 5)
.left_round	resb 1			; Left channel bitshift rounding corr.
.right_volume	resb 1			; Right channel volume (0 - 64)
.right_bitshift	resb 1			; Right channel bitshift (0 - 5)
.right_round	resb 1			; Right channel bitshift rounding corr.
		alignb 4
.strucsize:

endstruc

;------------------------------------------------------------------------------
; Internal wavetable data segment structure.
;------------------------------------------------------------------------------

struc		mod_wtbl_sw

.voltab_seg	resw 1			; Segment address of volume table
.channel_ofs	resw MOD_MAX_CHANS	; Channel descriptor offset index table
.amplify	resw 1			; Amplification
.output_format	resb 1			; Output device bitstream format
		alignb 4
.buffer_addr	resd 4			; Output render buffer address
		alignb 4		; channel[]
.channels	resb channel.strucsize * MOD_MAX_CHANS
.strucsize:

endstruc

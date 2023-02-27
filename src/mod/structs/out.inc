;==============================================================================
; MOD player - Output device
;------------------------------------------------------------------------------
; Global structures
;==============================================================================

;------------------------------------------------------------------------------
; Near function pointers for output devices (internal). Each output device must
; provide these functions with a specific API.
;------------------------------------------------------------------------------

struc	mod_out_fns

.setup:		resw 1			; Set up the output device
.shutdown	resw 1			; Shutdown the output device
.upload_sample	resw 1			; Initialize sample for wavetable mixer
.free_sample	resw 1			; Dispose sample from wavetable mixer
.set_amplify	resw 1			; Apply the amplification value
.set_interpol	resw 1			; Set sample interpolation mode
.set_stereomode	resw 1			; Set stereo rendering mode
.play		resw 1			; Start playback
.stop		resw 1			; Stop playback
.set_tick_rate	resw 1			; Set playroutine tick rate
.set_mixer	resw 1			; Set volume, panning and playback speed
.set_sample	resw 1			; Set sample to be played
.render		resw 1			; Render audio into the output buffer
.get_mixer_info	resw 1			; Add mixer info into mod_channel_info
.get_info	resw 1			; Get output device status info
		alignb 4
.strucsize:

endstruc

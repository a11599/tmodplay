;==============================================================================
; MOD player - Output device
;------------------------------------------------------------------------------
; Global structures
;==============================================================================

;------------------------------------------------------------------------------
; Near function pointers for output devices (internal). Each output device must
; provide these functions with a specific API.
;------------------------------------------------------------------------------

struc		mod_dev_api

.setup		resd 1			; Set up the output device
.shutdown	resd 1			; Shutdown the output device
.upload_sample	resd 1			; Initialize sample for wavetable mixer
.free_sample	resd 1			; Dispose sample from wavetable mixer
.set_channels	resd 1			; Set number of channels
.set_amplify	resd 1			; Apply the amplification value
.set_interpol	resd 1			; Set sample interpolation mode
.set_stereomode	resd 1			; Set stereo rendering mode
.play		resd 1			; Start playback
.stop		resd 1			; Stop playback
.set_tick_rate	resd 1			; Set playroutine tick rate
.set_mixer	resd 1			; Set volume, panning and playback speed
.set_sample	resd 1			; Set sample to be played
.render		resd 1			; Render audio into the output buffer
.get_chn_info	resd 1			; Get channel info
.get_info	resd 1			; Get output device status info
.get_position	resd 1			; Get playback position info
.reset_channels	resd 1			; Reset playback channels
		alignb 4
.strucsize:

endstruc

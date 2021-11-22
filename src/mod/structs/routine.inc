;==============================================================================
; MOD player - play routine
;------------------------------------------------------------------------------
; Structures
;==============================================================================

%include "mod/consts/global.inc"

;------------------------------------------------------------------------------
; Internal state of the play routine for each channel
;------------------------------------------------------------------------------

struc		mod_routine_channel

.flags:
.mixer_flags	resb 1			; Mixer flags
.sample_flags	resb 1			; Sample flags
.current_note	resb 1			; Note in current row
.note		resb 1			; Channel note
.volume		resb 1			; Channel volume (0 - 64)
.play_volume	resb 1			; Channel volume for playback
.pan		resb 1			; Channel pan (0 - 255)
.sample		resb 1			; Sample number (0 - 32)
.finetune	resb 1			; Channel finetune
.toneporta_dir	resb 1			; Tone portamento direction
.toneporta_spd	resb 1			; Tone portamento speed
.wave_control	resb 1			; Waveform control flags
.vibrato_pos	resb 1			; Vibrato waveform position
.vibrato_param	resb 1			; Vibrato command parameter
.tremolo_pos	resb 1			; Tremolo waveform position
.tremolo_param	resb 1			; Tremolo command parameter
.glissando_flag	resb 1			; Glissando enable / disable
		alignb 2
.sample_ofs	resw 1			; Sample offset position (memory)
.sample_pos	resw 1			; Sample playback start position
.period		resw 1			; Note period
.play_period	resw 1			; Note period for playback
.effect		resw 1			; Effect command (lo) / parameter (hi)
.toneporta_per	resw 1			; Wanted portamento note period
.sample_hdr_ofs	resw 1			; Offset to sample header
		alignb 4
.strucsize:

endstruc

;------------------------------------------------------------------------------
; Internal state of the play routine
;------------------------------------------------------------------------------

struc		mod_routine_state

.flags		resb 1			; Playback state flags
.bpm		resb 1			; Current BPM
.speed		resb 1			; Current speed (ticks/row)
.tick		resb 1			; Current tick
.pattern	resb 1			; Current playing pattern
.row		resb 1			; Current playing row
.position	resb 1			; Current (pattern) sequence position
.break_row	resb 1			; Wanted row after break
.break_position	resb 1			; Wanted sequence position after break
.patt_delay	resb 1			; Wanted pattern delay
.patt_del_count	resb 1			; Pattern delay counter
.loop_start_row	resb 1			; Start row of pattern loop
.loop_count	resb 1			; Pattern loop count
		alignb 2
.pattern_addr	resd 1			; Current playing pattern address
.channels	resb (mod_routine_channel.strucsize * MOD_MAX_CHANS)
		alignb 4
.strucsize:

endstruc
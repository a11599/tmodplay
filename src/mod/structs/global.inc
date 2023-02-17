;==============================================================================
; MOD player
;------------------------------------------------------------------------------
; Global structures
;==============================================================================

%include "mod/consts/global.inc"
%include "mod/structs/public.inc"
%include "mod/structs/out.inc"
%include "mod/structs/routine.inc"
%include "mod/structs/out_none.inc"
%include "mod/structs/out_dac.inc"
%include "mod/structs/out_sb.inc"
%include "mod/structs/wtbl_sw.inc"

;------------------------------------------------------------------------------
; Internal data structure for a sample.
;------------------------------------------------------------------------------

struc		mod_sample

.name		resb 23			; ASCIIZ name of the sample
.volume		resb 1			; Volume (0 - 64)
.addr		resd 1			; Linear address of sample data
.length		resd 1			; Sample length in bytes
.rpt_start	resd 1			; Sample loop repeat start position
.rpt_len	resd 1			; Sample loop repeat length
.wt_id		resd 1			; Sample ID in the wavetable mixer
.finetune	resb 1			; Sample finetune value (-8 - 7)
		alignb 4
.strucsize:

endstruc

;------------------------------------------------------------------------------
; Internal data structure for a module.
;------------------------------------------------------------------------------

struc		mod

		alignb 4		; File operation function far pointers
.file_fns	resb mod_file_fns.strucsize
		alignb 4		; Output device function near pointers
.out_fns	resb mod_out_fns.strucsize
		alignb 4		; Output device parameters
.out_params	resb mod_out_params.strucsize
		alignb 4
.instance_addr	resd 1			; Linear address of this structure
.extra_samples	resd 1			; Number of extra samples for wavetable
		alignb 4		; -------------------------------------
.file_data:				; File-specific data starts
.file_handle	resd 1			; File handle after opened
.title		resb 21			; Song title (ASCIIZ)
.num_channels	resb 1			; Number of channels
.num_samples	resb 1			; Number of samples
.length		resb 1			; Number of entries in sequence
.bpm		resb 1			; Song BPM
.num_patterns	resb 1			; Number of patterns in song
.restart_pos	resb 1			; Restart position
.state		resb 1			; Player status (MOD_STATE_*)
.sequence	resb 128		; Pattern number for each position
		alignb 4
.pattern_addr	resd 256		; Pattern addr rel. to player inst. seg
.sample_hdr_ofs	resw 31			; Offsets of sample header data
		alignb 4		; Sample header data
.sample_hdr	resb (mod_sample.strucsize * 31)
.flags		resd 1			; MOD flags (MOD_FLG_*)
		alignb 4		; File-specific data ends
.file_data_end:				; -------------------------------------
		alignb 4		; Playroutine state memory area
.routine_state	resb mod_routine_state.strucsize
		alignb 4		; Output device state shared memory area
.out_state	mod_union mod_out_none_state, mod_out_dac_state, mod_out_sb_state
		alignb 4		; Wavetable state shared memory area
.wt		mod_union mod_wtbl_sw

		alignb 4
.strucsize:

endstruc

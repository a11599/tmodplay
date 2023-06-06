;==============================================================================
; MOD player
;------------------------------------------------------------------------------
; MOD file data structures
;==============================================================================

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

.title		resb 21			; Song title (ASCIIZ)
.num_channels	resb 1			; Number of channels
.num_samples	resb 1			; Number of samples
.length		resb 1			; Number of entries in sequence
.bpm		resb 1			; Song BPM
.num_patterns	resb 1			; Number of patterns in song
.restart_pos	resb 1			; Restart position
.sequence	resb 128		; Pattern number for each position
		alignb 4
.pattern_addr	resd 256		; Pattern addresses
.sample_hdr_ofs	resd 31			; Sample header data addresses
.sample_hdr	resb (mod_sample.strucsize * 31)
.flags		resd 1			; MOD flags (MOD_FLG_*)
.strucsize:

endstruc

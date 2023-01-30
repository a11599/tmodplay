;==============================================================================
; MOD player
;------------------------------------------------------------------------------
; Public structures
;==============================================================================

;------------------------------------------------------------------------------
; Far function pointers for library dependencies. The caller must provide these
; functions which perform a specific operation using a specific API. Refer to
; the appropriate sys_file_* function for the API specification. It is possible
; to load MODs from packed files or any other source by using some handcrafted
; routines instead of sys_file_* as long as the API matches.
;------------------------------------------------------------------------------

struc		mod_file_fns

.open		resd 1			; Open a file on disk (sys_file_open)
.read		resd 1			; Read from a file (sys_file_read)
.close		resd 1			; Close the file (sys_file_close)
		alignb 4
.strucsize:

endstruc

;------------------------------------------------------------------------------
; Parameters for MOD player output devices. This is just a generic table, refer
; to the output device for details on what to provide as an input for each of
; them.
;------------------------------------------------------------------------------

struc		mod_out_params

.buffer_size	resw 1			; Output buffer size in milliseconds
.port		resw 2			; IO port addresses
.irq		resb 2			; IRQ addresses
.dma		resb 2			; DMA channels
.stereo_mode	resb 1			; Stereo mixing mode
.initial_pan	resb 1			; Initial panning for real stereo mode
.interpolation	resb 1			; Sample interpolation
		alignb 4
.strucsize:

endstruc

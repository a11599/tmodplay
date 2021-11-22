;==============================================================================
; MOD player - software wavetable
;------------------------------------------------------------------------------
; Constants
;==============================================================================

; Number of output samples produced by the unrolled (optimized) mixer code in
; one step.

UNROLL_COUNT	EQU 20			; 20 seems to be the sweet spot

; Maximum sample playback speed which can be mixed by the unrolled mixer code.
; Anything higher will cause crackles and playback issues. 17 is fine
; considering the current lowest period being 27 and the lowest supported
; sample rate being 8000 Hz. 17 is good from 7728 Hz sample rate, so it also
; provides a small safety margin.

UNROLL_MAX_SPD	EQU 17

; Additional memory used to unroll sample for optimized mixer code. Unroll one
; more to support an edge case with partial renders.

UNROLL_SAMPLES	EQU (UNROLL_COUNT + 1) * UNROLL_MAX_SPD

; Render macro operation control constants

RENDER_STORE	EQU 0			; Overwrite buffer with sample (store)
RENDER_ADD	EQU 1			; Mix sample to buffer (add)
RENDER_PAN_L	EQU 0			; Primary channel = left
RENDER_PAN_R	EQU 1			; Primary channel = right
RENDER_PAN_HARD	EQU 0			; Hard left/right panning (Amiga style)
RENDER_PAN_X	EQU 1			; Hard panning with 75% crossfade
RENDER_PAN_REAL	EQU 2			; Real panning using channel pan value

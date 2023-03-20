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

; Additional memory required at the beginning of each sample for the mixer code.

SAMPLE_PADDING	EQU 1

; Linear interpolation exponent. The actual interpolation is 2^LIN_IPOL_EXP.
; 32x interpolation gives enough headroom up to 46.4 kHz. Keep in mind however,
; that 1024 times actual interpolation bytes of low memory will be allocated for
; the interpolation lookup table. For normal Protracker MODs, 8x interpolation
; should be enough up to 46.4 kHz mixing rate.

LIN_IPOL_EXP	EQU 5			; 32x interpolation for full coverage

; Watte tri-linear interpolation exponent. The actual interpolation is
; 2^WATTE_IPOL_EXP. 32x interpolation gives enough headroom up to 46.4 kHz.
; Watte interpolation uses imuls, each additional exponent requires 2 additional
; CPU cycles per channel per sample on the 386 and 486.

WATTE_IPOL_EXP	EQU 5			; 32x interpolation for full coverage

; Render macro operation control constants

RENDER_STORE	EQU 0			; Overwrite buffer with sample (store)
RENDER_ADD	EQU 1			; Mix sample to buffer (add)
RENDER_PAN_L	EQU 0			; Primary channel = left
RENDER_PAN_R	EQU 1			; Primary channel = right
RENDER_PAN_HARD	EQU 0			; Hard left/right panning (Amiga style)
RENDER_PAN_X	EQU 1			; Hard panning with 75% crossfade
RENDER_PAN_REAL	EQU 2			; Real panning using channel pan value
RENDER_IPOL_NN	EQU 0			; Nearest neighbour interpolation
RENDER_IPOL_LIN	EQU 1			; Linear interpolation
RENDER_IPOL_WTL	EQU 2			; Jon Watte tri-linear interpolation

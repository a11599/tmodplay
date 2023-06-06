;==============================================================================
; MOD player - Configuration
;==============================================================================

; Use profiling to aggregate performance counter spent with audio rendering
; when MOD_USE_PROFILER is defined. The profiler must be started by the host
; application before starting MOD playback.

%define MOD_USE_PROFILER 1

; Maximum number of channels supported by the player. Maximum is 255. Each
; additional channel consumes about 100 bytes of memory.

MOD_MAX_CHANS	EQU 32

; Number of output samples produced by the software wavetable mixer code in one
; step. This increases the amount of memory required for each sample slightly,
; but more importantly it increases the generated code size and hence the
; memory requirements of the player. Since unrolling is combined with self
; modifying code, the benefits of unrolls will be lost due to increased setup
; code size. Use somewhere between 8-24, depending on targeted memory
; requirements.

UNROLL_COUNT	EQU 20			; 20 seems to be the sweet spot

; Maximum sample playback speed which can be mixed by the unrolled mixer code.
; Anything higher will cause crackles and playback issues. 17 is fine
; considering the current lowest period being 27 and the lowest supported
; sample rate being 8000 Hz. 17 is good from 7728 Hz sample rate, so it also
; provides a small safety margin.

UNROLL_MAX_SPD	EQU 17

; Linear interpolation exponent. The actual interpolation is 2 ^ LIN_IPOL_EXP.
; 32x interpolation gives enough headroom up to 46.4 kHz. Keep in mind however,
; that 1024 times actual interpolation bytes of memory will be allocated for
; the interpolation lookup table. For normal Protracker MODs, 8x interpolation
; should be enough for up to 46.4 kHz mixing rate.

LIN_IPOL_EXP	EQU 5			; 32x interpolation for full coverage

; Watte tri-linear interpolation exponent. The actual interpolation is
; 2 ^ WATTE_IPOL_EXP. 32x interpolation gives enough headroom up to 46.4 kHz.
; Watte interpolation uses imuls, each additional exponent requires 2 additional
; CPU cycles per channel per sample on the 386 and 486.

WATTE_IPOL_EXP	EQU 5			; 32x interpolation for full coverage

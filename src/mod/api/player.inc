;==============================================================================
; MOD player - public interface
;------------------------------------------------------------------------------
; API
;==============================================================================

%include "mod/consts/public.inc"
%include "mod/structs/public.inc"

extern mod_setup
extern mod_shutdown
extern mod_load
extern mod_unload
extern mod_set_amplify
extern mod_set_interpolation
extern mod_set_stereo_mode
extern mod_get_flags
extern mod_get_channel_count
extern mod_play
extern mod_stop
extern mod_render
extern mod_sb_detect

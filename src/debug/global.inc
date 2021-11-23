;==============================================================================
; Debug logging tools during development
;------------------------------------------------------------------------------
; -> DEBUG_BUILD environment variable: logging is only generated if it exists
;    If the environment variable is not defined, no debug-related code will be
;    added to the application.
;------------------------------------------------------------------------------
; All macros can be called at any point in the code, they don't alter flags or
; registers. Macros emit a far call at the point of inclusion (taking 5 bytes)
; into the current segment. The logging code itself will be output to the
; "debug" segment and log messages (via the log macro) are put into the
; "debug_data" segment.
;==============================================================================

cpu 386

%ifenv DEBUG_BUILD

%ifndef __DEBUG__
%define __DEBUG__
[section debug_data public class=DATA use16 align=1]
[section debug public class=CODE use16 align=4]
__SECT__

%endif

LOG_STDOUT	EQU 0x00		; Print log to standard output
LOG_STDERR	EQU 0x01		; Print log to standard error
LOG_FILE	EQU 0x02		; Print log to a file
LOG_AUTOCOMMIT	EQU 0x04		; Flush logfile after each write
LOG_APPEND	EQU 0x08		; Append to file instead overwrite

%endif

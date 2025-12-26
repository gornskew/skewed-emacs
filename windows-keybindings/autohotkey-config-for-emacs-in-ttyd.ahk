#Requires AutoHotkey v2.0
#SingleInstance Force

; Active only for Chrome / Edge (any tab, for now)
#HotIf WinActive("ahk_exe chrome.exe") || WinActive("ahk_exe msedge.exe")

; Movement keys: map to arrows so Chrome never sees Ctrl+N/Ctrl+P.
^n::Send "{Down}"   ; C-n -> next-line
^p::Send "{Up}"     ; C-p -> previous-line

; Keep these as real Ctrl combos for Emacs (and C-x C-f, etc.)
^b::Send "^b"
^f::Send "^f"       ; C-x C-f still works
^x::Send "^x"
^s::Send "^s"

^/::Send "^_"

^w::Send "{Esc}]" ; Bind M-] to kill-region in emacs

^!f::Send "{Esc}^f"
^!b::Send "{Esc}^b"
^!k::Send "{Esc}^k"
^!q::Send "{Esc}^q"


#HotIf  ; end conditional

#Requires AutoHotkey v2.0
#SingleInstance Force

^+v::Send "{Ctrl down}v{Ctrl up}" ; Ctrl + Shift + V: Paste (system-wide)

; Edge bindings (Emacs-like in text fields)
#HotIf WinActive("ahk_exe msedge.exe")
^p::Send "{Up}"
^n::Send "{Down}"
^f::Send "{Right}"
^b::Send "{Left}"
^d::Send "{Delete}"
^v::Send "{PgDn}"
!f::Send "{Ctrl down}{Right}{Ctrl up}"
!b::Send "{Ctrl down}{Left}{Ctrl up}"
^!f::Send "{Ctrl down}{Right}{Ctrl up}"
^!b::Send "{Ctrl down}{Left}{Ctrl up}"
!d::Send "{Ctrl down}{Delete}{Ctrl up}"
^a::Send "{Home}"
^e::Send "{End}"
^k::Send "{Shift down}{End}{Shift up}{Ctrl down}x{Ctrl up}"
#HotIf

; Claude bindings
#HotIf WinActive("ahk_exe claude.exe")
^p::Send "{Up}"
^n::Send "{Down}"
^f::Send "{Right}"
^b::Send "{Left}"
^d::Send "{Delete}"
^v::Send "{PgDn}"
!f::Send "{Ctrl down}{Right}{Ctrl up}"
!b::Send "{Ctrl down}{Left}{Ctrl up}"
^!f::Send "{Ctrl down}{Right}{Ctrl up}"
^!b::Send "{Ctrl down}{Left}{Ctrl up}"
!d::Send "{Ctrl down}{Delete}{Ctrl up}"
^a::Send "{Home}"
^e::Send "{End}"
^k::Send "{Shift down}{End}{Shift up}{Ctrl down}x{Ctrl up}"
#HotIf

; Chrome: Block browser shortcuts so they pass through to web terminal (for Emacs inside)
; No Emacs remaps here — arrows work natively in terminals
#HotIf WinActive("ahk_exe chrome.exe")
^p::Send "{Up}"
^n::Send "{Down}"
^t::Send "{Ctrl t}"
^w::Send "{Ctrl w}"          ; Block Ctrl+W (close tab)
^+t::return         ; Block Ctrl+Shift+T (reopen closed tab)
^+n::return         ; Block Ctrl+Shift+N (incognito window) — optional
; Add more if needed, e.g. ^+w::return for Ctrl+Shift+W (close window)
#HotIf
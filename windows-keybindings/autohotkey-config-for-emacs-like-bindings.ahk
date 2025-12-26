#Requires AutoHotkey v2.0
#SingleInstance Force

^+v::Send "{Ctrl down}v{Ctrl up}" ; Ctrl + Shift + V: Paste (system-wide)

; Edge bindings (Emacs-like in text fields)
#HotIf WinActive("ahk_exe msedge.exe")
^p::Send "{Up}"
^n::Send "{Down}"
;;^f::Send "{Right}"
^f::return 
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

; Chrome: Block browser shortcuts so raw Ctrl combos pass to web terminal (Emacs)
#HotIf WinActive("ahk_exe chrome.exe")

; Remap these for regular web pages (optional — remove if you want pure pass-through)
^p::Send "{Up}"
^n::Send "{Down}"

; Block browser actions — do NOTHING, so raw keys go to page
^t::return          ; New tab
^w::return          ; Close tab
^/::return          ; Find (Ctrl+/ is "Find on page" in some Chrome versions/layouts)
^l::return          ; Focus address bar (common conflict)
^+t::return         ; Reopen closed tab
^+n::return         ; Incognito
^+w::return         ; Close window

#HotIf

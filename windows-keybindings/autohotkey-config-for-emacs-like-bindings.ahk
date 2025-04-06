#Requires AutoHotkey v2.0
#SingleInstance Force

^+v::Send "{Ctrl down}v{Ctrl up}" ; Ctrl + Shift + V: Paste (system-wide)

; Edge bindings
#HotIf WinActive("ahk_exe msedge.exe")
^p::Send "{Up}"          ; C-p: Up
^n::Send "{Down}"        ; C-n: Down
^f::Send "{Right}"       ; C-f: Right
^b::Send "{Left}"        ; C-b: Left
^d::Send "{Delete}"      ; C-d: Delete right
^v::Send "{PgDn}"        ; C-v: Scroll down (Page Down)
!f::Send "{Ctrl down}{Right}{Ctrl up}"  ; M-f: Word right
!b::Send "{Ctrl down}{Left}{Ctrl up}"   ; M-b: Word left
^!f::Send "{Ctrl down}{Right}{Ctrl up}" ; M-C-f: Word right
^!b::Send "{Ctrl down}{Left}{Ctrl up}"  ; M-C-b: Word left
!d::Send "{Ctrl down}{Delete}{Ctrl up}" ; M-d: Delete word forward
^a::Send "{Home}"        ; C-a: Beginning of line
^e::Send "{End}"         ; C-e: End of line
^k::Send "{Shift down}{End}{Shift up}{Ctrl down}x{Ctrl up}" ; C-k: Kill to end of line
#HotIf

; Claude bindings (adjust exe name if needed)
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
!d::Send "{Ctrl down}{Delete}{Ctrl up}" ; M-d: Delete word forward
^a::Send "{Home}"        ; C-a: Beginning of line
^e::Send "{End}"         ; C-e: End of line
^k::Send "{Shift down}{End}{Shift up}{Ctrl down}x{Ctrl up}" ; C-k: Kill to end of line
#HotIf
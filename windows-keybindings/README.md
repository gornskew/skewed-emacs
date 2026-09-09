# Windows Keybindings for Emacs Users

This directory contains configuration files to make Windows more Emacs-friendly by:
1. Remapping CapsLock to Control
2. Setting up Emacs-style navigation keys in various applications
3. **Stopping the browser from stealing Emacs chords when Emacs runs
   in a web terminal (ttyd)** — see [Emacs in a browser tab](#emacs-in-a-browser-tab-ttyd)
   below. If you came here because `C-n` opened a new browser window,
   start there.

## Files

- **sharpkeys-capslock-to-control.skl**: SharpKeys configuration to remap CapsLock to Control
- **autohotkey-config-for-emacs-like-bindings.ahk**: AutoHotkey script for Emacs-style navigation keys in ordinary browser pages and the Claude desktop app
- **autohotkey-config-for-emacs-in-ttyd.ahk**: AutoHotkey script for Emacs running in a ttyd browser terminal — the one that keeps `C-n`, `C-p` and `C-w` out of the browser's hands

Each of these is a small text configuration you feed to a tool you
install yourself. This directory deliberately ships no third-party
executables — see below.

## Sanskrit / IAST transliteration (KeySwap)

For typing IAST diacritics (ā ī ū ṛ ṝ ḷ ṭ ḍ ṇ ś ṣ ṃ ḥ …) on Windows,
**KeySwap** works well: hold a base letter's key and cycle through its
diacritic variants.

Get it from the author: <https://www.YesVedanta.com/keyswap>.

`keyswap.exe` used to be committed here. It was removed 2026-08-16: it
is someone else's compiled Windows binary, redistributed with no license
grant of any kind — no LICENSE file, no terms in its ReadMe, nothing
that says we may pass it on. A repo that ships its own license carefully
should not casually redistribute a binary it has no right to. Install it
from the source above instead.

Its `config.txt` maps each base letter to the variants it cycles
through; the shipped default already covers IAST, so there is usually
nothing to configure.

## Setup Instructions

### Remapping CapsLock to Control

1. Download and install [SharpKeys](https://github.com/randyrants/sharpkeys/releases)
2. Open SharpKeys and click "Import"
3. Select the `sharpkeys-capslock-to-control.skl` file
4. Click "Write to Registry"
5. Log out and log back in (or restart your computer) for changes to take effect

### Setting Up Emacs Navigation Keys (ordinary pages)

1. Download and install [AutoHotkey v2](https://www.autohotkey.com/)
2. Copy the `autohotkey-config-for-emacs-like-bindings.ahk` file to your preferred location
3. Double-click the file to run it
4. To make it start automatically with Windows:
   - Press `Win+R`, type `shell:startup` and press Enter
   - Create a shortcut to the .ahk file in this folder

## Available Keybindings (emacs-like-bindings script)

`autohotkey-config-for-emacs-like-bindings.ahk` provides the following
Emacs-style keybindings in Microsoft Edge, Chrome and the Claude
desktop app. They turn Emacs chords into the arrow and editing keys
that ordinary text fields understand, so they are for normal web pages
— NOT for a terminal tab, where they would hide the real chords from
Emacs. Press `F12` to toggle the script on and off (the tooltip
confirms which).

| Keybinding | Function |
|------------|----------|
| Ctrl+p | Up arrow |
| Ctrl+n | Down arrow |
| Ctrl+f | Right arrow |
| Ctrl+b | Left arrow |
| Ctrl+a | Home (beginning of line) |
| Ctrl+e | End (end of line) |
| Ctrl+d | Delete character |
| Ctrl+k | Kill to end of line |
| Ctrl+v | Page down |
| Alt+f | Move forward one word |
| Alt+b | Move backward one word |
| Alt+d | Delete word forward |
| Ctrl+Alt+f | Move forward one word (alternative) |
| Ctrl+Alt+b | Move backward one word (alternative) |

## Emacs in a browser tab (ttyd)

The web terminal on port 6942 (and any hosted session such as
<https://hack.genworks.com/on?demo=staircase>) runs Emacs inside a
browser tab through [ttyd](https://github.com/tsl0922/ttyd). Most
Emacs chords reach it fine. A handful never arrive, because the
browser reserves them for itself before the page ever sees a key:

| Chord | Edge / Chrome does | Emacs wanted |
|-------|--------------------|--------------|
| `C-n` | new window | `next-line` |
| `C-p` | print — the terminal usually claims it first, but not on every build, so the script covers it too | `previous-line` |
| `C-w` | close the tab (!) | `kill-region` |
| `C-t` | new tab | `transpose-chars` |
| `C-S-n`, `C-S-t`, `C-S-w` | InPrivate window, reopen tab, close window | (rarely needed) |

`C-w` is the dangerous one: it closes the tab your Emacs session is
in. Chromium treats new-window / new-tab / close-tab as *reserved*
commands, which means no web page — ttyd included — can intercept
them, no matter what its JavaScript does. Everything else (`C-f`,
`C-b`, `C-a`, `C-e`, `C-k`, `C-s`, `C-x ...`, `C-c ...`, the Meta
chords) already gets through, because ttyd's terminal claims those
keys before the browser acts on them.

Two fixes, in the order to try them. The first is what we ship and
needs no administrator rights; the second frees the real control
characters.

### Step 1 — run the shipped AutoHotkey script (recommended first)

`autohotkey-config-for-emacs-in-ttyd.ahk` intercepts the reserved
chords at the operating-system level, before Edge or Chrome sees
them, and sends something the terminal can deliver to Emacs instead:

| You press | The script sends | Emacs does |
|-----------|------------------|------------|
| `C-n` | Down arrow | `next-line` (same command, different key) |
| `C-p` | Up arrow | `previous-line` |
| `C-w` | `M-]` | `kill-region` — the shipped Emacs config binds `M-]` to `kill-region` precisely for this (`dot-files/emacs.d/init.el`) |
| `C-/` | `C-_` | `undo` (some terminals cannot send `C-/`) |
| `C-M-f`, `C-M-b`, `C-M-k`, `C-M-q` | `ESC C-f` etc. | `forward-sexp`, `backward-sexp`, `kill-sexp`, `indent-sexp` — the `C-M-` chords that Windows otherwise eats |
| `C-b`, `C-f`, `C-x`, `C-s` | themselves | passed through untouched, listed only so `C-x C-f` and friends keep working |

To use it:

1. Install [AutoHotkey v2](https://www.autohotkey.com/) (v2, not v1 —
   the script starts with `#Requires AutoHotkey v2.0` and v1 will
   refuse it).
2. Copy `autohotkey-config-for-emacs-in-ttyd.ahk` somewhere permanent
   and double-click it. A green "H" icon appears in the system tray
   while it runs.
3. Open a ttyd page in Edge or Chrome — your own web terminal at
   `http://localhost:6942`, or a hosted session — and try `C-n`,
   `C-p`, then `C-SPC`, a few `C-n`, `C-w`, `C-y`. Line motion works
   and the tab stays open.
4. To start it with Windows: `Win+R`, type `shell:startup`, Enter, and
   drop a shortcut to the `.ahk` file in that folder.
5. After editing the script, right-click the tray icon and choose
   "Reload Script".

Things to know:

- The script is active for **every** Edge and Chrome window (`#HotIf
  WinActive("ahk_exe chrome.exe") || WinActive("ahk_exe msedge.exe")`),
  not only ttyd tabs. That means `C-n` no longer opens a new browser
  window anywhere while it runs, and `C-w` no longer closes tabs. Use
  the mouse or the menus for those, or exit the script from the tray
  icon when you are done with the terminal.
- **Do not run both AutoHotkey scripts at once with both active.** The
  emacs-like-bindings script also claims `C-f`, `C-b`, `C-a`, `C-e`,
  `C-k` and `C-d` in the browser and turns them into arrow and editing
  keys, which stops Emacs from seeing the real chords. If you keep
  both running, press `F12` to switch the emacs-like-bindings script
  OFF before you go to a terminal tab, and ON again afterwards. The
  simplest arrangement is one script at a time.
- Firefox users: add `|| WinActive("ahk_exe firefox.exe")` to the
  `#HotIf` line. Firefox lets pages see more control chords than
  Chromium does, but `C-n`, `C-t` and `C-w` are still reserved there.
- Arrow keys instead of `C-n`/`C-p` are indistinguishable for line
  motion, but a few Emacs modes bind `C-n` and `<down>` differently
  (some completion popups, for example). If that bothers you, Step 2
  lets the script send the real control character.

### Step 2 — tell Edge to release the shortcuts (registry policy)

Microsoft Edge (Windows only, Edge 101 and later) has a policy,
`ConfigureKeyboardShortcuts`, that unbinds the keyboard shortcut from
a list of commands. The commands still work from the menus; only the
key combination is released, so the page — ttyd — receives it as a
normal control character. Chrome has no equivalent policy.

Run this in PowerShell (no administrator rights needed; Edge reads
mandatory policies from the current-user hive as well as the machine
hive):

```powershell
New-Item -Path "HKCU:\SOFTWARE\Policies\Microsoft\Edge" -Force | Out-Null
Set-ItemProperty -Path "HKCU:\SOFTWARE\Policies\Microsoft\Edge" `
  -Name ConfigureKeyboardShortcuts -Type String `
  -Value '{"disabled":["new_window","new_tab","close_tab","print","find","save_page","open_file","downloads","history"]}'
```

Then **restart Edge completely** (close every window; the policy does
not refresh dynamically). Open `edge://policy` and confirm
`ConfigureKeyboardShortcuts` is listed with status OK. If it is not
there, your machine may be managed and only the `HKLM:` hive is
honored; ask your administrator, or stay with Step 1.

What each entry releases (the full list of command names is on
Microsoft's [Configurable Microsoft Edge commands](https://learn.microsoft.com/deployedge/edge-learnmore-configurable-edge-commands)
page):

| Command | Shortcut released | Emacs gets |
|---------|-------------------|------------|
| `new_window` | Ctrl+N | `C-n` for real |
| `new_tab` | Ctrl+T | `C-t` |
| `close_tab` | Ctrl+W | `C-w` for real |
| `print` | Ctrl+P | `C-p` for real |
| `find` | Ctrl+F | belt and braces; ttyd already claims it |
| `save_page` | Ctrl+S | belt and braces |
| `open_file` | Ctrl+O | belt and braces |
| `downloads` | Ctrl+J | belt and braces |
| `history` | Ctrl+H | belt and braces for the help prefix |

Optional extras if you use them in Emacs: `reopen_tab` (Ctrl+Shift+T),
`close_window` (Ctrl+Shift+W), `new_inprivate_window` (Ctrl+Shift+N),
`select_next_tab` / `select_previous_tab` (Ctrl+PgDn / Ctrl+PgUp) and
`select_tab_0` … `select_tab_7` plus `select_last_tab` (Ctrl+1 … 9,
Emacs's digit arguments).

With the policy in place you can, if you like, make the AutoHotkey
script send the real control characters instead of arrows: change the
two movement lines to

```autohotkey
^n::Send "{Ctrl down}n{Ctrl up}"
^p::Send "{Ctrl down}p{Ctrl up}"
```

That only works once `new_window` and `print` are unbound; with the
stock browser it would open a window and a print dialog.

### Other browsers

- **Firefox**: pages are allowed to handle more chords than in
  Chromium, but `C-n`, `C-t` and `C-w` stay reserved. Use the
  AutoHotkey script with `firefox.exe` added to its `#HotIf` line.
- **Vivaldi**: Settings → Keyboard → Browser shortcuts lets you delete
  the bindings for Ctrl+N, Ctrl+T, Ctrl+W and Ctrl+F directly in the
  UI; the keys then reach the page. If you live in web terminals,
  Vivaldi is the least painful Chromium-family option.
- **PowerToys Keyboard Manager** can remap a shortcut only while a
  particular application is focused — the same idea as the AutoHotkey
  script, with a graphical editor instead of a text file.

### What will not work

- A userscript, a browser extension, or a ttyd option cannot capture
  Ctrl+N / Ctrl+T / Ctrl+W in stock Edge or Chrome; Chromium handles
  reserved commands before page scripts run.
- The browser Keyboard Lock API only works in JavaScript-initiated
  fullscreen, which a ttyd page is not.
- Remapping CapsLock to Control (SharpKeys, above) is worth doing for
  comfort, but it does nothing about browser shortcuts.

### Practical order

1. Run `autohotkey-config-for-emacs-in-ttyd.ahk` (Step 1). Test in a
   ttyd tab: `C-f`, `C-b`, `C-n`, `C-p`, `C-a`, `C-e`, and `C-w` after
   marking a region.
2. If a chord still misbehaves, or you want the genuine `C-n`/`C-p`/`C-t`
   bytes, apply the Edge policy (Step 2) and restart Edge.
3. Everything else — `C-c`, `C-d`, `C-z`, the `C-x` prefixes, the
   Meta/Alt chords — already reaches ttyd. The fight is only ever
   about the reserved window / tab / find set.

## Extending for Other Applications

To add Emacs keybindings to other applications, you can modify the AutoHotkey script:

1. Open the .ahk file in a text editor
2. Add a new section following the pattern of existing sections
3. Determine the executable name of your application (e.g., `notepad.exe`)
4. Add a new section like this:

```autohotkey
#HotIf WinActive("ahk_exe your_application.exe")
^p::Send "{Up}"
^n::Send "{Down}"
; Add more keybindings as needed
#HotIf
```

5. Save the file and reload the script by right-clicking the AutoHotkey icon in the system tray and selecting "Reload Script"

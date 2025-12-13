;;; skewed-icons.el --- Terminal-safe icon system -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Centralized icon system for skewed-emacs designed for terminal reliability.
;;
;; Four modes, from most compatible to experimental:
;;
;; 1. `ascii' - Pure 7-bit ASCII. Works on ANY terminal.
;;
;; 2. `unicode' - Geometric Unicode symbols guaranteed never to be emojified.
;;    Uses circles, triangles, diamonds - shapes that have no emoji variant.
;;    This is the SAFEST choice for terminal use.
;;
;; 3. `unicode-fancy' - Richer Unicode including characters that MIGHT render
;;    as emoji in some terminals. Appends VS15 (text presentation selector)
;;    to force text mode, but terminal support varies.
;;
;; 4. `nerd' - Nerd Font icons. Requires font setup in terminal.
;;
;; The key insight: Some Unicode characters like ☀ ❤ ⚡ have dual presentation.
;; Terminals may render them as colorful 2-column emoji even when Emacs thinks
;; they're width-1 text. The `unicode' mode avoids these entirely.
;;
;; Usage:
;;   (skewed-icon :bullet)
;;   M-x skewed-icons-set-style

;;; Code:

(defgroup skewed-icons nil
  "Icon configuration for skewed-emacs."
  :group 'skewed-emacs
  :prefix "skewed-icons-")


(defcustom skewed-icons-style 'colorful
  "Icon style to use.
- `ascii': Pure ASCII, universal
- `unicode': Safe geometric Unicode (recommended for older terms)
- `colorful': Windows Terminal style Emojis
- `nerd': Nerd Font icons"
  :type '(choice (const :tag "ASCII" ascii)
                 (const :tag "Unicode geomeric" unicode)
                 (const :tag "Colorful Emoji (WinTerm)" colorful)
                 (const :tag "Nerd Fonts" nerd))
  :group 'skewed-icons)


;;; Width Fixing Logic =========================================================
(defun skewed-icons--fix-widths ()
  "Force Emacs to recognize standard Emoji and Symbol ranges as double-width.
This synchronizes Emacs' internal width calculation with modern terminals."
  ;; 1. Standard Emoji & Pictographs (covers most colorful icons)
  (set-char-table-range char-width-table '(#x1F300 . #x1F9FF) 2)

  ;; 2. Misc Symbols (covers the Gear ⚙ which is usually #x2699)
  ;;    Range #x2600 - #x27BF covers Sun, Cloud, Check, Pointing Fingers, etc.
  (set-char-table-range char-width-table '(#x2600 . #x27BF) 2)

  ;; 3. Dingbats & Geometric Shapes (covers heavy checks, crosses, circles)
  (set-char-table-range char-width-table '(#x2700 . #x27BF) 2)
  
  ;; 4. Supplemental Symbols (covers the Robot 🤖 and other newer additions)
  (set-char-table-range char-width-table '(#x1F900 . #x1F9FF) 2)
  
  ;; 5. Transport and Map Symbols (covers the Airplane ✈ and Rocket 🚀)
  (set-char-table-range char-width-table '(#x1F680 . #x1F6FF) 2)

  ;; 6. Enclosed Alphanumeric Supplement (covers regional indicators, etc)
  (set-char-table-range char-width-table '(#x1F100 . #x1F1FF) 2)

  ;; 7. Specific overrides for characters often stuck in "text" presentation
  ;;    The Airplane (U+2708) is tricky. It lives in Dingbats but often needs forcing.

  (set-char-table-range char-width-table '(#x1f402 . #x1f402) 2)

  (set-char-table-range char-width-table '(#x1f402 . #x1f50e) 2)

  (set-char-table-range char-width-table '(#x1f938 . #x1f938) 2)
  (set-char-table-range char-width-table '(#x3bb . #x3bb) 1)
  (set-char-table-range char-width-table '(#x26a1 . #x26a1) 1)

  (set-char-table-range char-width-table '(#x2708 . #x2708) 2)

  (message "Skewed-icons: Extended Emoji width fixes applied."))


;;; Icon Tables ================================================================

(defconst skewed-icons--ascii-table
  '((:bullet . "*") (:bullet-hollow . "o") (:bullet-tri . ">") (:bullet-dash . "-")
    (:check . "+") (:cross . "x") (:warning . "!") (:info . "i") (:ok . "+") (:error . "x")
    (:folder . ">") (:folder-open . "v") (:folder-archive . "#") (:file . "-")
    (:help-book . "?") (:help-target . ">") (:help-rocket . "!") (:help-robot . "@") (:help-palette . "~")
    (:sys-info       . "(i)︎")
    (:sys-process . "*") (:sys-memory . "*") (:sys-package . "*") (:sys-version . "*") (:sys-time . "*")
    (:japa-zero . ".") (:japa-progress . "o") (:japa-complete . "+")
    (:japa-bonus1 . "*") (:japa-bonus2 . "#") (:japa-bonus3 . "@") (:japa-epic . "!")
    (:japa-today-l . "") (:japa-today-r . "") (:japa-future . ".")
    (:habit-done . "+") (:habit-today . "#")
    (:svc-ccl . "C") (:svc-sbcl . "S") (:svc-commercial . "$") (:svc-smp . "&")
    (:arrow-right . ">") (:arrow-left . "<") (:star . "*") (:clock . "@")
    (:bell . "*") (:lightning . "!") (:dot . ".") (:ellipsis . "...")

    (:getting-started . ">")
    (:active-projects . "@")
    (:swank-services . "-")
    (:lisply-backends . "_")

    )
  "Pure ASCII - works everywhere.")

(defconst skewed-icons--unicode-table
  '(;; Bullets - basic geometric
    (:bullet         . "•")      ; U+2022 BULLET
    (:bullet-hollow  . "◦")      ; U+25E6 WHITE BULLET
    (:bullet-tri     . "▸")      ; U+25B8 BLACK RIGHT-POINTING SMALL TRIANGLE
    (:bullet-dash    . "─")      ; U+2500 BOX DRAWINGS
    
    ;; Status - safe dingbats
    (:check          . "✓")      ; U+2713 CHECK MARK (not ✔ which can emojify)
    (:cross          . "✗")      ; U+2717 BALLOT X
    (:warning        . "△")      ; U+25B3 WHITE UP-POINTING TRIANGLE
    (:info           . "ℹ")      ; U+2139 INFORMATION SOURCE
    (:ok             . "✓")
    (:error          . "✗")
    
    ;; Files - geometric
    (:folder         . "▪")      ; U+25AA BLACK SMALL SQUARE
    (:folder-open    . "▫")      ; U+25AB WHITE SMALL SQUARE  
    (:folder-archive . "◆")      ; U+25C6 BLACK DIAMOND
    (:file           . "─")
    
    ;; Help menu - safe decorative
    (:help-book      . "◆")      ; diamond for book
    (:help-target    . "◎")      ; U+25CE BULLSEYE
    (:help-rocket    . "▲")      ; triangle for rocket
    (:help-robot     . "◈")      ; U+25C8 
    (:help-palette   . "◐")      ; half circle for palette
    
    ;; System - varied geometric
    (:sys-info       . "👓︎")
    (:sys-process    . "◆")
    (:sys-memory     . "◐")
    (:sys-package    . "▪")
    (:sys-version    . "◇")
    (:sys-time       . "◷")      ; U+25F7 clock-like
    
    ;; Japa - ONLY geometric shapes, progression from empty to full
    (:japa-zero      . "○")      ; U+25CB WHITE CIRCLE (empty/new day)
    (:japa-progress  . "◐")      ; U+25D0 HALF (in progress)
    (:japa-complete  . "●")      ; U+25CF BLACK CIRCLE (complete)
    (:japa-bonus1    . "◉")      ; U+25C9 FISHEYE (bonus tier 1)
    (:japa-bonus2    . "◈")      ; U+25C8 (bonus tier 2)
    (:japa-bonus3    . "❖")      ; U+2756 BLACK DIAMOND MINUS WHITE X
    (:japa-epic      . "✦")      ; U+2726 BLACK FOUR POINTED STAR
    (:japa-today-l   . "")      ; U+27E8 
    (:japa-today-r   . "")      ; U+27E9
    (:japa-future    . "·")      ; U+00B7
    
    ;; Habits
    (:habit-done     . "✓")
    (:habit-today    . "▸")
    
    ;; Services  
    (:svc-ccl        . "◇")
    (:svc-sbcl       . "◆")
    (:svc-commercial . "▸")
    (:svc-smp        . "▶")
    
    ;; Misc
    (:arrow-right    . "→")
    (:arrow-left     . "←")
    (:star           . "✦")      ; safe 4-point star
    (:clock          . "◷")
    (:bell           . "♪")      ; musical note
    (:lightning      . "↯")      ; zigzag arrow
    (:dot            . "·")
    (:ellipsis       . "…")

    (:getting-started . "↯")
    (:active-projects . "✦")
    (:swank-services . "△")
    (:lisply-backends . "✦")

    )
  "Geometric Unicode - guaranteed never emojified.")


;; In skewed-icons.el

(defconst skewed-icons--colorful-table
  ;; Format: (:key . ("ICON_CHAR" . NEEDS_EXTRA_PADDING?))
  ;; If NEEDS_EXTRA_PADDING is t, we add 1 extra space to compensate for "Ghost" width.
  '((:bullet          . ("•" . nil))
    (:bullet-hollow   . ("◦" . nil))
    (:bullet-tri      . ("▸" . nil))
    (:bullet-dash     . ("─" . nil))

    (:check           . ("✔" . t))   ;; Heavy check often ghosts
    (:cross           . ("✘" . t))   ;; Heavy X often ghosts
    (:warning         . ("⚠" . t))   ;; Warning sign often ghosts
    (:info            . ("ℹ" . t))
    (:ok              . ("✔" . t))
    (:error           . ("✘" . t))

    (:folder          . ("📁" . nil)) ;; Usually honest 2-wide
    (:folder-open     . ("📂" . nil))
    (:folder-archive  . ("📦" . nil))
    (:file            . ("📄" . nil))

    (:help-book       . ("📖" . nil))
    (:help-target     . ("🎯" . nil))
    (:help-rocket     . ("🚀" . nil))
    (:help-robot      . ("🤖" . nil))
    (:help-palette    . ("🎨" . nil))

    (:sys-info        . ("🔎︎"  .  nil))
    (:sys-process     . ("🐂" . nil))    ;; The GEAR is a classic Ghost!
    (:sys-memory      . ("🧠" . nil))
    (:sys-package     . ("📦" . nil))
    (:sys-version     . ("🏷" . t))    ;; Label often ghosts
    (:sys-time        . ("🕒" . nil))

    ;; Japa
    (:japa-zero       . ("🌅" . nil))
    (:japa-progress   . ("📿" . t))
    (:japa-complete   . ("✅" . nil))
    (:japa-bonus1     . ("🏆" . t))
    (:japa-bonus2     . ("🥈" . t))
    (:japa-bonus3     . ("🥇" . t))
    (:japa-epic       . ("🛸" . t))
    (:japa-future     . ("❓" . t))

    (:habit-done      . ("✔" . t))
    (:habit-today     . ("▸" . nil))

    (:svc-ccl         . ("🖥" . t))    ;; Computer is a Ghost
    (:svc-sbcl        . ("🏭" . nil))    ;; Factory usually behaves
    (:svc-commercial  . ("✈" . t))     ;; Plane is a Ghost
    (:svc-smp         . ("🚀" . nil))  ;; Rocket usually behaves

    (:arrow-right     . ("→" . nil))
    (:arrow-left      . ("←" . nil))
    (:star            . ("⭐" . nil))
    (:clock           . ("🕒" . nil))
    (:bell            . ("🔔" . nil))
    (:lightning       . ("" . t))     ;; Bolt often ghosts
    (:dot             . ("·" . nil))
    (:ellipsis        . ("…" . nil))


    (:getting-started . "⚡️")
    (:active-projects . "🤸🏼‍♂️")
    (:swank-services . "🍸")
    (:lisply-backends . "λ")

    ))


(defvar skewed-icons--nerd-table nil)

(defun skewed-icons--init-nerd-table ()
  "Initialize nerd font icons if available."
  (when (and (null skewed-icons--nerd-table)
             (require 'nerd-icons nil t))
    (condition-case nil
        (setq skewed-icons--nerd-table
              `((:bullet . ,(nerd-icons-faicon "nf-fa-angle_right"))
                (:bullet-hollow . ,(nerd-icons-faicon "nf-fa-angle_right"))
                (:bullet-tri . ,(nerd-icons-faicon "nf-fa-caret_right"))
                (:bullet-dash . ,(nerd-icons-faicon "nf-fa-minus"))
                (:check . ,(nerd-icons-faicon "nf-fa-check"))
                (:cross . ,(nerd-icons-faicon "nf-fa-times"))
                (:warning . ,(nerd-icons-faicon "nf-fa-warning"))
                (:info . ,(nerd-icons-faicon "nf-fa-info"))
                (:ok . ,(nerd-icons-faicon "nf-fa-check"))
                (:error . ,(nerd-icons-faicon "nf-fa-times"))
                (:folder . ,(nerd-icons-faicon "nf-fa-folder"))
                (:folder-open . ,(nerd-icons-faicon "nf-fa-folder_open"))
                (:folder-archive . ,(nerd-icons-faicon "nf-fa-archive"))
                (:file . ,(nerd-icons-faicon "nf-fa-file_o"))
                (:help-book . ,(nerd-icons-faicon "nf-fa-book"))
                (:help-target . ,(nerd-icons-faicon "nf-fa-bullseye"))
                (:help-rocket . ,(nerd-icons-faicon "nf-fa-rocket"))
                (:help-robot . ,(nerd-icons-codicon "nf-cod-hubot"))
                (:help-palette . ,(nerd-icons-faicon "nf-fa-paint_brush"))
                (:sys-process . ,(nerd-icons-devicon "nf-dev-gnu"))
                (:sys-memory . ,(nerd-icons-faicon "nf-fa-microchip"))
                (:sys-package . ,(nerd-icons-faicon "nf-fa-cube"))
                (:sys-version . ,(nerd-icons-faicon "nf-fa-info_circle"))
                (:sys-time . ,(nerd-icons-faicon "nf-fa-clock_o"))
                (:japa-zero . ,(nerd-icons-faicon "nf-fa-sun_o"))
                (:japa-progress . ,(nerd-icons-faicon "nf-fa-adjust"))
                (:japa-complete . ,(nerd-icons-faicon "nf-fa-check_circle"))
                (:japa-bonus1 . ,(nerd-icons-faicon "nf-fa-fire"))
                (:japa-bonus2 . ,(nerd-icons-faicon "nf-fa-star"))
                (:japa-bonus3 . ,(nerd-icons-faicon "nf-fa-trophy"))
                (:japa-epic . ,(nerd-icons-faicon "nf-fa-heart"))
                (:japa-today-l . "")
                (:japa-today-r . "")
                (:japa-future . "·")
                (:habit-done . ,(nerd-icons-faicon "nf-fa-check"))
                (:habit-today . ,(nerd-icons-faicon "nf-fa-calendar"))
                (:svc-ccl . ,(nerd-icons-faicon "nf-fa-desktop"))
                (:svc-sbcl . ,(nerd-icons-faicon "nf-fa-industry"))
                (:svc-commercial . ,(nerd-icons-faicon "nf-fa-plane"))
                (:svc-smp . ,(nerd-icons-faicon "nf-fa-rocket"))
                (:arrow-right . ,(nerd-icons-faicon "nf-fa-arrow_right"))
                (:arrow-left . ,(nerd-icons-faicon "nf-fa-arrow_left"))
                (:star . ,(nerd-icons-faicon "nf-fa-star"))
                (:clock . ,(nerd-icons-faicon "nf-fa-clock_o"))
                (:bell . ,(nerd-icons-faicon "nf-fa-bell"))
                (:lightning . ,(nerd-icons-faicon "nf-fa-bolt"))
                (:dot . "·")
                (:ellipsis . "…")))
      (error nil))))

;;; Public API =================================================================

(defun skewed-icons--get-current-table ()
  "Return the active icon table based on `skewed-icons-style`."
  (pcase skewed-icons-style
    ('ascii skewed-icons--ascii-table)
    ('unicode skewed-icons--unicode-table)
    ('colorful skewed-icons--colorful-table)
    ('nerd (progn
             (skewed-icons--init-nerd-table)
             (or skewed-icons--nerd-table
                 skewed-icons--unicode-table)))
    (_ skewed-icons--colorful-table)))

(defun skewed-icon (name)
  "Get icon string for NAME using current style.
Handles both simple string values and (STRING . GHOST) cons cells."
  (let* ((table (skewed-icons--get-current-table))
         (entry (alist-get name table)))
    (cond
     ((stringp entry) entry)          ;; Simple string (ASCII/Unicode tables)
     ((consp entry) (car entry))      ;; Cons cell (Colorful table)
     (t "?"))))

(defun skewed-icon-is-ghost (name)
  "Return t if the current icon is a 'ghost' needing extra padding.
Only returns t if style is 'colorful' and the icon is flagged."
  (if (eq skewed-icons-style 'colorful)
      (let* ((table skewed-icons--colorful-table)
             (entry (alist-get name table)))
        (if (consp entry) (cdr entry) nil))
    nil)) ;; ASCII/Unicode/Nerd never ghost


(defun skewed-icons-set-style (style)
  "Set icon style interactively."
  (interactive
   (list (intern (completing-read "Icon style: "
                                  '("ascii" "unicode" "colorful" "nerd")
                                  nil t))))
  (setq skewed-icons-style style)
  
  ;; THE CRITICAL FIX: If using colorful, enforce width=2
  (when (eq style 'colorful)
    (skewed-icons--fix-widths))

  (message "Icon style: %s" style)
  (when (and (fboundp 'dashboard-refresh-buffer)
             (get-buffer "*dashboard*"))
    (dashboard-refresh-buffer)))

;; Run fix automatically if style is already set to colorful at load time
(when (eq skewed-icons-style 'colorful)
  (skewed-icons--fix-widths))




(provide 'skewed-icons)
;;; skewed-icons.el ends here




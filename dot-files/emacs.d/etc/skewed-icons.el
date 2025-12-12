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

(defcustom skewed-icons-style 'unicode
  "Icon style to use.
- `ascii': Pure ASCII, universal
- `unicode': Safe geometric Unicode (recommended)
- `unicode-fancy': Richer Unicode with VS15 (may vary by terminal)
- `nerd': Nerd Font icons"
  :type '(choice (const :tag "ASCII" ascii)
                 (const :tag "Unicode safe (recommended)" unicode)
                 (const :tag "Unicode fancy (VS15)" unicode-fancy)
                 (const :tag "Nerd Fonts" nerd))
  :group 'skewed-icons)

;;; Icon Tables ================================================================

(defconst skewed-icons--ascii-table
  '((:bullet . "*") (:bullet-hollow . "o") (:bullet-tri . ">") (:bullet-dash . "-")
    (:check . "+") (:cross . "x") (:warning . "!") (:info . "i") (:ok . "+") (:error . "x")
    (:folder . ">") (:folder-open . "v") (:folder-archive . "#") (:file . "-")
    (:help-book . "?") (:help-target . ">") (:help-rocket . "!") (:help-robot . "@") (:help-palette . "~")
    (:sys-process . "*") (:sys-memory . "*") (:sys-package . "*") (:sys-version . "*") (:sys-time . "*")
    (:japa-zero . ".") (:japa-progress . "o") (:japa-complete . "+")
    (:japa-bonus1 . "*") (:japa-bonus2 . "#") (:japa-bonus3 . "@") (:japa-epic . "!")
    (:japa-today-l . "[") (:japa-today-r . "]") (:japa-future . ".")
    (:habit-done . "+") (:habit-today . "#")
    (:svc-ccl . "C") (:svc-sbcl . "S") (:svc-commercial . "$") (:svc-smp . "&")
    (:arrow-right . ">") (:arrow-left . "<") (:star . "*") (:clock . "@")
    (:bell . "*") (:lightning . "!") (:dot . ".") (:ellipsis . "..."))
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
    (:japa-today-l   . "⟨")      ; U+27E8 
    (:japa-today-r   . "⟩")      ; U+27E9
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
    (:ellipsis       . "…"))
  "Geometric Unicode - guaranteed never emojified.")

;; Fancy unicode with colorful characters + VS15 to force text presentation
(defconst skewed-icons--unicode-fancy-table
  (let ((vs15 "\uFE0E"))  ; Variation Selector 15 = text presentation
    `((:bullet         . "•")
      (:bullet-hollow  . "◦")
      (:bullet-tri     . "▸")
      (:bullet-dash    . "─")
      
      (:check          . ,(concat "✔" vs15))
      (:cross          . ,(concat "✘" vs15))
      (:warning        . ,(concat "⚠" vs15))
      (:info           . "ℹ")
      (:ok             . ,(concat "✔" vs15))
      (:error          . ,(concat "✘" vs15))
      
      (:folder         . "▪")
      (:folder-open    . "▫")  
      (:folder-archive . "◆")
      (:file           . "─")
      
      (:help-book      . ,(concat "❧" vs15))
      (:help-target    . "◎")
      (:help-rocket    . "▲")
      (:help-robot     . ,(concat "⚙" vs15))
      (:help-palette   . ,(concat "✿" vs15))
      
      (:sys-process    . ,(concat "⚙" vs15))
      (:sys-memory     . "◐")
      (:sys-package    . "◆")
      (:sys-version    . "◇")
      (:sys-time       . "◷")
      
      ;; Japa - colorful with VS15
      (:japa-zero      . ,(concat "☀" vs15))   ; sun
      (:japa-progress  . "◐")
      (:japa-complete  . "●")
      (:japa-bonus1    . "◉")
      (:japa-bonus2    . ,(concat "★" vs15))   ; star
      (:japa-bonus3    . "✦")
      (:japa-epic      . ,(concat "❤" vs15))   ; heart
      (:japa-today-l   . "⟨")
      (:japa-today-r   . "⟩")
      (:japa-future    . "·")
      
      (:habit-done     . ,(concat "✔" vs15))
      (:habit-today    . "▸")
      
      (:svc-ccl        . "◇")
      (:svc-sbcl       . "◆")
      (:svc-commercial . "▸")
      (:svc-smp        . "▶")
      
      (:arrow-right    . "→")
      (:arrow-left     . "←")
      (:star           . ,(concat "★" vs15))
      (:clock          . "◷")
      (:bell           . ,(concat "♪" vs15))
      (:lightning      . "↯")
      (:dot            . "·")
      (:ellipsis       . "…")))
  "Fancy Unicode with VS15 for text presentation.")

;; Nerd font table - populated lazily
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
                (:japa-today-l . "[")
                (:japa-today-r . "]")
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

(defun skewed-icon (name)
  "Get icon for NAME using current style."
  (let ((table (pcase skewed-icons-style
                 ('ascii skewed-icons--ascii-table)
                 ('unicode skewed-icons--unicode-table)
                 ('unicode-fancy skewed-icons--unicode-fancy-table)
                 ('nerd (progn
                          (skewed-icons--init-nerd-table)
                          (or skewed-icons--nerd-table
                              skewed-icons--unicode-table)))
                 (_ skewed-icons--unicode-table))))
    (or (alist-get name table)
        (alist-get name skewed-icons--ascii-table)
        "?")))

(defun skewed-icons-set-style (style)
  "Set icon style interactively."
  (interactive
   (list (intern (completing-read "Icon style: "
                                  '("ascii" "unicode" "unicode-fancy" "nerd")
                                  nil t))))
  (setq skewed-icons-style style)
  (message "Icon style: %s" style)
  (when (and (fboundp 'dashboard-refresh-buffer)
             (get-buffer "*dashboard*"))
    (dashboard-refresh-buffer)))

(defun skewed-icons-preview ()
  "Preview all icons in current style."
  (interactive)
  (let ((buf (get-buffer-create "*skewed-icons*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Style: %s\n\n" skewed-icons-style))
        (insert "Japa Graph Preview:\n  ")
        (insert (skewed-icon :japa-zero) " "
                (skewed-icon :japa-progress) " "
                (skewed-icon :japa-complete) " "
                (skewed-icon :japa-today-l)
                (skewed-icon :japa-bonus1)
                (skewed-icon :japa-today-r) " "
                (skewed-icon :japa-bonus2) " "
                (skewed-icon :japa-bonus3) " "
                (skewed-icon :japa-epic) "\n\n")
        (dolist (group '(("Status" :check :cross :ok :error :warning :info)
                         ("Japa" :japa-zero :japa-progress :japa-complete
                                  :japa-bonus1 :japa-bonus2 :japa-bonus3 :japa-epic)
                         ("Services" :svc-ccl :svc-sbcl :svc-commercial :svc-smp)))
          (insert (format "%s:\n" (car group)))
          (dolist (name (cdr group))
            (insert (format "  %s %s\n" (skewed-icon name) name)))
          (insert "\n"))))
    (display-buffer buf)))

(provide 'skewed-icons)
;;; skewed-icons.el ends here

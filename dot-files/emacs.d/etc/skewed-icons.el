;;; skewed-icons.el --- Terminal-safe icon system -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Centralized icon system for skewed-emacs that works on ANY terminal.
;;
;; Design principles:
;; 1. Default (safe) mode uses only universally-supported single-width characters
;; 2. Higher modes are opt-in for users who know their terminal supports them
;; 3. All icons are accessed via semantic names, not hardcoded characters
;;
;; Safe characters come from these Unicode blocks:
;; - ASCII (U+0000-007F) - universal
;; - Latin-1 Supplement (U+0080-00FF) - universal  
;; - General Punctuation (U+2000-206F) - very safe
;; - Arrows (U+2190-21FF) - safe
;; - Box Drawing (U+2500-257F) - designed for terminals
;; - Geometric Shapes (U+25A0-25FF) - mostly safe
;;
;; Usage:
;;   (skewed-icon :bullet)
;;   (skewed-icon :check)
;;   (skewed-icon :folder-open)
;;
;; Configuration:
;;   M-x skewed-icons-set-style RET safe|unicode|nerd|emoji

;;; Code:

(defgroup skewed-icons nil
  "Icon configuration for skewed-emacs."
  :group 'skewed-emacs
  :prefix "skewed-icons-")

(defcustom skewed-icons-style 'unicode
  "Icon style to use.
- `safe': Universal ASCII/basic Unicode (default)
- `unicode': Fancier Unicode symbols  
- `nerd': Nerd Font icons (requires font in terminal)
- `emoji': Full emoji (may cause width issues)"
  :type '(choice (const :tag "Safe (universal)" safe)
                 (const :tag "Unicode symbols" unicode)
                 (const :tag "Nerd Fonts" nerd)
                 (const :tag "Emoji" emoji))
  :group 'skewed-icons)

;;; Icon Tables

(defconst skewed-icons--safe-table
  '(;; Generic bullets/markers
    (:bullet         . "*")
    (:bullet-hollow  . "o")
    (:bullet-tri     . ">")
    (:bullet-dash    . "-")
    
    ;; Status indicators
    (:check          . "+")
    (:cross          . "x")
    (:warning        . "!")
    (:info           . "i")
    (:ok             . "+")
    (:error          . "x")
    
    ;; File/folder indicators  
    (:folder         . ">")
    (:folder-open    . "v")
    (:folder-archive . "#")
    (:file           . "-")
    
    ;; Help menu items (distinct markers)
    (:help-book      . "?")
    (:help-target    . ">")
    (:help-rocket    . "!")
    (:help-robot     . "@")
    (:help-palette   . "~")
    
    ;; System info bullets
    (:sys-process    . "*")
    (:sys-memory     . "*")
    (:sys-package    . "*")
    (:sys-version    . "*")
    (:sys-time       . "*")
    
    ;; Progress/achievement (japa graph)
    (:japa-zero      . ".")
    (:japa-progress  . "o")
    (:japa-complete  . "+")
    (:japa-bonus1    . "*")
    (:japa-bonus2    . "#")
    (:japa-bonus3    . "@")
    (:japa-epic      . "!")
    (:japa-today-l   . "[")
    (:japa-today-r   . "]")
    (:japa-future    . ".")
    
    ;; Habit indicators
    (:habit-done     . "+")
    (:habit-today    . "#")
    
    ;; Service type indicators
    (:svc-ccl        . "C")
    (:svc-sbcl       . "S")
    (:svc-commercial . "$")
    (:svc-smp        . "&")
    
    ;; Misc
    (:arrow-right    . ">")
    (:arrow-left     . "<")
    (:star           . "*")
    (:clock          . "@")
    (:bell           . "*")
    (:lightning      . "!")
    (:dot            . ".")
    (:ellipsis       . "..."))
  "Baseline safe icons - ASCII, works everywhere.")

(defconst skewed-icons--unicode-table
  '(;; Generic bullets/markers
    (:bullet         . "•")
    (:bullet-hollow  . "◦")
    (:bullet-tri     . "▸")
    (:bullet-dash    . "─")
    
    ;; Status indicators
    (:check          . "✓")
    (:cross          . "✗")
    (:warning        . "⚠")
    (:info           . "ℹ")
    (:ok             . "✓")
    (:error          . "✗")
    
    ;; File/folder indicators
    (:folder         . "▪")
    (:folder-open    . "▫")
    (:folder-archive . "◆")
    (:file           . "─")
    
    ;; Help menu items
    (:help-book      . "◆")
    (:help-target    . "▸")
    (:help-rocket    . "◇")
    (:help-robot     . "◈")
    (:help-palette   . "◐")
    
    ;; System info bullets
    (:sys-process    . "▪")
    (:sys-memory     . "▫")
    (:sys-package    . "▪")
    (:sys-version    . "▫")
    (:sys-time       . "▪")
    
    ;; Progress/achievement (japa graph)
    (:japa-zero      . "○")
    (:japa-progress  . "◐")
    (:japa-complete  . "●")
    (:japa-bonus1    . "◉")
    (:japa-bonus2    . "★")
    (:japa-bonus3    . "✦")
    (:japa-epic      . "✧")
    (:japa-today-l   . "⟨")
    (:japa-today-r   . "⟩")
    (:japa-future    . "·")
    
    ;; Habit indicators
    (:habit-done     . "✓")
    (:habit-today    . "▸")
    
    ;; Service type indicators
    (:svc-ccl        . "◇")
    (:svc-sbcl       . "◆")
    (:svc-commercial . "▸")
    (:svc-smp        . "▶")
    
    ;; Misc
    (:arrow-right    . "→")
    (:arrow-left     . "←")
    (:star           . "★")
    (:clock          . "◷")
    (:bell           . "◆")
    (:lightning      . "↯")
    (:dot            . "·")
    (:ellipsis       . "…"))
  "Unicode symbols - fancier but still mostly safe.")

(defconst skewed-icons--emoji-table
  '(;; Generic bullets/markers
    (:bullet         . "•")
    (:bullet-hollow  . "◦")
    (:bullet-tri     . "▸")
    (:bullet-dash    . "─")
    
    ;; Status indicators
    (:check          . "✅")
    (:cross          . "❌")
    (:warning        . "⚠️")
    (:info           . "ℹ️")
    (:ok             . "✅")
    (:error          . "❌")
    
    ;; File/folder indicators
    (:folder         . "📁")
    (:folder-open    . "📂")
    (:folder-archive . "🗃️")
    (:file           . "📄")
    
    ;; Help menu items  
    (:help-book      . "📖")
    (:help-target    . "🎯")
    (:help-rocket    . "🚀")
    (:help-robot     . "🤖")
    (:help-palette   . "🎨")
    
    ;; System info bullets
    (:sys-process    . "🐃")
    (:sys-memory     . "🧠")
    (:sys-package    . "📦")
    (:sys-version    . "🎪")
    (:sys-time       . "🕐")
    
    ;; Progress/achievement (japa graph)
    (:japa-zero      . "☀️")
    (:japa-progress  . "📿")
    (:japa-complete  . "✅")
    (:japa-bonus1    . "🔥")
    (:japa-bonus2    . "💪")
    (:japa-bonus3    . "🏆")
    (:japa-epic      . "🚀")
    (:japa-today-l   . "⟦")
    (:japa-today-r   . "⟧")
    (:japa-future    . "·")
    
    ;; Habit indicators
    (:habit-done     . "✓")
    (:habit-today    . "🧹")
    
    ;; Service type indicators
    (:svc-ccl        . "🖥️")
    (:svc-sbcl       . "🏭")
    (:svc-commercial . "✈️")
    (:svc-smp        . "🚀")
    
    ;; Misc
    (:arrow-right    . "→")
    (:arrow-left     . "←")
    (:star           . "⭐")
    (:clock          . "🕐")
    (:bell           . "🔔")
    (:lightning      . "⚡")
    (:dot            . "·")
    (:ellipsis       . "…"))
  "Full emoji - may cause terminal width issues.")

;; Nerd font table - populated lazily
(defvar skewed-icons--nerd-table nil)

(defun skewed-icons--init-nerd-table ()
  "Initialize nerd font icon table if nerd-icons is available."
  (when (and (null skewed-icons--nerd-table)
             (require 'nerd-icons nil t))
    (condition-case nil
        (setq skewed-icons--nerd-table
              `(;; Generic bullets
                (:bullet         . ,(nerd-icons-faicon "nf-fa-angle_right"))
                (:bullet-hollow  . ,(nerd-icons-faicon "nf-fa-angle_right"))
                (:bullet-tri     . ,(nerd-icons-faicon "nf-fa-caret_right"))
                (:bullet-dash    . ,(nerd-icons-faicon "nf-fa-minus"))
                
                ;; Status
                (:check          . ,(nerd-icons-faicon "nf-fa-check"))
                (:cross          . ,(nerd-icons-faicon "nf-fa-times"))
                (:warning        . ,(nerd-icons-faicon "nf-fa-warning"))
                (:info           . ,(nerd-icons-faicon "nf-fa-info"))
                (:ok             . ,(nerd-icons-faicon "nf-fa-check"))
                (:error          . ,(nerd-icons-faicon "nf-fa-times"))
                
                ;; Folders
                (:folder         . ,(nerd-icons-faicon "nf-fa-folder"))
                (:folder-open    . ,(nerd-icons-faicon "nf-fa-folder_open"))
                (:folder-archive . ,(nerd-icons-faicon "nf-fa-archive"))
                (:file           . ,(nerd-icons-faicon "nf-fa-file_o"))
                
                ;; Help menu
                (:help-book      . ,(nerd-icons-faicon "nf-fa-book"))
                (:help-target    . ,(nerd-icons-faicon "nf-fa-bullseye"))
                (:help-rocket    . ,(nerd-icons-faicon "nf-fa-rocket"))
                (:help-robot     . ,(nerd-icons-codicon "nf-cod-hubot"))
                (:help-palette   . ,(nerd-icons-faicon "nf-fa-paint_brush"))
                
                ;; System info
                (:sys-process    . ,(nerd-icons-devicon "nf-dev-gnu"))
                (:sys-memory     . ,(nerd-icons-faicon "nf-fa-microchip"))
                (:sys-package    . ,(nerd-icons-faicon "nf-fa-cube"))
                (:sys-version    . ,(nerd-icons-faicon "nf-fa-info_circle"))
                (:sys-time       . ,(nerd-icons-faicon "nf-fa-clock_o"))
                
                ;; Japa
                (:japa-zero      . ,(nerd-icons-faicon "nf-fa-circle_o"))
                (:japa-progress  . ,(nerd-icons-faicon "nf-fa-adjust"))
                (:japa-complete  . ,(nerd-icons-faicon "nf-fa-check_circle"))
                (:japa-bonus1    . ,(nerd-icons-faicon "nf-fa-fire"))
                (:japa-bonus2    . ,(nerd-icons-faicon "nf-fa-star"))
                (:japa-bonus3    . ,(nerd-icons-faicon "nf-fa-trophy"))
                (:japa-epic      . ,(nerd-icons-faicon "nf-fa-rocket"))
                (:japa-today-l   . "[")
                (:japa-today-r   . "]")
                (:japa-future    . "·")
                
                ;; Habit
                (:habit-done     . ,(nerd-icons-faicon "nf-fa-check"))
                (:habit-today    . ,(nerd-icons-faicon "nf-fa-calendar"))
                
                ;; Services
                (:svc-ccl        . ,(nerd-icons-faicon "nf-fa-desktop"))
                (:svc-sbcl       . ,(nerd-icons-faicon "nf-fa-industry"))
                (:svc-commercial . ,(nerd-icons-faicon "nf-fa-plane"))
                (:svc-smp        . ,(nerd-icons-faicon "nf-fa-rocket"))
                
                ;; Misc
                (:arrow-right    . ,(nerd-icons-faicon "nf-fa-arrow_right"))
                (:arrow-left     . ,(nerd-icons-faicon "nf-fa-arrow_left"))
                (:star           . ,(nerd-icons-faicon "nf-fa-star"))
                (:clock          . ,(nerd-icons-faicon "nf-fa-clock_o"))
                (:bell           . ,(nerd-icons-faicon "nf-fa-bell"))
                (:lightning      . ,(nerd-icons-faicon "nf-fa-bolt"))
                (:dot            . "·")
                (:ellipsis       . "…")))
      (error nil))))

;;; Public API

(defun skewed-icon (name)
  "Get icon string for NAME using current style.
NAME is a keyword like :bullet, :check, :folder, etc."
  (let ((table (pcase skewed-icons-style
                 ('safe skewed-icons--safe-table)
                 ('unicode skewed-icons--unicode-table)
                 ('emoji skewed-icons--emoji-table)
                 ('nerd (progn
                          (skewed-icons--init-nerd-table)
                          (or skewed-icons--nerd-table
                              skewed-icons--unicode-table)))
                 (_ skewed-icons--safe-table))))
    (or (alist-get name table)
        (alist-get name skewed-icons--safe-table)
        "?")))

(defun skewed-icons-set-style (style)
  "Set icon style interactively."
  (interactive
   (list (intern (completing-read "Icon style: "
                                  '("safe" "unicode" "nerd" "emoji")
                                  nil t))))
  (setq skewed-icons-style style)
  (message "Icon style: %s" style)
  (when (and (fboundp 'dashboard-refresh-buffer)
             (get-buffer "*dashboard*"))
    (dashboard-refresh-buffer)))

(defun skewed-icons-preview ()
  "Show preview of all icons in current style."
  (interactive)
  (with-current-buffer (get-buffer-create "*skewed-icons*")
    (erase-buffer)
    (insert (format "Icon Style: %s\n\n" skewed-icons-style))
    (dolist (group '(("Bullets" :bullet :bullet-hollow :bullet-tri)
                     ("Status" :check :cross :ok :error :warning :info)
                     ("Folders" :folder :folder-open :folder-archive)
                     ("Help Menu" :help-book :help-target :help-rocket :help-robot :help-palette)
                     ("System" :sys-process :sys-memory :sys-package :sys-version :sys-time)
                     ("Japa" :japa-zero :japa-progress :japa-complete :japa-bonus1 :japa-bonus2 :japa-bonus3 :japa-epic)
                     ("Services" :svc-ccl :svc-sbcl :svc-commercial :svc-smp)))
      (insert (format "%s:\n" (car group)))
      (dolist (name (cdr group))
        (insert (format "  %s %s\n" (skewed-icon name) name)))
      (insert "\n"))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(provide 'skewed-icons)
;;; skewed-icons.el ends here

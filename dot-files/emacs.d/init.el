;;; package --- Summary: init.el 
;;; Commentary:
;;; This is my personal Emacs configuration.
;;; It was inspired by but is no longer based on Crafted Emacs.
;;;
;;; Code:

(require 'cl-lib)

(defvar emacs-config-directory (file-name-directory (file-truename load-file-name)))
(unless (and (getenv "SKEWED_EMACS_CONTAINER") (not (getenv "EMACS_BATCH_MODE")))
  (load (concat emacs-config-directory "etc/load-and-compile.el")))
;;
;; FLAG -- Now make sure all background compilings have been forced and completed
;;         at this juncture.
;;


(defvar load-lisply? t)

(defvar light-theme-options
  '(("adwaita" . adwaita)
    ("tsdh-light" . tsdh-light)
    ("light-blue" . light-blue)
    ("doom-one-light" . doom-one-light)
    ("doom-feather-light" . doom-feather-light)
    ("doom-light" . doom-gruvbox-light))
  "List of light themes for `light-theme' function.")

(defvar dark-theme-options
  '(("doom-tokyo-night" . doom-tokyo-night)
    ("doom-one" . doom-one)
    ("doom-pine" . doom-pine)
    ("doom-purple" . doom-shades-of-purple)
    ("doom-gruvbox" . doom-gruvbox)
    ("modus-vivendi" . modus-vivendi-deuteranopia)
    ("zenburn" . zenburn))
  "List of dark themes for `dark-theme' function.")

(defvar my-files-to-load `("slime"
			   "org"
			   "magit"
			   "copilot"
			   ;;"yaml-mode" ;; caused lisp_eval crashes
			   ;;"impatient-markdown"
     			   ;;"dashboard"
			   ))


(defun load-one-config (file directory)
  "Load a single configuration file FILE from DIRECTORY."
  (let ((pwd (or directory (concat emacs-config-directory "/etc/"))))
    (let ((full-path (concat pwd file ".el")))
      (message "About to attempt load-file on %s..." full-path)
      (if (file-exists-p full-path) (load-file full-path)
        (warn "%s seems to be missing, cannot load-file on it." full-path)))))

;;
;; FLAG -- force native-compile all the things referenced below. 
;;
(defun main-setup ()
  "Load custom config files from ./etc."
  (dolist (filespec my-files-to-load)
    (if (listp filespec)
	(load-one-config (first filespec) (second filespec)) 
      (load-one-config filespec nil)))
    ;;
    ;; FLAG -- sort out load-gdl
    ;;
    ;;(load-gdl) ;; maybe now needed now? - just slime-connect to container
  ;;


  (add-hook 'before-make-frame-hook 'on-before-make-frame)
  (add-hook 'after-make-frame-functions 'on-after-make-frame)
  
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

  ;; Load Lisply MCP service if enabled still using manual load, this
  ;; is not quite packaged yet as a proper emacs package,
  ;; it's just built-in to skewed-emacs's config thusly:
  (when load-lisply?
    (let ((lisply-dir (concat emacs-config-directory "/sideloaded/lisply-backend/source/")))
      (when (file-exists-p lisply-dir)
        (message "Loading Lisply service from %s" lisply-dir)
        (dolist (file '("http-setup.el" "endpoints.el"))
          (let ((file-path (concat lisply-dir file)))
            (if (file-exists-p file-path)
                (load-file file-path)
              (message "Warning: MCP service file %s not found" file-path))))))
   (setq httpd-host "0.0.0.0")
   (emacs-lisply-start-server))

    
  (message "Done with main-setup."))
    


(defun set-default-settings ()
  "Set my personal preferred default settings."
  (interactive)
  ;;
  ;; enable this for tricky undo situations
  ;;
  ;; (global-undo-tree-mode)
  ;;
  (if (display-graphic-p)
      (setup-graphical-keybindings-and-faces)
    (setup-terminal-keybindings-and-faces))
  
  (setup-other-keybindings-and-faces)

  ;; Font rendering improvements for better PDF and general text display
  (when (display-graphic-p)
    ;; Better font rendering settings
    (setq-default
     font-use-system-font t
     inhibit-compacting-font-caches t
     line-spacing 0.1)
    
    (when (package-installed-p 'pdf-tools)
      (setq pdf-view-display-size 'fit-page
            pdf-view-resize-factor 1.1
            pdf-view-use-scaling t
            pdf-view-use-imagemagick nil)) ; Use poppler for better quality
    
    ;; Improve doc-view as fallback for PDF rendering
    (with-eval-after-load 'doc-view
      (setq doc-view-resolution 200 ; Increase from default 100
            doc-view-ghostscript-options
            '("-dNOPAUSE" "-sDEVICE=png16m" "-dTextAlphaBits=4" 
              "-dBATCH" "-dSAFER" "-dQUIET" "-dGraphicsAlphaBits=4")))
    
    ;; Smooth scrolling improvements
    (setq scroll-step 1
          scroll-conservatively 10000
          scroll-preserve-screen-position 1))

  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8)
  (set-clipboard-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  ;;(setq line-number-mode t) ;; FLAG only do this for a select few
  (setq column-number-mode t)
  (global-font-lock-mode t)
  (setq-default transient-mark-mode t)
  (show-paren-mode t)
  (setq scroll-step 1)
  (setup-input-methods)
  (setq confirm-kill-processes nil)

  (menu-bar-mode 0)
  (tool-bar-mode 0)

)



(defun enable-company-mode ()
  "Enable company mode."
  (interactive)
  (company-mode 1))

(defun disable-company-mode ()
  "Enable company mode."
  (interactive)
  (company-mode 0))

(defun disable-line-number-mode ()
  "Disable it dammit."
  (interactive)
  (line-number-mode -1))


(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;;
;; C-x & should Cycle through possible *slime-repl...*  buffers.
;;

(defun cycle-slime-repl-buffers ()
  "Cycle through possible *slime-repl...* buffers."
  (interactive)
  (let ((buffers (buffer-list)))
    (dolist (buffer buffers)
      (if (string-match-p "\\*slime-repl.*\\*" (buffer-name buffer))
	  (switch-to-buffer buffer)))))

;;
;; Get various s-expression keychords working in terminal (iTerm2 at least)
;;
;; tips on setup were found here:
;;
;;  https://emacs.stackexchange.com/questions/44898/how-to-map-c-m-left-c-m-right-etc-to-the-correct-hex-codes-in-iterm2
;; and https://emacswiki.org/emacs/iTerm2
;;

(defun setup-terminal-keybindings-and-faces ()
  "Set up bindings for terminal.
FLAG: make sure these don't clobber graphical mode bindings,
      if so, then we need to make this conditional based
      on  (display-graphic-p)."
  (interactive)
  (message "About to set up keybindings for terminal use.")
  (and (boundp 'input-decode-map) (message "At this point input-decode-map is bound."))
  (dolist (map (list input-decode-map function-key-map))
    (define-key map "[113;7u" (kbd "C-M-q"))
    (define-key map "[102;7u" (kbd "C-M-f"))
    (define-key map "[98;7u" (kbd "C-M-b"))
    (define-key map "[107;7u" (kbd "C-M-k")))
  (global-set-key (kbd "ESC C-M-f") 'forward-sexp)
  (global-set-key (kbd "ESC C-M-b") 'backward-sexp)
  (global-set-key (kbd "ESC C-M-k") 'kill-sexp)
  (global-set-key (kbd "ESC C-M-q") 'indent-sexp)

  ;; (global-set-key (kbd "C-c C-e") 'chatgpt-shell-prompt-compose)
  
  (message "Done with keybindings setup."))


(defun setup-graphical-keybindings-and-faces ()
  "Set up keybindings and faces for graphical mode."

  ;;(doom-modeline-mode 1)
  
  (let ((scale-factor (if (> (x-display-pixel-width) 1920) 1.5 1.0)))
    
    (set-face-attribute 'default nil
			;;:family "Source Code Pro"
			:height (round (* 110 scale-factor))
			:weight 'normal
			:width 'normal)))


(defun setup-other-keybindings-and-faces ()
  "Set up bindings for graphical mode."
  (interactive)

  (global-set-key (kbd "\C-x y") 'previous-window-any-frame)
  (global-set-key (kbd "\C-c 8") "•")
  (global-set-key "\M-=" 'just-one-space)
  ;; Modified keyboard shortcuts
  (global-set-key "\C-x\C-b" 'electric-buffer-list)
  (global-set-key (kbd "C-x &") 'cycle-slime-repl-buffers)
  (global-set-key (kbd "C-c M-q") 'unfill-paragraph)
  (global-set-key (kbd "C-c x") 'server-shutdown))


;;
;; from emacswiki.org/emacs/iTerm2
;;
;; FLAG -- maybe disable this if it messes with copy/paste from terminal.
;; FLAG -- maybe conditionalize this out for (display-graphic-p).
;;
(defun enable-mouse ()
  "Enable mouse interaction in terminal mode."
  (interactive)
  (unless window-system
    (require 'mouse)
    (xterm-mouse-mode t)
    (defun track-mouse (e))
    (setq mouse-sel-mode t)))

(defun disable-mouse ()
  "Disable mouse interaction in terminal mode."
  (interactive)
  (xterm-mouse-mode -1))

(defun clear-themes ()
  "Clear all themes."
  (interactive)
  (dolist (theme custom-enabled-themes) (disable-theme theme)))
(defun set-cursor-bright ()
  "Set cursor to a bright magenta color."
  (interactive)
  (when (not (display-graphic-p))
    (send-string-to-terminal "\e]12;rgb:ff/00/ff\a"))  ; Magenta
  (set-cursor-color "#ff00ff"))  ; Fallback for consistency




(defun dark-theme (&optional theme-name)
  "Load a dark theme, defaulting to \='doom-tokyo-night\='.
THEME-NAME is a string, e.g., \='doom-tokyo-night\='."
  (interactive
   (list (completing-read "Select dark theme: "
                          (mapcar #'car dark-theme-options)  ; Use defvar directly
                          nil t "doom-tokyo-night")))
  (let* ((selected-theme (or theme-name "doom-tokyo-night"))  ; Default if nil
         (theme-symbol (cdr (assoc selected-theme dark-theme-options))))
    (unless theme-symbol
      (error "Unknown theme: %s" selected-theme))
    (clear-themes)
    (load-theme theme-symbol t)
    (when (not (display-graphic-p))
      (send-string-to-terminal "\e]12;rgb:ff/00/ff\a"))  ; Magenta cursor
    (set-cursor-color "#ff00ff")  ; Fallback
    (setq cursor-type 'bar)
    (message "Loaded dark theme: %s" selected-theme)))



(defun light-theme (&optional theme-name)
  "Load a light theme, defaulting to \='adwaita\='.
THEME-NAME is a string, e.g., \='adwaita\='."
  (interactive
   (list (completing-read "Select light theme: "
			  (mapcar #'car light-theme-options)  ; Use defvar directly
			  nil t "adwaita")))
  (let* ((selected-theme (or theme-name "adwaita"))  ; Default if nil
	 (theme-symbol (cdr (assoc selected-theme light-theme-options))))
    (unless theme-symbol
      (error "Unknown theme: %s" selected-theme))
    (clear-themes)
    (load-theme theme-symbol t)
    (when (not (display-graphic-p))
      (send-string-to-terminal "\e]12;rgb:00/00/ff\a"))  ; Dark blue cursor for light
    (set-cursor-color "#0000ff")  ; Fallback
    (setq cursor-type 'bar)
    (message "Loaded light theme: %s" selected-theme)))


(defun setup-input-methods ()
  "Set up additional input methods."
  (let ((file (concat emacs-config-directory "/etc/sa-translit.el")))
    (when (file-exists-p file)
      (load-file file)
      (register-input-method
       "sa-translit" "Sanskrit Transliteration" 'quail-use-package
       "sa-translit" "Converts Harvard-Kyoto and ITRANS scheme to IAST diacritics."
       file))))








(defun set-frame-size-and-position (frame)
  "Set the frame size of FRAME to half the screen width and full screen height.
Make it tiled to the left."
  (let* ((screen-width (display-pixel-width))
         (screen-height (display-pixel-height))
	 (decoration-width 42) ;; Adjust as needed
	 (decoration-height 35) ;; Adjust as needed
	 (frame-width (- (floor (/ screen-width 2)) decoration-width))
	 (fudge 10)
	 (frame-height (- (- screen-height decoration-height) fudge)))
    (set-frame-position frame 0 0)
    (set-frame-size frame frame-width frame-height t)))


;;
;; FLAG -- probably call set-default-settings from here or similar.
;;
(defun on-before-make-frame ()
  "Set up settings for new frame, e.g. turning off `tool-bar-mode`."
  (tool-bar-mode -1))


(defun on-after-make-frame (frame)
  "Configure settings for new FRAME."
  (select-frame frame)
  ;;(setup-themes)
  ;; FLAG -- will the below work?
  (set-default-settings)
  ;; 
  (when (display-graphic-p frame)
    ;;(setup-graphical-keybindings-and-faces)
    (set-frame-size-and-position frame)))


(defun setup-themes ()
  "Set up my preferred default themes."
  (dark-theme)

  )


(defun unfill-paragraph ()
  "Transform a filled paragraph into a single long line."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))



(defun unfill-region (start end)
  "Transform the filled region from START to END into a single long line."
  (interactive "r")
  (let ((fill-column (point-max)))
    (fill-region start end nil)))

(defun unfill-buffer ()
  "Transform the filled buffer into single long lines."
  (interactive)
  (unfill-region (point-min) (point-max)))


(defun load-ai-tools ()
  ;; FLAG - fill in 
  (message "FLAG - Load the Llama etc configs here from a separate file."))


;;
;; The actual running of stuff:
;;

(when (file-exists-p "~/.emacs-local-early") (load-file "~/.emacs-local-early"))
(main-setup)
(setup-themes)
(set-default-settings) ;; add hood for calling this for new frames
(load-ai-tools)
(when (file-exists-p "~/.emacs-local") (load-file "~/.emacs-local"))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

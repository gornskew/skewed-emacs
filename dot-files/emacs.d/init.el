;;; init.el --- skewed config -*- lexical-binding: nil -*-
;;; Commentary:
;;; This is my personal Emacs configuration.
;;; It was inspired by but is no longer based on Crafted Emacs.
;;;
;;; Code:

(require 'cl-lib)

;; Define emacs-config-directory
(defvar emacs-config-directory (file-name-directory (file-truename load-file-name)))

(defvar start-lisply? (not (string= (getenv "START_HTTP") "false")))
(defvar skewed-emacs-container? (getenv "SKEWED_EMACS_CONTAINER"))
(defvar skewed-emacs-docker-build? (or (getenv "EMACS_BATCH_MODE")
				       (getenv "SKEWED_EMACS_DOCKER_BUILD")))


;; Redirect custom settings to a separate file
(setq custom-file (concat emacs-config-directory "custom.el"))

(defvar second-party-packages nil)
(defvar third-party-packages nil)

(setq
 third-party-packages
 (remove
  nil
  `((flycheck
     :commands flycheck-mode
     :hook (prog-mode . flycheck-mode)
     :defer (not skewed-emacs-docker-build?)
     )
    (company
     :commands company-mode
     :hook (prog-mode . company-mode)
     :defer (not skewed-emacs-docker-build?)
     )
    (eat
     :commands eat
     :bind ("C-c t" . eat)
     :defer (not skewed-emacs-docker-build?)
     )
    (doom-themes
     :demand t				; Load immediately for UI
     :config (load-theme 'doom-one t)
     :defer nil
     )
    (zenburn-theme
     :demand t				; Alternative theme
     :config (load-theme 'zenburn t)
     :defer nil)
    (ellama
     :commands ellama
     :defer (not skewed-emacs-docker-build?))
    (json
     :mode ("\\.json\\'" . json-mode)
     :defer nil)
    (simple-httpd
     :commands httpd-start
     :defer nil)
    (dashboard
     :demand t				; Load for daemon startup
     :defer nil
     :config (dashboard-setup-startup-hook))
    (paredit
     :hook (emacs-lisp-mode . paredit-mode)
     :defer (not skewed-emacs-docker-build?))
    (magit
     :defer (not skewed-emacs-docker-build?)
     :commands (magit-status magit-blame)
     :bind ("C-x g" . magit-status)
     :config (setq magit-git-executable (locate-file "git" exec-path)))

    ,(unless (eql system-type 'android)
      `(pdf-tools
	:defer (not skewed-emacs-docker-build?)
	:mode ("\\.pdf\\'" . pdf-view-mode)
	:config
	(when skewed-emacs-docker-build?
	  (pdf-tools-install))))
    (org
     :defer (not skewed-emacs-docker-build?)
     :commands (org-mode org-agenda)
     :hook (org-mode . org-config-setup)
     ))))

(setq
 second-party-packages
 `((org-config
    :defer (not skewed-emacs-docker-build?)
    :load-path ,(lambda () (get-config-path "etc"))
    :hook (org-mode . (lambda () (require 'org-config))))
   (sa-translit-config
    :defer nil
    :load-path ,(lambda () (get-config-path "etc")))
   (dashboard-config
    :defer nil
    :load-path ,(lambda () (get-config-path "etc"))
    :config
    (require 'dashboard-config)
    (generate-skewed-dashboard-banner))
   (impatient-markdown-config
    :defer (not skewed-emacs-docker-build?)
    :load-path ,(lambda () (get-config-path "etc")))
   (slime-config
    :defer (not skewed-emacs-docker-build?)
    :load-path ,(lambda () (get-config-path "etc"))
    :commands (slime slime-connect slime-repl slime-selector
		     load-and-or-start-gendl set-slime-shortcuts)
    :mode (("\\.lisp\\'" . lisp-mode)
           ("\\.cl\\'" . lisp-mode)
           ("\\.gdl\\'" . lisp-mode)
           ("\\.gendl\\'" . lisp-mode)
           ("\\.lhtm\\'" . lisp-mode)
           ("\\.lhtml\\'" . lisp-mode)
           ("\\.sexp\\'" . lisp-mode)
           ("\\.sexpr\\'" . lisp-mode)
           ("\\.sexps\\'" . lisp-mode))
    :hook (lisp-mode . (lambda () (require 'slime-config))))
   (lisply-config
    :defer nil
    :load-path ,(lambda () (get-config-path "etc"))
    :config
    (when (and start-lisply? (not skewed-emacs-docker-build?))
      (require 'lisply-config)
      (setq httpd-host "0.0.0.0")
      (emacs-lisply-start-server)))))

(load (concat emacs-config-directory "etc/load-and-compile.el"))
(setup-packages-and-customizations emacs-config-directory)

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

(defun main-setup ()
  "Set up hooks and Lisply server if enabled."

  (add-hook 'before-make-frame-hook 'on-before-make-frame)
  (add-hook 'after-make-frame-functions 'on-after-make-frame)
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)



  ;;
  ;; FLAG -- do something to not have to hardcode "gendl-ccl" and the port here. 
  ;;
  (when skewed-emacs-container?
    (message "We Are a Skewed-emacs container, defaulting slime-connect settings to
gendl-ccl/4200.")
    (setq slime-lisp-host "gendl-ccl"
   	  slime-connect-host-history '("gendl-ccl")
   	  slime-port 4200
   	  slime-connect-port-history '("4200")))

  (message "Done with main-setup.")


  )

(defun set-default-settings ()
  "Set my personal preferred default settings."
  (interactive)

  (if (display-graphic-p)
      (setup-graphical-keybindings-and-faces)
    (setup-terminal-keybindings-and-faces))
  (setup-other-keybindings-and-faces)
  (when (display-graphic-p)
    ;;(set-fontset-font "DejaVu Sans" nil t)
    (setq-default
     font-use-system-font t
     inhibit-compacting-font-caches t
     line-spacing 0.1)
    (when (package-installed-p 'pdf-tools)
      (setq pdf-view-display-size 'fit-page
            pdf-view-resize-factor 1.1
            pdf-view-use-scaling t
            pdf-view-use-imagemagick nil))
    
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
  (setq column-number-mode t)
  (global-font-lock-mode t)
  (setq-default transient-mark-mode t)
  (show-paren-mode t)
  (setq scroll-step 1)
  (setup-input-methods)
  (setq confirm-kill-processes nil)
  (menu-bar-mode 0)
  (tool-bar-mode 0))


(defun enable-company-mode ()
  "Enable company mode."
  (interactive)
  (company-mode 1))

(defun disable-company-mode ()
  "Disable company mode."
  (interactive)
  (company-mode 0))

(defun disable-line-number-mode ()
  "Disable line number mode."
  (interactive)
  (line-number-mode -1))

(defun server-shutdown ()
  "Save buffers, quit, and shutdown server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;;(defun cycle-slime-repl-buffers ()
;;  "Cycle through possible *slime-repl...* buffers."
;;  (interactive)
;;  (let ((buffers (buffer-list)))
;;    (dolist (buffer buffers)
;;      (if (string-match-p "\\*slime-repl.*\\*" (buffer-name buffer))
;;          (switch-to-buffer buffer)))))

(defun setup-terminal-keybindings-and-faces ()
  "Set up bindings for terminal."
  (interactive)
  (message "About to set up keybindings for terminal use.")
  (dolist (map (list input-decode-map function-key-map))
    (define-key map "[113;7u" (kbd "C-M-q"))
    (define-key map "[102;7u" (kbd "C-M-f"))
    (define-key map "[98;7u" (kbd "C-M-b"))
    (define-key map "[107;7u" (kbd "C-M-k")))
  (global-set-key (kbd "ESC C-M-f") 'forward-sexp)
  (global-set-key (kbd "ESC C-M-b") 'backward-sexp)
  (global-set-key (kbd "ESC C-M-k") 'kill-sexp)
  (global-set-key (kbd "ESC C-M-q") 'indent-sexp)
  (message "Done with keybindings setup."))

(defun setup-graphical-keybindings-and-faces ()
  "Set up keybindings and faces for graphical mode."
  (let ((scale-factor (if (> (x-display-pixel-width) 1920) 1.5 1.0)))
    (set-face-attribute 'default nil
                        :height (round (* 110 scale-factor))
                        :weight 'normal
                        :width 'normal)))

(defun setup-other-keybindings-and-faces ()
  "Set up additional keybindings and faces."
  (interactive)
  (global-set-key (kbd "\C-x y") 'previous-window-any-frame)
  (global-set-key (kbd "\C-c 8") "•")
  (global-set-key "\M-=" 'just-one-space)
  (global-set-key "\C-x\C-b" 'electric-buffer-list)
  ;;(global-set-key (kbd "C-x &") 'cycle-slime-repl-buffers)
  (global-set-key (kbd "C-c M-q") 'unfill-paragraph)
  (global-set-key (kbd "C-c x") 'server-shutdown))

(defun enable-mouse ()
  "Enable mouse interaction in terminal mode."
  (interactive)
  (unless window-system
    (require 'mouse)
    (xterm-mouse-mode t)
    (defun track-mouse (e))
    ;; (setq mouse-sel-mode t) ;; what is this supposed to be 
    ))

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
    (send-string-to-terminal "\e]12;rgb:ff/00/ff\a"))
  (set-cursor-color "#ff00ff"))

(defun dark-theme (&optional theme-name)
  "Load a dark theme, defaulting to `doom-tokyo-night."
  (interactive
   (list (completing-read "Select dark theme: "
                          (mapcar #'car dark-theme-options)
                          nil t "doom-tokyo-night")))
  (let* ((selected-theme (or theme-name "doom-tokyo-night"))
         (theme-symbol (cdr (assoc selected-theme dark-theme-options))))
    (unless theme-symbol
      (error "Unknown theme: %s" selected-theme))
    (clear-themes)
    (load-theme theme-symbol t)
    (when (not (display-graphic-p))
      (send-string-to-terminal "\e]12;rgb:ff/00/ff\a"))
    (set-cursor-color "#ff00ff")
    (setq cursor-type 'bar)
    (message "Loaded dark theme: %s" selected-theme)))

(defun light-theme (&optional theme-name)
  "Load a light theme, defaulting to `adwaita`."
  (interactive
   (list (completing-read "Select light theme: "
                          (mapcar #'car light-theme-options)
                          nil t "adwaita")))
  (let* ((selected-theme (or theme-name "adwaita"))
         (theme-symbol (cdr (assoc selected-theme light-theme-options))))
    (unless theme-symbol
      (error "Unknown theme: %s" selected-theme))
    (clear-themes)
    (load-theme theme-symbol t)
    (when (not (display-graphic-p))
      (send-string-to-terminal "\e]12;rgb:00/00/ff\a"))
    (set-cursor-color "#0000ff")
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
  "Set the frame size of FRAME to half the screen width and full screen height."
  (let* ((screen-width (display-pixel-width))
         (screen-height (display-pixel-height))
         (decoration-width 42)
         (decoration-height 35)
         (frame-width (- (floor (/ screen-width 2)) decoration-width))
         (fudge 10)
         (frame-height (- (- screen-height decoration-height) fudge)))
    (set-frame-position frame 0 0)
    (set-frame-size frame frame-width frame-height t)))

(defun on-before-make-frame ()
  "Set up settings for new frame, e.g., turning off `tool-bar-mode`."
  (tool-bar-mode -1))

(defun on-after-make-frame (frame)
  "Configure settings for new FRAME."
  (select-frame frame)
  (set-default-settings)
  (when (display-graphic-p frame)
    (set-frame-size-and-position frame)))



(defun setup-themes ()
  "Set up my preferred default themes."
  (dark-theme))

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
  "Load AI-related configurations."
  (message "FLAG - Load the Llama etc configs here from a separate file."))

(defun skewed-initialize ()
  "Main initialize."
  (let ((start-time (float-time)) curr-time elapsed)
    (setq curr-time start-time)
    
    (when (file-exists-p "~/.emacs-local-early")
      (load-file "~/.emacs-local-early"))
    
    (main-setup)

    (let ((float-time (float-time)))
      (setq elapsed (- float-time curr-time))
      (setq curr-time float-time))

    (message "Done with main-setup in %s seconds."
	     elapsed)
    
    (when (file-exists-p custom-file)
      (load (file-name-sans-extension custom-file)))
    
    (setup-themes)

    (let ((float-time (float-time)))
      (setq elapsed (- float-time curr-time))
      (setq curr-time float-time))
    (message "Done with theme setups in %s seconds."
	     elapsed)
    
    (set-default-settings)

    (let ((float-time (float-time)))
      (setq elapsed (- float-time curr-time))
      (setq curr-time float-time))
    (message "Done with default settings setting in %s seconds."
	     elapsed)
    
    
    ;; bump this to load-and-compile
    (load-ai-tools)

    
    (when (file-exists-p "~/.emacs-local")
      (load-file "~/.emacs-local"))

    (message "Done with skewed-initialize in %s seconds."
	     (- (float-time) start-time))

    ))


(skewed-initialize)

(provide 'init)

;;; init.el ends here

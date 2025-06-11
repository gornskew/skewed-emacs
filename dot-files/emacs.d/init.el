;;; init.el -- Emacs configuration -*- lexical-binding: nil -*-
;;; Commentary:
;;; This is my personal Emacs configuration.
;;; It is no longer based on Crafted Emacs.
;;; Code:

(require 'cl-lib)
(defvar too-old-p (< emacs-major-version 27))
(defvar really-old-p (< emacs-major-version 24))
(defvar inhibit-gendl-splash-p t)
(defvar emacs-config-directory (file-name-directory (file-truename load-file-name)))
(defvar my-files-to-load nil)
(defvar load-lisply? t)

(defun ensure-package-installed (pkg)
  "Ensure PKG is installed.  If not, install it."
  (unless (package-installed-p pkg)
    (package-install pkg)))

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

(defun load-one-config (file directory)
  "Load a single configuration file FILE from DIRECTORY."
  (let ((pwd (or directory (concat emacs-config-directory "/etc/"))))
    (let ((full-path (concat pwd file ".el")))
      (message "About to attempt load-file on %s..." full-path)
      (if (file-exists-p full-path) (load-file full-path)
        (warn "%s seems to be missing, cannot load-file on it." full-path)))))

(defun my-all-the-icons-fonts-installed-p ()
  "Check if all-the-icons fonts are installed."
  (let ((fonts '("all-the-icons" "file-icons" "github-octicons" "Weather Icons")))
    (cl-every (lambda (font) (member font (font-family-list))) fonts)))

(defun my-install-all-the-icons-fonts ()
  "Install all-the-icons fonts if they are not already installed."
  (unless (my-all-the-icons-fonts-installed-p)
    (all-the-icons-install-fonts t)))

;;(use-package vterm
;;  :ensure t
;;  :config
;;  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes")
;;  (add-to-list 'load-path "~/.emacs.d/vterm"))


(defun install-chatgpt ()
  "Install the chatgpt multi-model shell."
  (use-package chatgpt-shell
    :ensure t
    :custom
    ((chatgpt-shell-openai-key
      (or (getenv "OPENAI_API_KEY") 
          (plist-get (car (auth-source-search :host "llm.openai")) :secret)
          "your-api-key-here")))))


(defun main-setup ()
  "Set up the main configuration."

  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  (let ((packages-to-install
	 '(flycheck company
		    simple-httpd
		    ;;undo-tree
		    ;;vscode-dark-plus-theme
		    ;;all-the-icons
		    ;;neotree
		    ;;treemacs
		    ;; vterm
		    ;; dashboard
		    ;;minimap
		    ;;lsp-mode
		    ;;lsp-ui
		    ;;lsp-treemacs
		    ;;treemacs-all-the-icons
		    ;;lsp-ivy
		    doom-themes
		    zenburn-theme
		    use-package
		    ellama
		    ))
	(need-package-refresh-contents? nil))

    (dolist (pack packages-to-install)
      (unless (package-installed-p pack) (setq need-package-refresh-contents? t)))

    (when need-package-refresh-contents?
      (package-refresh-contents) (package-initialize)
      )

    (setq my-files-to-load
	  `(,@(unless really-old-p '("slime"))
            ,@(unless too-old-p
		'("org" "magit" "straight" "copilot" "impatient-markdown"
		  ;;"dashboard"
		  ))))

    (dolist (filespec my-files-to-load)
      (if (listp filespec)
	  (load-one-config (car filespec) (cadr filespec)) ;; car and cadr - really??
	(load-one-config filespec nil)))
    (dolist (pack packages-to-install)
      (ensure-package-installed pack))

    ;;(install-chatgpt) ;; needs some troubleshooting
    
    (require 'flycheck)(global-flycheck-mode)
    (require 'company)
    (require 'doom-themes)(require 'zenburn-theme)
    (dolist (hook '(c-mode-hook c++-mode-hook python-mode-hook
				emacs-lisp-mode-hook lisp-mode-hook))
      (add-hook hook 'enable-company-mode))
    (dolist (hook '(slime-repl-mode-hook dired-mode-hook treemacs-mode-hook))
      (add-hook hook 'disable-company-mode))

    (dolist (hook '(slime-repl-mode-hook
		    markdown-mode-hook
		    lisp-mode-hook elisp-mode-hook
		    dired-mode-hook treemacs-mode-hook))
      (add-hook hook 'disable-line-number-mode))

    ))

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
  (let ((scale-factor (if (> (x-display-pixel-width) 1920) 1.5 1.0)))
    
    (set-face-attribute 'default nil
			;;:family "Source Code Pro"
			:height (round (* 110 scale-factor))
			:weight 'normal
			:width 'normal)))


(defun setup-other-keybindings-and-faces ()
  "Set up bindings for graphical mode."
  (interactive)
  (global-set-key (kbd "\C-c 8") "•")
  (global-set-key "\M-=" 'just-one-space)
  ;; Modified keyboard shortcuts
  (global-set-key "\C-x\C-b" 'electric-buffer-list)

  (global-set-key (kbd "C-x &") 'cycle-slime-repl-buffers)

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


(defvar dark-theme-options
  '(("doom-tokyo-night" . doom-tokyo-night)
    ("doom-one" . doom-one)
    ("doom-pine" . doom-pine)
    ("doom-purple" . doom-shades-of-purple)
    ("doom-gruvbox" . doom-gruvbox)
    ("modus-vivendi" . modus-vivendi-deuteranopia)
    ("zenburn" . zenburn))
  "List of dark themes for `dark-theme' function.")

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

(defvar light-theme-options
  '(("adwaita" . adwaita)
    ("tsdh-light" . tsdh-light)
    ("light-blue" . light-blue)
    ("doom-one-light" . doom-one-light)
    ("doom-feather-light" . doom-feather-light)
    ("doom-light" . doom-gruvbox-light))
  "List of light themes for `light-theme' function.")

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

  ;;(my-install-all-the-icons-fonts)


  ;; Treemacs setup
;;  (use-package treemacs
;;    :ensure t
;;    :defer t
;;    :config
;;    (setq treemacs-width 30)
;;    :bind
;;    (:map global-map
;;          ("M-0" . treemacs-select-window)
;;          ("C-x t 1" . treemacs-delete-other-windows)
;;          ("C-x t t" . treemacs)))
  
;;  (use-package treemacs-all-the-icons
;;    :ensure t
;;    :config (treemacs-load-theme "all-the-icons"))
  
  ;; Minimap setup
;;  (use-package minimap
;;    :ensure t
;;    :config
;;    (setq minimap-width-fraction 0.1)
    ;;(add-hook 'prog-mode-hook 'minimap-mode)
;;    )

  ;; Load Lisply MCP service if enabled still using manual load, this
  ;; is not quite packaged yet as a proper emacs package,
  ;; it's just built-in to skewed-emacs's config thusly:
  (when load-lisply?
   (let ((lisply-dir (concat emacs-config-directory "/sideloaded/lisply-backend/source/")))
      (when (file-exists-p lisply-dir)
        (message "Loading Lisply service from %s" lisply-dir)
        (dolist (file '("http-setup.el" "endpoints.el" "backend.el"))
          (let ((file-path (concat lisply-dir file)))
            (if (file-exists-p file-path)
                (load-file file-path)
              (message "Warning: MCP service file %s not found" file-path))))))


;; COMMENTED OUT FOR CONTAINER USE - startup.sh handles this
   ) ;; Close the (when load-lisply? block

   ;;    (emacs-lisply-start-server))


  ;; Load local customizations if they exist
  (when (file-exists-p "~/.emacs-local")
    (load-file "~/.emacs-local"))

  )

;;(defun my/vscode-layout ()
;;  "Setup the layout to mimic VS Code."
;;  (interactive)
;;  (treemacs)
;;  (minimap-mode 1)
;;  (eshell))



(defun set-frame-size-and-position (frame)
  "Set the frame size of FRAME to half the screen width and full screen height.
Make it tiled to the left."
  (let* ((screen-width (display-pixel-width))
         (screen-height (display-pixel-height))
	 (decoration-width 42) ;; Adjust as needed
	 (decoration-height 35) ;; Adjust as needed
	 (frame-width (- (floor (/ screen-width 2)) decoration-width))
	 (frame-height (- screen-height decoration-height)))
    (set-frame-position frame 0 0)
    (set-frame-size frame frame-width frame-height t)))



(defun on-before-make-frame ()
  "Set up settings for new frame, e.g. turning off `tool-bar-mode`."
  (tool-bar-mode -1))


(defun on-after-make-frame (frame)
  "Configure settings for new FRAME."
  (select-frame frame)
  (setup-themes)
  (when (display-graphic-p frame)
    ;;(my-install-all-the-icons-fonts)
    (setup-graphical-keybindings-and-faces)
    (set-frame-size-and-position frame)))

(defvar neo-smart-open nil)

(defun setup-themes ()
  "Set up my preferred default themes."
  (dark-theme)

  )


(add-hook 'before-make-frame-hook 'on-before-make-frame)
(add-hook 'after-make-frame-functions 'on-after-make-frame)



(when (display-graphic-p)
  (on-after-make-frame (selected-frame)))


;; FLAG -- sort out load-gdl
;;

;;(add-hook 'emacs-startup-hook 'my/vscode-layout)

(main-setup)
(set-default-settings)
;;(load-gdl)
(setup-themes)


(defun unfill-paragraph ()
  "Transform a filled paragraph into a single long line."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(global-set-key (kbd "C-c M-q") 'unfill-paragraph)  ; Optional: Bind it to a key


(defun unfill-region (start end)
  "Transform the filled region from START to END into a single long line."
  (interactive "r")
  (let ((fill-column (point-max)))
    (fill-region start end nil)))

(defun unfill-buffer ()
  "Transform the filled buffer into single long lines."
  (interactive)
  (unfill-region (point-min) (point-max)))


(use-package eat
  :ensure t)
  ;; :config
  ;; Set TERM to a true color-capable value inside eat
;;;  (setq eat-term-name "xterm-truecolor")
  ;; Optional: Ensure true color is recognized
  ;; NOTE - eat currently doesn't support xterm-truecolor,
  ;; we need to hold it back to xterm-256color.
(add-hook 'eat-mode-hook
          (lambda ()
            (setenv "TERM" "xterm-256color")))

(when (not (display-graphic-p))
  (setenv "TERM" "xterm-256color")
  (require 'term)
  (terminal-init-xterm))

(add-hook 'eshell-load-hook #'eat-eshell-mode)

(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)


;; Retrieve API keys from environment variables or auth-source
(defvar *anthropic-key* (or (getenv "ANTHROPIC_API_KEY")
                            (plist-get (car (auth-source-search :host "llm.anthropic")) :secret)
                            "<YOUR-ANTHROPIC-KEY-HERE>"))
(defvar *openai-key* (or (getenv "OPENAI_API_KEY")
                         (plist-get (car (auth-source-search :host "llm.openai")) :secret)
                         "<YOUR-OPENAI-KEY-HERE>"))


(straight-use-package '(gptel :type git
			      :host github
			      :repo "karthink/gptel"
			      :branch "feature-tool-use"))
(require 'gptel)
(setq gptel-api-key *openai-key*)
(gptel-make-anthropic "Claude" :stream t :key *anthropic-key*)


(require 'llm)
(require 'llm-openai)
(require 'llm-claude)
(require 'llm-ollama)
(require 'llm-deepseek)

;; Claude Models
(defvar *claude-sonnet*
  (make-llm-claude :default-chat-temperature 0.3
                   :default-chat-max-tokens 64000
                   :chat-model "claude-3-7-sonnet-20250219"
                   :key *anthropic-key*))

(defvar *claude-haiku*
  (make-llm-claude :default-chat-temperature 0.3
                   :default-chat-max-tokens 1000000
                   :chat-model "claude-3-5-haiku-20241022"
                   :key *anthropic-key*))

(defvar *claude-opus*
  (make-llm-claude :default-chat-temperature 0.3
                   :default-chat-max-tokens 1000000
                   :chat-model "claude-3-opus-20240229"
                   :key *anthropic-key*))

;; OpenAI Model
(defvar *openai-gpt-4o-mini*
  (make-llm-openai :default-chat-temperature 0.3
                   :default-chat-max-tokens 1000000
                   :chat-model "gpt-4o-mini"
                   :key *openai-key*))

;; Ollama Models (running on localhost)
(defvar *ollama-codellama*
  (make-llm-ollama :host "localhost"
                   :port 11434
                   :chat-model "codellama:latest"
                   :embedding-model "codellama:latest"
                   :default-chat-temperature 0.3))

(defvar *ollama-phi3*
  (make-llm-ollama :host "localhost"
                   :port 11434
                   :chat-model "phi3:latest"
                   :embedding-model "phi3:latest"
                   :default-chat-temperature 0.3))

(defvar *ollama-mistral*
  (make-llm-ollama :host "localhost"
                   :port 11434
                   :chat-model "mistral:latest"
                   :embedding-model "mistral:latest"
                   :default-chat-temperature 0.3))

(defvar *ollama-llama2*
  (make-llm-ollama :host "localhost"
                   :port 11434
                   :chat-model "llama2:latest"
                   :embedding-model "llama2:latest"
                   :default-chat-temperature 0.3))

(defvar *ollama-codellama-13b-instruct-q4*
  (make-llm-ollama :host "localhost"
                   :port 11434
                   :chat-model "codellama:13b-instruct-q4_0"
                   :embedding-model "codellama:13b-instruct-q4_0"
                   :default-chat-temperature 0.3))

(defvar *ollama-qwen2.5-14b-instruct-q4*
  (make-llm-ollama :host "localhost"
                   :port 11434
                   :chat-model "qwen2.5:14b-instruct-q4_0"
                   :embedding-model "qwen2.5:14b-instruct-q4_0"
                   :default-chat-temperature 0.3))

(defvar *ollama-qwen2.5-7b-instruct*
  (make-llm-ollama :host "localhost"
                   :port 11434
                   :chat-model "qwen2.5:7b-instruct"
                   :embedding-model "qwen2.5:7b-instruct"
                   :default-chat-temperature 0.3))

(defvar *ollama-deepseek-coder-6.7b-instruct-q8*
  (make-llm-ollama :host "localhost"
                   :port 11434
                   :chat-model "deepseek-coder:6.7b-instruct-q8_0"
                   :embedding-model "deepseek-coder:6.7b-instruct-q8_0"
                   :default-chat-temperature 0.3))

(defvar *ollama-deepseek-coder*
  (make-llm-ollama :host "localhost"
                   :port 11434
                   :chat-model "deepseek-coder:latest"
                   :embedding-model "deepseek-coder:latest"
                   :default-chat-temperature 0.3))

;; Ellama Setup
(use-package ellama
  :ensure t
  :init
  (setopt ellama-provider *claude-sonnet*) ;; Default provider
  (defun switch-ellama-provider (provider)
    "Switch the ellama provider to PROVIDER."
    (interactive
     (list (completing-read "Select provider: "
                            '(("Claude Sonnet" . *claude-sonnet*)
                              ("Claude Haiku" . *claude-haiku*)
                              ("Claude Opus" . *claude-opus*)
                              ("GPT-4o Mini" . *openai-gpt-4o-mini*)
                              ("Ollama CodeLLaMA" . *ollama-codellama*)
                              ("Ollama Phi3" . *ollama-phi3*)
                              ("Ollama Mistral" . *ollama-mistral*)
                              ("Ollama LLaMA2" . *ollama-llama2*)
                              ("Ollama CodeLLaMA 13B Instruct Q4" . *ollama-codellama-13b-instruct-q4*)
                              ("Ollama Qwen2.5 14B Instruct Q4" . *ollama-qwen2.5-14b-instruct-q4*)
                              ("Ollama Qwen2.5 7B Instruct" . *ollama-qwen2.5-7b-instruct*)
                              ("Ollama DeepSeek Coder 6.7B Instruct Q8" . *ollama-deepseek-coder-6.7b-instruct-q8*)
                              ("Ollama DeepSeek Coder" . *ollama-deepseek-coder*))
                            nil t)))
    (let ((provider-map '(("Claude Sonnet" . *claude-sonnet*)
                          ("Claude Haiku" . *claude-haiku*)
                          ("Claude Opus" . *claude-opus*)
                          ("GPT-4o Mini" . *openai-gpt-4o-mini*)
                          ("Ollama CodeLLaMA" . *ollama-codellama*)
                          ("Ollama Phi3" . *ollama-phi3*)
                          ("Ollama Mistral" . *ollama-mistral*)
                          ("Ollama LLaMA2" . *ollama-llama2*)
                          ("Ollama CodeLLaMA 13B Instruct Q4" . *ollama-codellama-13b-instruct-q4*)
                          ("Ollama Qwen2.5 14B Instruct Q4" . *ollama-qwen2.5-14b-instruct-q4*)
                          ("Ollama Qwen2.5 7B Instruct" . *ollama-qwen2.5-7b-instruct*)
                          ("Ollama DeepSeek Coder 6.7B Instruct Q8" . *ollama-deepseek-coder-6.7b-instruct-q8*)
                          ("Ollama DeepSeek Coder" . *ollama-deepseek-coder*))))
      (setopt ellama-provider (cdr (assoc provider provider-map)))
      (message "Ellama provider switched to %s" provider)))
  :bind
  (("C-c e" . ellama-chat)
   ("C-c s" . switch-ellama-provider)))

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478"
     "113a135eb7a2ace6d9801469324f9f7624f8c696b72e3709feb7368b06ddaccc"
     "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b"
     "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
     "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

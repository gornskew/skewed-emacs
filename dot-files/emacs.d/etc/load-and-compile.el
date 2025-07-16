;;; package --- Summary: init.el 
;;; Commentary:
;;; This is my personal Emacs configuration.
;;; It was inspired by but is no longer based on Crafted Emacs.
;;;
;;; Code:

;; Ensure use-package is installed

(require 'cl-lib)

;; Configure package archives with fallback to Tsinghua mirrors
(defun check-url-reachable (url)
  "Check if URL is reachable by attempting an HTTP request."
  (condition-case nil
      (let ((url-request-method "HEAD"))
        (url-retrieve-synchronously url t t)
        t)
    (error nil)))

(defun configure-package-archives ()
  "Set package-archives to official GNU/MELPA if reachable, else Tsinghua mirrors."
  (let* ((official-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			      ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                              ("melpa" . "https://melpa.org/packages/")))
         (tsinghua-archives
	  '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	    ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
            ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
         (gnu-official (cl-rest (cl-assoc "gnu" official-archives :test #'string-equal)))
	 (nongnu-official (cl-rest (cl-assoc "nongnu" official-archives :test #'string-equal)))
         (melpa-official (cl-rest (cl-assoc "melpa" official-archives :test #'string-equal))))
    (if (and (check-url-reachable gnu-official)
	     (check-url-reachable nongnu-official)
             (check-url-reachable melpa-official))
        (progn
          (message "Using official GNU and MELPA repositories")
          (setq package-archives official-archives))
      (progn
        (message "Official repositories unreachable, trying with Tsinghua.edu mirrors...")
        (setq package-archives tsinghua-archives)))))

;; Initialize package archives
(configure-package-archives)

(setq native-comp-jit-compilation nil)
(setq native-comp-deferred-compilation nil)
(setq native-comp-async-jobs-number 0)

(defun ensure-package-installed (pkg)
  "Ensure PKG is installed.  If not, install it."
  (unless (package-installed-p pkg)
    (package-install pkg)))


(defvar standard-skewed-packages '(use-package
				    flycheck
				    company
				    simple-httpd
				    copilot
				    eat
				    doom-themes
				    zenburn-theme
				    ellama
				    chatgpt-shell
				    magit
				    pdf-tools
				    json

				    ;;all-the-icons
				    ;;undo-tree
				    ;;vscode-dark-plus-theme
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
				    ))



(defun batch-byte-compile-directory (directory)
  "Find and byte-compile all .el files in DIRECTORY and its subdirectories."
  (interactive "Directory: ")
  (let ((el-files (directory-files-recursively
                   directory
                   "\\.el$"
                   nil
                   (lambda (dir) (not (string-match-p "\\.git" dir))))))
    (dolist (file el-files)
      (when (file-exists-p file)
        (byte-compile-file file)))))


(defun ensure-standard-packages ()
  ;;
  ;; Refresh package contents if needed
  ;;
  (let ((need-package-refresh-contents? nil))
    (cl-dolist (pack standard-skewed-packages)
      (unless (package-installed-p pack)
	(package-refresh-contents)
	(package-initialize)
	(cl-return))))
  
  (dolist (pack standard-skewed-packages)
    (ensure-package-installed pack))

  ;;
  ;; FLAG -- force compile everything and wait for finish. 
  ;;
  (batch-byte-compile-directory emacs-config-directory)
  
  
  )




(ensure-standard-packages)



;; stuff to drag back out later from history:

;;(defun my-all-the-icons-fonts-installed-p ()
;; "Check if all-the-icons fonts are installed."
;; (let ((fonts '("all-the-icons" "file-icons"
;;		"github-octicons" "Weather Icons")))
;;   (cl-every (lambda (font) (member font (font-family-list))) fonts)))


;;(use-package vterm
;;  :ensure t
;;  :config
;;  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes")
;;  (add-to-list 'load-path "~/.emacs.d/vterm"))

;;;; Install and configure all-the-icons
;;(use-package all-the-icons
;;  :ensure t)

;;(unless (find-font (font-spec :name "all-the-icons"))
;;  (all-the-icons-install-fonts t))

;; Install and configure doom-modeline
;;(use-package doom-modeline
;;  :ensure t
;;  :after all-the-icons
;;  :config
;;  ;; Customize doom-modeline (optional)
;;  (setq doom-modeline-height 25
;;        doom-modeline-bar-width 3
;;        doom-modeline-icon t
;;        doom-modeline-major-mode-icon t))



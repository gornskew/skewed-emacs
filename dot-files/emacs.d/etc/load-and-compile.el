;;; load-and-compile.el -*- lexical-binding: nil -*- --- Initialize and compile Emacs packages
;;; Commentary:
;;; This file handles package installation, loading, and compilation based on environment variables.
;;; - If $SKEWED_EMACS_CONTAINER is true, assume packages are installed/compiled, set up load-path, and load packages without internet access.
;;; - If $EMACS_BATCH_MODE is true and $SKEWED_EMACS_CONTAINER is false, install and load packages/customizations, compile all .el files in CONFIG-DIR after main-setup.
;;; - Otherwise, install/load packages/customizations with deferred compilation.
;;;
;;; Code:

(require 'cl-lib)
(require 'use-package)

(setq use-package-always-ensure t) ; Ensure packages are installed

(setq ;; native-comp-deferred-compilation nil ;; obsolete
      native-comp-jit-compilation nil)

;; need this or no ? 
;;(setq package-check-signature nil) ; Avoid signature checks
;;(setq package-refresh-contents nil) ; Prevent automatic refresh

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



(defun num-cpus () (cl-first (read-from-string (shell-command-to-string "nproc"))))


(defun await-async-compilations (item)
  (let ((wait-time 0)(increment 1))
    (while (> (hash-table-count comp-async-compilations) 0)
      (message "Waiting for %d native compilation jobs to complete..."
	       (hash-table-count comp-async-compilations))
      (message "%s sec on %s, %s async compile jobs still active."
	       (cl-incf wait-time increment) item
	       (hash-table-count comp-async-compilations))
      (sleep-for increment))
    (message "Done Native Compiling %s in %s sec" item wait-time)))

(defun recompile-all-packages ()
  "Force recompile all installed packages (byte and native)."
  (interactive)
  (let* ((packages package-alist)
	 (package-names (mapcar #'cl-first packages))
	 (package-dirs (mapcar #'package-desc-dir (mapcar #'car (mapcar #'cl-rest packages)))))

    (let ((num-cpus (floor (/ (num-cpus) 2))))
      (when (package-installed-p 'pdf-tools)
	(require 'pdf-tools) (pdf-tools-install t)
	(await-async-compilations 'pdf-tools))
      (when (package-installed-p 'simple-httpd)
	(require 'simple-httpd)
	(httpd-start)(httpd-stop))
      (dolist (pkg package-names)
	(message "Pre-loading package: %s" pkg)
	(require pkg nil t)
	(await-async-compilations pkg))
      (cl-mapc (lambda (package-name directory)
		 (when (file-directory-p directory)
		   (message "Byte Recompiling package: %s" package-name)
		   (byte-recompile-directory directory 0 t)))  package-names package-dirs)

      (byte-compile-file (concat emacs-config-directory "init.el"))
      (byte-compile-file (concat emacs-config-directory "/etc/load-and-compile.el"))

      (when (featurep 'native-compile)
	(let ((native-comp-async-jobs-number num-cpus))
	  (message "Beginning Native Compiling Processes for %s directories" (length package-dirs))
          (native-compile-async package-dirs t)
	  (await-async-compilations "installed packages")
	  (native-compile-async (concat emacs-config-directory "init.el"))
	  (native-compile-async (concat emacs-config-directory "etc"))
	  (native-compile-async (concat emacs-config-directory "sideloaded"))
	  (await-async-compilations "local config files"))))))


(defun get-config-path (relative)
  (concat emacs-config-directory relative))

(defun all-packages-installed-p ()
  "Check if all third-party packages are installed."
  (cl-every (lambda (pkg-entry)
	      (package-installed-p (if (symbolp pkg-entry) pkg-entry (car pkg-entry))))
	    third-party-packages))


(defun setup-packages-and-customizations (&optional config-dir)
  "Install and load packages and customizations based on environment variables."
  (unless config-dir (setq config-dir "~/.emacs.d/"))
  (let ((in-container? skewed-emacs-container?))
    (package-initialize)
    ;; Only configure archives and refresh if third-party packages are missing
    (unless (all-packages-installed-p)
      (configure-package-archives)
      (when (or (not package-archive-contents)
                (not (file-exists-p (concat package-user-dir "/archives/gnu/archive-contents")))
                (not (file-exists-p (concat package-user-dir "/archives/nongnu/archive-contents")))
                (not (file-exists-p (concat package-user-dir "/archives/melpa/archive-contents"))))
	(package-refresh-contents)))
    (dolist (pkg-entry third-party-packages)
      (if (symbolp pkg-entry)
	  (progn
            (eval `(use-package ,pkg-entry :ensure t))
	    (eval `(require ',pkg-entry)))
	(progn
          (eval `(use-package ,(car pkg-entry) :ensure t ,@(cdr pkg-entry)))
	  (eval `(require ',(car pkg-entry))))))
    (dolist (pkg-entry second-party-packages)
      (if (symbolp pkg-entry)
	  (let ((use-package-expression `(use-package ,pkg-entry :ensure nil))
		(require-expression `(require ',pkg-entry)))
            (eval use-package-expression)
	    (eval require-expression))
	(let ((use-package-expression `(use-package ,(car pkg-entry) :ensure nil ,@(cdr pkg-entry)))
	      (require-expression `(require ',(car pkg-entry))))
	  (eval use-package-expression)
	  (eval require-expression))))))



(provide 'load-and-compile)
;;; load-and-compile.el ends here

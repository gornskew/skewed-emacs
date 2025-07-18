;;; load-and-compile.el --- Initialize and compile Emacs packages
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

(defun recompile-all-packages ()
  "Force recompile all installed packages (byte and native)."
  (interactive)
  (let* ((packages package-alist)
	 (package-names (mapcar #'cl-first packages))
	 (package-dirs (mapcar #'package-desc-dir (mapcar #'car (mapcar #'cl-rest packages)))))
    (let ((num-cpus (floor (/ (num-cpus) 2))))      
      (cl-mapc (lambda (package-name directory)
	      (when (file-directory-p directory)
		(message "Byte Recompiling package: %s" package-name)
		(byte-recompile-directory directory 0 t)))
	    package-names package-dirs)
      (when (featurep 'native-compile)
	(let ((native-comp-async-jobs-number num-cpus)
	      (wait-time-total 0)) ;; binds dynamically into call to native-compile-async call
	  (cl-mapc (lambda (package-name directory)
		     (when (file-directory-p directory)
		       (message "Beginning Native Compiling Processes for  package: %s" package-name)
		       (native-compile-async directory t) ;; this spawns background jobs without waiting
		       (let ((wait-time 0)(increment 1))
			 (while (> (hash-table-count comp-async-compilations) 0)
			   (message "Waiting for %d native compilation jobs to complete..."
				    (hash-table-count comp-async-compilations))
			   (message "%s sec on %s, %s sec total" 
				    (cl-incf wait-time increment) package-name
				    (cl-incf wait-time-total))
			   (sleep-for increment))
			 (message "Done Native Compiling %s in %s sec" package-name wait-time))))
		   package-names package-dirs)
	  (message "Done All Native Compiling in %s seconds using %s parallel cores"
		   wait-time-total native-comp-async-jobs-number))))))
  
(defun get-config-path (relative)
  (concat emacs-config-directory relative))


(defun all-packages-installed-p ()
  "Check if all third-party packages are installed."
  (cl-every (lambda (pkg-entry)
	      (package-installed-p (if (symbolp pkg-entry) pkg-entry (car pkg-entry))))
	    third-party-packages))


(defun setup-packages-and-customizations (config-dir)
  "Install and load packages and customizations based on environment variables."
  (let ((in-container skewed-emacs-container?))
    (package-initialize)
    ;; Only configure archives and refresh if third-party packages are missing
    (unless (all-packages-installed-p)
      (configure-package-archives)
      (when (or (not package-archive-contents)
                (not (file-exists-p (concat package-user-dir "/archives/gnu/archive-contents")))
                (not (file-exists-p (concat package-user-dir "/archives/nongnu/archive-contents")))
                (not (file-exists-p (concat package-user-dir "/archives/melpa/archive-contents")))))
      (package-refresh-contents))
    (dolist (pkg-entry third-party-packages)
      (if (symbolp pkg-entry)
	  (progn
            (eval `(use-package ,pkg-entry :ensure ,(not in-container)))
	    (eval `(require ',pkg-entry)))
	(progn
          (eval `(use-package ,(car pkg-entry) :ensure ,(not in-container) ,@(cdr pkg-entry)))
	  (eval `(require ',(car pkg-entry))))))
    (dolist (pkg-entry second-party-packages)
      (if (symbolp pkg-entry)
	  (progn
            (eval `(use-package ,pkg-entry :ensure ,(not in-container)))
	    (eval `(require ',pkg-entry)))
	(progn
          (eval `(use-package ,(car pkg-entry) :ensure ,(not in-container) ,@(cdr pkg-entry)))
	  (eval `(require ',(car pkg-entry))))))
    (when in-container 
      (setq native-comp-deferred-compilation nil)
      (setq no-native-compile t))))


(defun update-all-packages ()
  "Manually refresh package archives and update third-party packages."
  (interactive)
  (configure-package-and-install)
  (dolist (pkg-entry third-party-packages)
    (let ((pkg (if (symbolp pkg-entry) pkg-entry (car pkg-entry))))
      (when (package-installed-p pkg)
        (package-install pkg :upgrade t))))
  ;; Delete compilation flag to trigger recompilation on next startup
  (let ((compile-flag-file (concat emacs-config-directory ".compiled")))
    (when (file-exists-p compile-flag-file)
      (delete-file compile-flag-file)
      (message "Deleted %s to trigger recompilation on next startup" compile-flag-file))))



(defun clear-eln-cache ()
  "Clear the native compilation cache."
  (interactive)
  (when (file-exists-p comp-eln-cache-dir)
    (delete-directory comp-eln-cache-dir t)
    (message "Cleared native compilation cache")))
  
  

(provide 'load-and-compile)
;;; load-and-compile.el ends here

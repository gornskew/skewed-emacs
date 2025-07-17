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

(defvar third-party-packages
  '(flycheck
    company
    eat
    doom-themes
    zenburn-theme
    ellama
    chatgpt-shell
    json
    simple-httpd
    dashboard
    paredit
    
    (magit
     :config
     (setq magit-git-executable (locate-file "git" exec-path))
     (global-set-key (kbd "C-x g") 'magit-status))
    (pdf-tools
     :config
     (when (and (string= (getenv "EMACS_BATCH_MODE") "true")
		(not (string= (getenv "SKEWED_EMACS_CONTAINER") "true")))
       (pdf-tools-install)))
    (org
     :config
     (use-package org-config
       :ensure nil
       :load-path (lambda () (get-config-path "etc"))))
    (copilot
     :config
     (bind-key* "C-." 'copilot-accept-completion)
     (bind-key* "M-," 'copilot-accept-completion)
     (bind-key* "C-," 'copilot-accept-completion-by-word)))
    
  "List of third-party packages with their configurations. Each entry is either a symbol (for packages without config) or a list starting with the package name (a symbol) followed by optional :config forms.")


(defvar second-party-packages
  '((sa-translit
     :load-path (lambda () (get-config-path "etc")))
    (dashboard-config
     :load-path (lambda () (get-config-path "etc")))
    (impatient-markdown
     :load-path (lambda () (get-config-path "etc")))
;;    (slime-config
;;     :load-path (lambda () (get-config-path "etc"))
;;     :config
;;     (use-package configure-glime
;;       :ensure nil
;;       :load-path (lambda () (get-config-path "etc/slime")))
;;     (setq slime-lisp-host "gendl")
;;     (setq slime-connect-host-history '("gendl"))
;;     (setq slime-port 4200)
    ;;   (setq slime-connect-port-history '("4200")))
    ))



(setq package-check-signature nil) ; Avoid signature checks
(setq package-refresh-contents nil) ; Prevent automatic refresh

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

(defun batch-native-compile-directory (directory &optional cpu-count)
  "Byte-compile and natively compile .el files in DIRECTORY if needed.
Uses CPU-COUNT (or all available CPUs) for native compilation and waits for all processes to complete."
  (add-to-list 'load-path directory)
  (let ((el-files (directory-files-recursively
                   directory
                   "\\.el$"
                   nil
                   (lambda (dir) (not (string-match-p "\\.git" dir)))))
        (comp-files nil)
        (native-comp-async-jobs-number
         (or cpu-count
             (with-temp-buffer
               (call-process "nproc" nil t nil)
               (string-to-number (buffer-string))))))
    (message "Using %d CPUs for native compilation" native-comp-async-jobs-number)

    ;; Step 1: Byte-compile .el files if needed
    (dolist (file el-files)
      (let ((elc-file (concat file "c")))
        (when (or (not (file-exists-p elc-file))
                  (file-newer-than-file-p file elc-file))
          (condition-case err
              (progn
                (message "Byte-compiling %s" file)
                (byte-compile-file file))
            (error (message "Error byte-compiling %s: %s" file err))))))

    ;; Step 2: Collect files for native compilation if .eln is missing or outdated
    (dolist (file el-files)
      (let ((elc-file (concat file "c"))
            (eln-file (comp-el-to-eln-filename file)))
        (when (and (file-exists-p elc-file)
                   (or (not (file-exists-p eln-file))
                       (file-newer-than-file-p elc-file eln-file)))
          (push file comp-files))))

    ;; Step 3: Natively compile collected .elc files asynchronously
    (when comp-files
      (let ((comp-async-report-warnings-errors t))
        (message "Starting native compilation for %d files" (length comp-files))
        (native-compile-async comp-files t)
        ;; Wait for all native compilation jobs to finish
        (while (> (hash-table-count comp-async-compilations) 0)
          (message "Waiting for %d native compilation jobs to complete..."
                   (hash-table-count comp-async-compilations))
          (sleep-for 0.5))
        (message "Native compilation completed")))))

(defun batch-compile-all ()
  "Compile all .el files in CONFIG-DIR if necessary."
  (if (all-packages-installed-p)
      (message "Packages appear to be installed, not compiling all.%s" emacs-config-directory)
    (progn
      (message "Checking for compilation in %s" emacs-config-directory)
      (batch-native-compile-directory emacs-config-directory (max 1 (floor (/ comp-num-cpus 2)))))))


(defun batch-compile-all ()
  "Force-compile all .el files in CONFIG-DIR recursively in batch mode."
  (message "Batch compiling all .el files in %s" emacs-config-directory)
  (batch-native-compile-directory emacs-config-directory (max 1 (floor (/ comp-num-cpus 2)))))

(defun get-config-path (relative)
  (concat emacs-config-directory relative))


(defun all-packages-installed-p ()
  "Check if all third-party packages are installed."
  (cl-every (lambda (pkg-entry)
	      (package-installed-p (if (symbolp pkg-entry) pkg-entry (car pkg-entry))))
	    third-party-packages))


(defun setup-packages-and-customizations (config-dir)
  "Install and load packages and customizations based on environment variables."
  (let ((in-container (string= (getenv "SKEWED_EMACS_CONTAINER") "true"))
        (batch-mode (string= (getenv "EMACS_BATCH_MODE") "true")))
    (if in-container
        (progn
          (message "Running in container, skipping package installation")
          (dolist (pkg-entry second-party-packages)
            (let ((load-path-func (plist-get (cdr pkg-entry) :load-path)))
              (when load-path-func
                (add-to-list 'load-path (funcall load-path-func))))))
      (progn
        ;; Only configure archives and refresh if third-party packages are missing
        (unless (all-packages-installed-p)
          (configure-package-archives)
          (package-initialize)
          (when (or (not package-archive-contents)
                    (not (file-exists-p (concat package-user-dir "/archives/gnu/archive-contents")))
                    (not (file-exists-p (concat package-user-dir "/archives/nongnu/archive-contents")))
                    (not (file-exists-p (concat package-user-dir "/archives/melpa/archive-contents"))))
            (package-refresh-contents)))
        ;; Load third-party packages
        (dolist (pkg-entry third-party-packages)
          (if (symbolp pkg-entry)
              (eval `(use-package ,pkg-entry))
            (eval `(use-package ,(car pkg-entry) ,@(cdr pkg-entry)))))
        ;; Load second-party packages
        (dolist (pkg-entry second-party-packages)
          (if (symbolp pkg-entry)
              (eval `(use-package ,pkg-entry :ensure nil))
            (eval `(use-package ,(car pkg-entry) :ensure nil ,@(cdr pkg-entry)))))
        ;; Enable deferred compilation in interactive mode
        (unless (or in-container batch-mode)
          (setq native-comp-deferred-compilation t)
          (setq native-comp-async-jobs-number 2))))))


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

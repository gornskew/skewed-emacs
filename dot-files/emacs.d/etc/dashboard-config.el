;;; Dashboard setup 
(require 'url)

(defvar skewed-dashboard-banner-file (concat (temporary-file-directory) "skewed-emacs-banner.txt"))
(dashboard-setup-startup-hook)
(setq dashboard-banner-logo-title "𝑺𝑲𝑬𝑾𝑬𝑫 𝑬𝑴𝑨𝑪𝑺")

(setq dashboard-startup-banner skewed-dashboard-banner-file)
(setq dashboard-items '((help . 3)
			(recents  . 3)
                        ;;(active-projects . 5)
			;;(lisply-status . 1)
                        (system-info . 1)       
                        ;;(other-status . 1)
                        (agenda . 1)
                        (bookmarks . 1)))

(setq dashboard-center-content t)
(setq dashboard-footer-messages '("Brought to you by 𝙶𝚘𝚛𝚗𝚜𝚔𝚎𝚠 𝙴𝚗𝚝𝚎𝚛𝚙𝚛𝚒𝚜𝚎𝚜"))

(setq initial-buffer-choice
      (lambda ()
	(dashboard-refresh-buffer)
	(get-buffer-create "*dashboard*")))

(defun get-directory-mtime (dir)
  "Get the most recent modification time of any file in directory tree."
  (let ((max-time '(0 0)))
    (when (file-directory-p dir)
      (condition-case nil
        (dolist (file (directory-files-recursively dir "." t))
          (when (and (file-regular-p file) 
                     (not (string-match "\\.git/" file)))
            (let ((file-time (file-attribute-modification-time (file-attributes file))))
              (when (time-less-p max-time file-time)
                (setq max-time file-time)))))
        (error nil)))
    max-time))



;;
;; FLAG -- deal with cross-container host/port scenarios. 
;;
;; FLAG -- drive hostnames and ports with environment vars. 
;;
;;
(defun lisply-backend-status-string-uncached ()
  "Return backend status string without any side effects.
No buffers created, no messages shown, no slowdowns."
  (with-output-to-string 
    (princ "Lisply Backend Health:\n")
    (dolist (backend '((:host "gendl" :port 9080)
                       (:host "skewed-emacs" :port 7080)))
      (cl-destructuring-bind (&key host port) backend
        (let ((result (silent-http-ping host port "/lisply/ping-lisp" 1)))
          (let ((status (plist-get result :status))
                (time (plist-get result :time)))
            (princ (format "    [%s] %s:%s%s\n" 
                          (if (string= status "OK") "OK" "!DN!")
                          host port
                          (if (string= status "OK") 
                              (format " (%s)" (or time "?ms"))
                            "")))))))))

(let ((lisply-backend-status-cache nil)
      lisply-backend-status-cache-timeout 5)
  (defun lisply-backend-status-string ()
    "Return backend status string with caching to avoid repeated network calls."
    (let ((now (current-time)))
      ;; Check if cache is valid (within timeout period)
      (if (and lisply-backend-status-cache
               (plist-get lisply-backend-status-cache :timestamp)
               (< (float-time (time-subtract now (plist-get lisply-backend-status-cache :timestamp)))
                  lisply-backend-status-cache-timeout))
          ;; Return cached result
          (plist-get lisply-backend-status-cache :result)
	;; Generate new result and cache it
	(let ((result (lisply-backend-status-string-uncached)))
          (setq lisply-backend-status-cache 
		(list :timestamp now :result result))
          result)))))

			      
(defun dashboard-insert-lisply-backends (list-size)
  "Insert Lisply Section"
  (insert (lisply-backend-status-string)))


(defun gather-system-info ()
  (list :emacs-pid (emacs-pid)
	:current-time (current-time-string)
	:active-packages (length package-activated-list)
	:active-buffers (length (buffer-list))
	:emacs-version emacs-version
	:system-type system-type
	:working-dir default-directory
	:active-projects (let ((project-dirs (seq-filter (lambda (dir) 
							   (and (file-directory-p dir)
								(file-exists-p (expand-file-name ".git" dir))))
							 (directory-files "/projects" t "^[^.]"))))
			   (when project-dirs
			     (mapcar (lambda (proj)
				       (format "  %s " (file-name-nondirectory proj)))
				     (seq-take project-dirs 8))))
	:lisply-backends '((:name "skewed-emacs" :port 7080 :endpoint "/lisply/lisp-eval")
			(:name "gendl" :port 9080 :endpoint "/lisply/lisp-eval"))
	:quick-actions '((:key "[C-c d r]" :action  "Refresh Dashboard")
			 (:Key "[C-c d d]" :action "Open Dashboard")
			 (:key "[C-c d a]" :action "Toggle Auto-refresh"))))


(defun dashboard-insert-system-info (list-size)
  "Insert system information section."
  (dashboard-insert-heading "System Information:")
  (insert "\n")
  (let ((system-info (gather-system-info)))
    (insert (format "    Emacs PID: %s | Uptime: %s\n" 
                    (plist-get system-info :emacs-pid)
                    (emacs-uptime)))
    (insert (format "    Packages: %d active | Buffers: %d open\n"
                    (plist-get system-info :active-packages)
                    (plist-get system-info :active-buffers)))
    (insert (format "    Version: %s | Platform: %s\n"
                    (plist-get system-info :emacs-version)
                    (plist-get system-info :system-type)))
    (insert (format "    Time: %s\n"
                    (plist-get system-info :current-time)))))

(defun dashboard-insert-help-info (list-size)
  (dashboard-insert-heading "Getting Started:")
  (insert "\n")
  (insert "• Emacs Tutorial: C-h C-t\n")
  (insert "• Gendl Repl: M-x slime-connect RET\n")
  (insert "• Claude Code: M-x eat, then `claudly`\n"))

(defun dashboard-insert-lisply-status (list-size)
  "Insert LISPLY services status section with live probing."
  (dashboard-insert-heading "Lisply MCP Backends:")
  (insert "\n")
  (let ((test-results (test-lisply-backends)))
    (let ((emacs-status (plist-get test-results :skewed-emacs))
          (gendl-status (plist-get test-results :gendl)))
      (insert (format "    %s skewed-emacs:7080 (%s)\n" 
                      (plist-get emacs-status :status)
                      (plist-get emacs-status :note)))
      (insert (format "    %s gendl:9080 (%s)\n" 
                      (plist-get gendl-status :status)
                      (plist-get gendl-status :note))))))

(defun dashboard-insert-other-status (list-size)
  "Insert other services status section."
  (dashboard-insert-heading "Other Services:")
  (insert "\n")
  (insert "    〰 SLIME: gendl-ccl:4200\n")    
  (insert "    ⚏  Docker Network: emacs-gendl-network\n"))

(defun dashboard-insert-active-projects (list-size)
  "Insert active projects from /projects directory, sorted by most recently modified content."
  (dashboard-insert-heading "Active Projects:")
  (insert "\n")
  (let* ((project-dirs (seq-filter (lambda (dir) 
                                    (and (file-directory-p (expand-file-name dir "/projects"))
                                         (file-exists-p (expand-file-name (concat dir "/.git") "/projects"))))
                                  (directory-files "/projects" nil "^[^.]")))
         ;; Add modification times and sort by most recent first
         (projects-with-mtime (mapcar (lambda (proj)
                                       (let ((full-path (expand-file-name proj "/projects")))
                                         (cons proj (get-directory-mtime full-path))))
                                     project-dirs))
         (sorted-projects (sort projects-with-mtime 
                               (lambda (a b) 
                                 (time-less-p (cdr b) (cdr a)))))
         (display-projects (seq-take (mapcar #'car sorted-projects) (or list-size 8))))
    (if display-projects
        (dolist (proj display-projects)
          (insert (format "    %s [git]\n" proj)))
      (insert "    --- No projects detected ---\n"))))

;; Register custom dashboard generators
(setq dashboard-item-generators
      (append '((help . dashboard-insert-help-info)
		(system-info . dashboard-insert-system-info)
                (lisply-status . dashboard-insert-lisply-backends)
		(other-status . dashboard-insert-other-status)
                (active-projects . dashboard-insert-active-projects))
              (cl-remove-if (lambda (item) 
                              (memq (car item) '(system-info lisply-status active-projects projects)))
                            dashboard-item-generators)))


(defun silent-http-ping (host port endpoint &optional timeout)
  "Ping HTTP endpoint silently without any side effects or messages.
Returns (:status OK|ERROR :time response-time-ms)."
  (let ((url (format "http://%s:%d%s" host port (or endpoint "/")))
        (url-request-timeout (or timeout 2))
        (start-time (current-time))
        (inhibit-message t)          ; Suppress all messages
        (message-log-max nil)        ; Don't log messages
        (url-show-status nil)        ; Don't show URL status
        (url-automatic-caching nil)  ; Disable caching
        (url-debug nil))             ; Disable debug output
    (condition-case err
        (let ((buffer (url-retrieve-synchronously url nil t timeout)))
          (if buffer
              (unwind-protect
                  (with-current-buffer buffer
                    (let ((response-time (float-time (time-subtract (current-time) start-time))))
                      (goto-char (point-min))
                      (if (re-search-forward "HTTP/[0-9]\\.[0-9] \\([0-9]+\\)" nil t)
                          (let ((status-code (string-to-number (match-string 1))))
                            (list :status (if (< status-code 400) "OK" "ERROR")
                                  :code status-code
                                  :time (format "%.0fms" (* response-time 1000))))
                        (list :status "ERROR" :error "No HTTP response"))))
                ;; Always clean up the buffer
                (kill-buffer buffer))
            (list :status "ERROR" :error "No response")))
      (error (list :status "ERROR" :error (error-message-string err))))))



(defvar skewed-dashboard-banners
  '(:blur-vision
    "
 ░▒▓███████▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓████████▓▒░▒▓███████▓▒░  
░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░ 
░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░ 
 ░▒▓██████▓▒░░▒▓███████▓▒░░▒▓██████▓▒░ ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓██████▓▒░ ░▒▓█▓▒░░▒▓█▓▒░ 
       ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░ 
       ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░ 
░▒▓███████▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓████████▓▒░░▒▓█████████████▓▒░░▒▓████████▓▒░▒▓███████▓▒░  
                                                                                       
                                                                                       
      ░▒▓████████▓▒░▒▓██████████████▓▒░ ░▒▓██████▓▒░ ░▒▓██████▓▒░ ░▒▓███████▓▒░        
      ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░               
      ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░               
      ░▒▓██████▓▒░ ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓████████▓▒░▒▓█▓▒░       ░▒▓██████▓▒░         
      ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░             ░▒▓█▓▒░        
      ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░      ░▒▓█▓▒░        
      ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░░▒▓██████▓▒░░▒▓███████▓▒░        
"
    :big-money-ne
    (:skewed
     "
  /$$$$$$  /$$   /$$ /$$$$$$$$ /$$      /$$ /$$$$$$$$ /$$$$$$$ 
 /$$__  $$| $$  /$$/| $$_____/| $$  /$ | $$| $$_____/| $$__  $$
| $$  \\__/| $$ /$$/ | $$      | $$ /$$$| $$| $$      | $$  \\ $$
|  $$$$$$ | $$$$$/  | $$$$$   | $$/$$ $$ $$| $$$$$   | $$  | $$
 \\____  $$| $$  $$  | $$__/   | $$$$_  $$$$| $$__/   | $$  | $$
 /$$  \\ $$| $$\\  $$ | $$      | $$$/ \\  $$$| $$      | $$  | $$
|  $$$$$$/| $$ \\  $$| $$$$$$$$| $$/   \\  $$| $$$$$$$$| $$$$$$$/
 \\______/ |__/  \\__/|________/|__/     \\__/|________/|_______/
"
     :emacs
     "
       /$$$$$$$$ /$$      /$$  /$$$$$$   /$$$$$$   /$$$$$$     
      | $$_____/| $$$    /$$$ /$$__  $$ /$$__  $$ /$$__  $$    
      | $$      | $$$$  /$$$$| $$  \\ $$| $$  \\__/| $$  \\__/    
      | $$$$$   | $$ $$/$$ $$| $$$$$$$$| $$      |  $$$$$$     
      | $$__/   | $$  $$$| $$| $$__  $$| $$       \\____  $$    
      | $$      | $$\\  $ | $$| $$  | $$| $$    $$ /$$  \\ $$    
      | $$$$$$$$| $$ \\/  | $$| $$  | $$|  $$$$$$/|  $$$$$$/    
      |________/|__/     |__/|__/  |__/ \\______/  \\______/
")
    
    :big-money-nw
    (:skewed
     "
 $$$$$$\\  $$\\   $$\\ $$$$$$$$\\ $$\\      $$\\ $$$$$$$$\\ $$$$$$$\\  
$$  __$$\\ $$ | $$  |$$  _____|$$ | $\\  $$ |$$  _____|$$  __$$\\ 
$$ /  \\__|$$ |$$  / $$ |      $$ |$$$\\ $$ |$$ |      $$ |  $$ |
\\$$$$$$\\  $$$$$  /  $$$$$\\    $$ $$ $$\\$$ |$$$$$\\    $$ |  $$ |
 \\____$$\\ $$  $$<   $$  __|   $$$$  _$$$$ |$$  __|   $$ |  $$ |
$$\\   $$ |$$ |\\$$\\  $$ |      $$$  / \\$$$ |$$ |      $$ |  $$ |
\\$$$$$$  |$$ | \\$$\\ $$$$$$$$\\ $$  /   \\$$ |$$$$$$$$\\ $$$$$$$  |
 \\______/ \\__|  \\__|\\________|\\__/     \\__|\\________|\\_______/ 
"
     :emacs
     "
      $$$$$$$$\\ $$\\      $$\\  $$$$$$\\   $$$$$$\\   $$$$$$\\      
      $$  _____|$$$\\    $$$ |$$  __$$\\ $$  __$$\\ $$  __$$\\     
      $$ |      $$$$\\  $$$$ |$$ /  $$ |$$ /  \\__|$$ /  \\__|    
      $$$$$\\    $$\\$$\\$$ $$ |$$$$$$$$ |$$ |      \\$$$$$$\\      
      $$  __|   $$ \\$$$  $$ |$$  __$$ |$$ |       \\____$$\\     
      $$ |      $$ |\\$  /$$ |$$ |  $$ |$$ |  $$\\ $$\\   $$ |    
      $$$$$$$$\\ $$ | \\_/ $$ |$$ |  $$ |\\$$$$$$  |\\$$$$$$  |    
      \\________|\\__|     \\__|\\__|  \\__| \\______/  \\______/    
")
    :big-money-se
    (:skewed
    "
  ______   __    __  ________  __       __  ________  _______  
 /      \\ |  \\  /  \\|        \\|  \\  _  |  \\|        \\|       \\ 
|  $$$$$$\\| $$ /  $$| $$$$$$$$| $$ / \\ | $$| $$$$$$$$| $$$$$$$\\
| $$___\\$$| $$/  $$ | $$__    | $$/  $\\| $$| $$__    | $$  | $$
 \\$$    \\ | $$  $$  | $$  \\   | $$  $$$\\ $$| $$  \\   | $$  | $$
 _\\$$$$$$\\| $$$$$\\  | $$$$$   | $$ $$\\$$\\$$| $$$$$   | $$  | $$
|  \\__| $$| $$ \\$$\\ | $$_____ | $$$$  \\$$$$| $$_____ | $$__/ $$
 \\$$    $$| $$  \\$$\\| $$     \\| $$$    \\$$$| $$     \\| $$    $$
  \\$$$$$$  \\$$   \\$$ \\$$$$$$$$ \\$$      \\$$ \\$$$$$$$$ \\$$$$$$$
"
    :emacs
    "
       ________  __       __   ______    ______    ______      
      |        \\|  \\     /  \\ /      \\  /      \\  /      \\     
      | $$$$$$$$| $$\\   /  $$|  $$$$$$\\|  $$$$$$\\|  $$$$$$\\    
      | $$__    | $$$\\ /  $$$| $$__| $$| $$   \\$$| $$___\\$$    
      | $$  \\   | $$$$\\  $$$$| $$    $$| $$       \\$$    \\     
      | $$$$$   | $$\\$$ $$ $$| $$$$$$$$| $$   __  _\\$$$$$$\\    
      | $$_____ | $$ \\$$$| $$| $$  | $$| $$__/  \\|  \\__| $$    
      | $$     \\| $$  \\$ | $$| $$  | $$ \\$$    $$ \\$$    $$    
       \\$$$$$$$$ \\$$      \\$$ \\$$   \\$$  \\$$$$$$   \\$$$$$$
")
    
    :big-money-sw
    (:skewed
     "
  ______   __    __  ________  __       __  ________  _______  
 /      \\ /  |  /  |/        |/  |  _  /  |/        |/       \\ 
/$$$$$$  |$$ | /$$/ $$$$$$$$/ $$ | / \\ $$ |$$$$$$$$/ $$$$$$$  |
$$ \\__$$/ $$ |/$$/  $$ |__    $$ |/$  \\$$ |$$ |__    $$ |  $$ |
$$      \\ $$  $$<   $$    |   $$ /$$$  $$ |$$    |   $$ |  $$ |
 $$$$$$  |$$$$$  \\  $$$$$/    $$ $$/$$ $$ |$$$$$/    $$ |  $$ |
/  \\__$$ |$$ |$$  \\ $$ |_____ $$$$/  $$$$ |$$ |_____ $$ |__$$ |
$$    $$/ $$ | $$  |$$       |$$$/    $$$ |$$       |$$    $$/ 
 $$$$$$/  $$/   $$/ $$$$$$$$/ $$/      $$/ $$$$$$$$/ $$$$$$$/
"
     :emacs
     "
       ________  __       __   ______    ______    ______      
      /        |/  \\     /  | /      \\  /      \\  /      \\     
      $$$$$$$$/ $$  \\   /$$ |/$$$$$$  |/$$$$$$  |/$$$$$$  |    
      $$ |__    $$$  \\ /$$$ |$$ |__$$ |$$ |  $$/ $$ \\__$$/     
      $$    |   $$$$  /$$$$ |$$    $$ |$$ |      $$      \\     
      $$$$$/    $$ $$ $$/$$ |$$$$$$$$ |$$ |   __  $$$$$$  |    
      $$ |_____ $$ |$$$/ $$ |$$ |  $$ |$$ \\__/  |/  \\__$$ |    
      $$       |$$ | $/  $$ |$$ |  $$ |$$    $$/ $$    $$/     
      $$$$$$$$/ $$/      $$/ $$/   $$/  $$$$$$/   $$$$$$/      
")))



(defun generate-skewed-dashboard-banner (&optional text-file)
  (unless text-file (setq text-file skewed-dashboard-banner-file))
  (let ((skewed-banner (plist-get
			(plist-get skewed-dashboard-banners :big-money-sw) :skewed))
        (emacs-banner (plist-get
		       (plist-get skewed-dashboard-banners :big-money-se) :emacs)))

    (write-region (concat skewed-banner emacs-banner) nil text-file nil nil nil nil)
    ))


(provide 'dashboard-config)

;; ;; Dashboard Configuration Changes
;; ;; Made on Sat Jul 19 13:31:55 2025
;; ;; 
;; ;; Changes made:
;; ;; 1. Fixed residual HTTP ping messages in minibuffer
;; ;; 2. Added refresh completion message with timestamp and timing
;; ;; 3. Added 'last touched' timestamps to Active Projects

;; ;;; Updated simple-http-ping function (suppresses messages)
;; (defun simple-http-ping (url &optional timeout)
;;   "Simple HTTP ping using built-in Emacs url functions, with suppressed messages."
;;   (let ((url-request-timeout (or timeout 3))
;;         (start-time (current-time))
;;         (inhibit-message t)  ; Suppress messages
;;         (message-log-max nil)) ; Don't log messages
;;     (condition-case err
;;         (with-current-buffer (url-retrieve-synchronously url nil nil timeout)
;;           (let ((response-time (float-time (time-subtract (current-time) start-time))))
;;             (goto-char (point-min))
;;             (if (re-search-forward "HTTP/[0-9]\\.[0-9] \\([0-9]+\\)" nil t)
;;                 (let ((status-code (string-to-number (match-string 1))))
;;                   (search-forward "\n\n" nil t)
;;                   (let ((body (string-trim (buffer-substring (point) (point-max)))))
;;                     (kill-buffer)
;;                     (list :status (if (< status-code 400) "OK" "ERROR")
;;                           :code status-code
;;                           :time (format "%.0fms" (* response-time 1000))
;;                           :body body
;;                           :url url)))
;;               (kill-buffer)
;;               (list :status "ERROR" :error "No HTTP response" :url url))))
;;       (error (list :status "ERROR" 
;;                    :error (error-message-string err)
;;                    :url url)))))

;; ;;; Updated dashboard-insert-active-projects (adds timestamps)
;; (defun dashboard-insert-active-projects (list-size)
;;   "Insert active projects from /projects directory, sorted by most recently modified content, with timestamps."
;;   (dashboard-insert-heading "Active Projects:")
;;   (insert "\n")
;;   (let* ((project-dirs (seq-filter (lambda (dir) 
;;                                     (and (file-directory-p (expand-file-name dir "/projects"))
;;                                          (file-exists-p (expand-file-name (concat dir "/.git") "/projects"))))
;;                                   (directory-files "/projects" nil "^[^.]")))
;;          ;; Add modification times and sort by most recent first
;;          (projects-with-mtime (mapcar (lambda (proj)
;;                                        (let ((full-path (expand-file-name proj "/projects")))
;;                                          (cons proj (get-directory-mtime full-path))))
;;                                      project-dirs))
;;          (sorted-projects (sort projects-with-mtime 
;;                                (lambda (a b) 
;;                                  (time-less-p (cdr b) (cdr a)))))
;;          (display-projects (seq-take sorted-projects (or list-size 8))))
;;     (if display-projects
;;         (dolist (proj-time-pair display-projects)
;;           (let* ((proj (car proj-time-pair))
;;                  (mtime (cdr proj-time-pair))
;;                  (time-ago (if (and mtime (not (equal mtime '(0 0))))
;;                               (let ((seconds-ago (float-time (time-subtract (current-time) mtime))))
;;                                 (cond
;;                                  ((< seconds-ago 3600) (format "%.0fm ago" (/ seconds-ago 60)))
;;                                  ((< seconds-ago 86400) (format "%.1fh ago" (/ seconds-ago 3600)))
;;                                  ((< seconds-ago 604800) (format "%.1fd ago" (/ seconds-ago 86400)))
;;                                  (t (format-time-string "%Y-%m-%d" mtime))))
;;                             "unknown")))
;;             (insert (format "    %s [git] - %s\n" proj time-ago))))
;;       (insert "    --- No projects detected ---\n"))))

;; ;;; Advice to add timing message to dashboard refresh
;; (advice-add 'dashboard-refresh-buffer :around 
;;             (lambda (orig-fun &rest args)
;;               "Add timing and clean messaging to dashboard refresh."
;;               (let ((start-time (current-time))
;;                     (inhibit-message t)  ; Suppress intermediate messages
;;                     (message-log-max nil))
;;                 (apply orig-fun args)
;;                 (let* ((end-time (current-time))
;;                        (elapsed (float-time (time-subtract end-time start-time)))
;;                        (timestamp (format-time-string "%H:%M:%S")))
;;                   (message "Dashboard contents refreshed at %s in %.2f seconds." timestamp elapsed)))))

;; ;; Optional: Standalone refresh function with messaging
;; (defun dashboard-refresh-with-message ()
;;   "Refresh dashboard and show completion message with timing."
;;   (interactive)
;;   (let ((start-time (current-time))
;;         (inhibit-message t)  ; Suppress intermediate messages
;;         (message-log-max nil))
;;     (dashboard-refresh-buffer)
;;     (let* ((end-time (current-time))
;;            (elapsed (float-time (time-subtract end-time start-time)))
;;            (timestamp (format-time-string "%H:%M:%S")))
;;       (message "Dashboard contents refreshed at %s in %.2f seconds." timestamp elapsed))))

;; ;; End of changes

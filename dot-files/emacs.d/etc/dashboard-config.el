;;; Dashboard Config --- splashy dash  -*- lexical-binding: nil -*-
;;; Dashboard setup

;; To Do:
;;
;;
;;

(require 'subr-x)
(require 'url)
(require 'dashboard)
(require 'dashboard-additions)
(require 'cl-lib)

;;; Code:

(defvar skewed-emacs-container? nil) ;; assumed to be pre-defined from init.el
(defvar skewed-dashboard-banner-file (concat (temporary-file-directory) "skewed-emacs-banner.txt"))

(defvar projects-dir
  (expand-file-name
   (if (or skewed-emacs-container? skewed-emacs-docker-build?
	   (file-exists-p "/projects/"))
       "/projects/" "~/projects/")))

(dashboard-setup-startup-hook)
(setq dashboard-banner-logo-title "𝑺𝑲𝑬𝑾𝑬𝑫 𝑬𝑴𝑨𝑪𝑺")

(setq dashboard-startup-banner skewed-dashboard-banner-file)


;;(setq dashboard-items nil)

(setq dashboard-items `((help . 3)
 			;;(recents  . 3) replace this with one we control. 
 			(active-projects . 5)
			(other-status . 1)
 			(lisply-status . 1)
 			(system-info . 1)
 			(agenda . 1)
			))


(setq dashboard-item-generators
      (append '((help . dashboard-insert-help-info)
		(active-projects . dashboard-insert-active-projects)
		(lisply-status . dashboard-insert-lisply-backends)
 		(system-info . dashboard-insert-system-info)
 		(other-status . dashboard-insert-other-status))
              (cl-remove-if (lambda (item) 
                              (memq (car item) '(system-info lisply-status active-projects projects)))
                            dashboard-item-generators)))

(setq dashboard-center-content t)
(setq dashboard-footer-messages '("Brought to you by 𝙶𝚘𝚛𝚗𝚜𝚔𝚎𝚠 𝙴𝚗𝚝𝚎𝚛𝚙𝚛𝚒𝚜𝚎𝚜"))

(setq initial-buffer-choice
      (lambda ()
	(dashboard-refresh-buffer)
	(get-buffer-create "*dashboard*")))


(defun dashboard-insert-help-info (list-size)
  "Insert getting-started info. LIST-SIZE nil to leave blank."
  (if list-size
      (progn
        (dashboard-insert-heading "Getting Started:")
        (insert "\n")
        (dolist (line (help-info-strings))
          (insert line)))
    (dashboard-insert-heading "No Help for You")))

(defun dashboard-insert-active-projects (list-size)
  "Insert active projects info.  LIST-SIZE is passed along."
  (dashboard-insert-heading (format "Active Projects in %s:" projects-dir))
  (insert "\n")
  (dolist (proj-string (active-projects-strings list-size))
    (insert proj-string)))

(defun dashboard-insert-system-info (list-size)
    "Insert system information section.  LIST-SIZE nil to leave blank."
    (if list-size
	(progn
	  (dashboard-insert-heading "System Information:")
	  (insert "\n")
	  (insert (system-info-string)))
      (dashboard-insert-heading "No System Info for You")))

;;
;; FLAG - get more sophisticated for determining live services for
;; standalone runnings.
;;
(defun dashboard-insert-lisply-backends (list-size)
  "Insert precooked Lisply string or not.  LIST-SIZE not currently being respected."
  (if (and skewed-emacs-container? list-size)
      (progn
	(dashboard-insert-heading "Live Lisply Backends:")
	(insert "\n")
	(insert (lisply-backends-string)))
    (dashboard-insert-heading "No Configured Lisply Backends")
    (insert "\n")
    (insert "     𝙰𝚝 𝚃𝚑𝚒𝚜 𝙹𝚞𝚗𝚌𝚝𝚞𝚛𝚎")))



;;
;; Add back the use of this later
;;
;;(defun dashboard-insert-other-status (list-size)
;;  "Insert other services status section."
;;  (message "should limit list to %s" list-size)
;;  (dashboard-insert-heading "Other Services:")
;;  (insert "\n")
;;  (insert "    〰 SLIME: gendl-ccl:4200\n")    
;;  (insert "    ⚏  Docker Network: emacs-gendl-network\n"))
;;
;; Helper functions below. 
;;

(defun silent-http-ping (host port endpoint &optional timeout)
  "Ping HTTP ENDPOINT on HOST at PORT silently.
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
        (let ((buffer (url-retrieve-synchronously url nil t url-request-timeout)))
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


(defun get-emacs-memory-usage ()
  "Get current Emacs process memory usage in MB."
  (let ((status-file (format "/proc/%d/status" (emacs-pid))))
    (when (file-exists-p status-file)
      (with-temp-buffer
        (insert-file-contents status-file)
        (goto-char (point-min))
        ;; Look for VmRSS (Resident Set Size) - this is like RES in htop
        (when (re-search-forward "^VmRSS:[[:space:]]+\\([0-9]+\\)[[:space:]]+kB" nil t)
          (let ((rss-kb (string-to-number (match-string 1))))
            (/ rss-kb 1024.0)))))))

(defun get-emacs-cpu-time ()
  "Get total CPU time consumed by Emacs process in seconds."
  (let ((stat-file (format "/proc/%d/stat" (emacs-pid))))
    (when (file-exists-p stat-file)
      (with-temp-buffer
        (insert-file-contents stat-file)
        (goto-char (point-min))
        ;; /proc/pid/stat format: pid comm state ppid ... utime stime ...
        ;; Fields 14 and 15 are utime and stime (in clock ticks)
        (let ((fields (split-string (buffer-string))))
          (when (>= (length fields) 15)
            (let ((utime (string-to-number (nth 13 fields)))  ; user time
                  (stime (string-to-number (nth 14 fields)))  ; system time
                  (clock-ticks-per-sec 100)) ; typically 100 on Linux
              (/ (+ utime stime) clock-ticks-per-sec))))))))


(defun visible-buffer-list ()
  "Return a list of open, non-internal buffers as seen in `electric-buffer-list`."
  (seq-filter (lambda (buf)
                (not (string-prefix-p " " (buffer-name buf))))
              (buffer-list)))

(defun gather-system-info ()
  "System info gathering with memory and CPU usage."
  (let ((memory-mb (get-emacs-memory-usage))
        (cpu-seconds (get-emacs-cpu-time)))
    (list :emacs-pid (emacs-pid)
          :current-time (current-time-string)
          :active-packages (length package-activated-list)
          :visible-buffers (length (visible-buffer-list))
	  :total-buffers (length (buffer-list))
          :emacs-version emacs-version
          :system-type system-type
          :working-dir default-directory
          :memory-mb memory-mb
          :cpu-seconds cpu-seconds
          :lisply-backends '((:name "skewed-emacs" :port 7080 :endpoint "/lisply/lisp-eval")
                           (:name "gendl" :port 9080 :endpoint "/lisply/lisp-eval"))
          :quick-actions '((:key "[C-c d r]" :action  "Refresh Dashboard")
                         (:Key "[C-c d d]" :action "Open Dashboard")
                         (:key "[C-c d a]" :action "Toggle Auto-refresh")))))


(defun get-simple-directory-mtime (dir)
  "Get directory modification time of DIR (when files added/removed)."
  (file-attribute-modification-time (file-attributes dir)))

(defun get-git-recent-activity (dir)
  "Get most recent git commit time of DIR."
  (when (file-exists-p (expand-file-name ".git" dir))
    (let ((default-directory dir))
      (condition-case nil
          (let ((output (shell-command-to-string "git log -1 --format=%ct")))
            (when (string-match "^[0-9]+" output)
              (seconds-to-time (string-to-number (match-string 0 output)))))
        (error nil)))))

(defvar project-mtime-cache (make-hash-table :test 'equal))
(defvar project-cache-timeout 30)

(defun get-cached-directory-mtime (dir strategy-function)
  "Get directory mtime with caching of DIR using STRATEGY-FUNCTION."
  (let* ((cache-key dir)
         (cached-entry (gethash cache-key project-mtime-cache))
         (now (current-time)))
    (if (and cached-entry
             (< (float-time (time-subtract now (plist-get cached-entry :timestamp)))
                project-cache-timeout))
        (plist-get cached-entry :mtime)
      (let ((new-mtime (funcall strategy-function dir)))
        (puthash cache-key (list :mtime new-mtime :timestamp now) project-mtime-cache)
        new-mtime))))

(defun help-info-strings ()
  "Return a list of propertized strings for help dashboard item."
  (list
   (propertize "• Emacs Tutorial: C-h C-t\n"
               'keymap (let ((map (make-sparse-keymap)))
                         (define-key map (kbd "RET") 'help-with-tutorial)
                         (define-key map [mouse-1] 'help-with-tutorial)
                         map)
               'face 'button
               'help-echo "Run Emacs tutorial (C-h C-t)")
   (propertize "• Gendl Repl: M-x slime-connect RET\n"
               'keymap (let ((map (make-sparse-keymap)))
                         (define-key map (kbd "RET") 'slime-connect)
                         (define-key map [mouse-1] 'slime-connect)
                         map)
               'face 'button
               'help-echo "Connect to Gendl REPL (slime-connect)")
   (propertize "• Claude Code: M-x eat, then `claudly`\n"
               'keymap (let ((map (make-sparse-keymap))
			     (function (lambda () (interactive)
					 (eat))))
                         (define-key map (kbd "RET") function)
                         (define-key map [mouse-1] function)
                         map)
               'face 'button
               'help-echo "Run eat and insert 'claudly'")
   (propertize "• M-x light-theme, dark-theme, load-theme\n"
               'keymap (let ((map (make-sparse-keymap)))
                         (define-key map (kbd "RET") 'load-theme)
                         (define-key map [mouse-1] 'load-theme)
                         map)
               'face 'button
               'help-echo "Run load-theme")))


;; (defun help-info-string ()
;;   "Return a string suitable for help dashboad item."
;;   (with-output-to-string 
;;    (princ (format "• Emacs Tutorial: C-h C-t\n"))
;;    (princ (format "• Gendl Repl: M-x slime-connect RET\n"))
;;    (princ (format "• Claude Code: M-x eat, then `claudly`\n"))
;;    (princ (format "• M-x light-theme, dark-theme, load-theme\n"))
;;    ))


(defvar lisply-backends-cache-timeout 5)
(defvar lisply-backends-cache nil)
(defun lisply-backends-string ()
  "Return backend status string with caching."
  (let ((now (current-time)))
    ;; Check if cache is valid (within timeout period)
    (if (and lisply-backends-cache
             (plist-get lisply-backends-cache :timestamp)
             (< (float-time (time-subtract now (plist-get lisply-backends-cache :timestamp)))
                lisply-backends-cache-timeout))
        ;; Return cached result
        (plist-get lisply-backends-cache :result)
      ;; Generate new result and cache it
      (let ((result (lisply-backends-string-uncached)))
        (setq lisply-backends-cache 
	      (list :timestamp now :result result))
        result))))

;;
;; FLAG -- deal with cross-container host/port scenarios. 
;;
;; FLAG -- drive hostnames and ports with environment vars. 
;;
;;
(defun lisply-backends-string-uncached ()
  "Return backend status string without any side effects.
No buffers created, no messages shown, no slowdowns."
  (with-output-to-string 
    (dolist (backend (discover-network-lisply-backends))
      (cl-destructuring-bind (&key host port) backend
        (let ((result (silent-http-ping host port "/lisply/ping-lisp" 0.3)))
          (let ((status (plist-get result :status))
                (time (plist-get result :time)))
            (princ (format "    [%s] %s:%s%s\n" 
                          (if (string= status "OK") "OK" "!DN!")
                          host port
                          (if (string= status "OK") 
                              (format " (%s)" (or time "?ms"))
                            "")))))))))
			      
(defun active-projects-strings (list-size)
  "Return a list of propertized strings for active projects."
  (let* ((project-dirs
          (when (file-exists-p projects-dir)
            (seq-filter
             (lambda (dir)
               (and (file-directory-p (expand-file-name dir projects-dir))
                    (file-exists-p
                     (expand-file-name (concat dir "/.git") projects-dir))))
             (directory-files projects-dir nil "^[^.]"))))
         (projects-with-mtime
          (mapcar
           (lambda (proj)
             (let* ((full-path (expand-file-name proj projects-dir))
                    (git-time (get-cached-directory-mtime
                               full-path #'get-git-recent-activity))
                    (dir-time (get-simple-directory-mtime full-path))
                    (activity-time (or git-time dir-time)))
               (cons proj activity-time)))
           project-dirs))
         (sorted-projects (sort projects-with-mtime
                               (lambda (a b)
                                 (time-less-p (cdr b) (cdr a)))))
         (display-projects (seq-take sorted-projects (or list-size 8))))
    (cond
     (display-projects
      (mapcar
       (lambda (proj-time-pair)
         (let* ((proj (car proj-time-pair))
                (mtime (cdr proj-time-pair))
                (full-path (expand-file-name proj projects-dir))
                (time-ago
                 (if (and mtime (not (equal mtime '(0 0))))
                     (let ((seconds-ago
                            (float-time (time-subtract
                                         (current-time) mtime))))
                       (cond
                        ((< seconds-ago 3600)
                         (format "%.0fm ago" (/ seconds-ago 60)))
                        ((< seconds-ago 86400)
                         (format "%.1fh ago" (/ seconds-ago 3600)))
                        ((< seconds-ago 604800)
                         (format "%.1fd ago" (/ seconds-ago 86400)))
                        (t (format-time-string "%Y-%m-%d" mtime))))
                   "unknown")))
           (format "    %s - %s\n"
                   (propertize proj
                               'keymap (let ((map (make-sparse-keymap)))
                                         (define-key map (kbd "RET")
						     (eval 
						      `(lambda () (interactive)
							 (dired ,full-path))))
                                         (define-key map [mouse-1]
						     `(lambda () (interactive)
							(dired ,full-path)))
                                         map)
                               'face 'button
                               'help-echo (format "Open %s in Dired" full-path))
                   time-ago)))
       display-projects))
     ((and (file-exists-p projects-dir)
           (file-directory-p projects-dir))
      (list (format "    --- No projects detected in %s ---\n" projects-dir)))
     (t
      (list (format "    --- No %s directory exists ---\n" projects-dir))))))


(defun system-info-string ()
  "Insert system information section with memory and CPU usage."
  (with-output-to-string
    (let ((system-info (gather-system-info)))
      (princ (format "    Emacs PID: %s | Uptime: %s\n" 
                      (plist-get system-info :emacs-pid)
                      (emacs-uptime)))
      (let ((memory (plist-get system-info :memory-mb))
            (cpu-time (plist-get system-info :cpu-seconds)))
	(princ (format "    Memory: %s | CPU Time: %s\n"
			(if memory (format "%.1f MB" memory) "N/A")
			(if cpu-time 
                            (let ((minutes (floor (/ cpu-time 60)))
                                  (seconds (mod cpu-time 60)))
                              (if (> minutes 0)
                                  (format "%dm %.1fs" minutes seconds)
				(format "%.1fs" seconds)))
                          "N/A"))))
      (princ (format "    Packages: %d | Buffers: %d/%d total\n"
                      (plist-get system-info :active-packages)
                      (plist-get system-info :visible-buffers)
		      (plist-get system-info :total-buffers)))
      (princ (format "    Version: %s | Platform: %s\n"
                      (plist-get system-info :emacs-version)
                      (plist-get system-info :system-type)))
      (princ (format "    Time: %s\n"
                     (plist-get system-info :current-time))))))




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
  "Generate our needed dashboard banner TEXT-FILE."
  (unless text-file (setq text-file skewed-dashboard-banner-file))
  (let ((skewed-banner (plist-get
			(plist-get skewed-dashboard-banners :big-money-sw) :skewed))
        (emacs-banner (plist-get
		       (plist-get skewed-dashboard-banners :big-money-se) :emacs)))

    (write-region (concat skewed-banner emacs-banner) nil text-file nil nil nil nil)
    ))


(provide 'dashboard-config)
;;; dashboard-config.el ends here


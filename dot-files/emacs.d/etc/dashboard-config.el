;; Dashboard setup

(defvar skewed-dashboard-banner-file (concat (temporary-file-directory) "skewed-emacs-banner.txt"))
(dashboard-setup-startup-hook)
(setq dashboard-banner-logo-title "𝑺𝑲𝑬𝑾𝑬𝑫 𝑬𝑴𝑨𝑪𝑺")

(setq dashboard-startup-banner skewed-dashboard-banner-file)
(setq dashboard-items '((help . 3)
			(recents  . 3)
                        (active-projects . 5)
			(mcp-status . 1)
                        (system-info . 1)       
                        (other-status . 1)
                        (agenda . 1)
                        (bookmarks . 1)))

(setq dashboard-center-content t)
(setq dashboard-footer-messages '("Brought to you by 𝙶𝚘𝚛𝚗𝚜𝚔𝚎𝚠 𝙴𝚗𝚝𝚎𝚛𝚙𝚛𝚒𝚜𝚎𝚜"))

;;
;; Possibly break this one out into init.el for clarity
;;
(setq initial-buffer-choice
      (lambda ()
	(dashboard-refresh-buffer)
	(get-buffer-create "*dashboard*")))



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
							 (directory-files "/projects" t "^[^.]")
							 )))
			   (when project-dirs
			     (mapcar (lambda (proj)
				       (format "  %s " (file-name-nondirectory proj)))
				     (seq-take project-dirs 8))))
	:mcp-backends '((:name "skewed-emacs" :port 7080 :endpoint "/lisply/lisp-eval")
			(:name "gendl" :port 9080 :endpoint "/lisply/lisp-eval"))
	:quick-actions '((:key "[C-c d r]" :action  "Refresh Dashboard")
			 (:Key "[C-c d d]" :action "Open Dashboard")
			 (:key "[C-c d a]" :action "Toggle Auto-refresh"))))

;; Enhanced dashboard generators with fixes for Unicode and projects
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


(defun dashboard-insert-mcp-status (list-size)
  "Insert MCP services status section with clean ASCII."
  (dashboard-insert-heading "Lisply MCP Backends:")
  (insert "\n")
  (let ((system-info (gather-system-info)))
    (dolist (backend (plist-get system-info :mcp-backends))
      (insert (format "    ✓ %s:%d\n" 
                      (plist-get backend :name)
                      (plist-get backend :port))))))

(defun dashboard-insert-other-status (list-size)
  "Insert MCP services status section with clean ASCII."
  (dashboard-insert-heading "Other Services:")
  (insert "\n")
  (let ((system-info (gather-system-info)))
    (insert "    〰 SLIME: gendl-ccl:4200\n")    
    (insert "    ⚏  Docker Network: emacs-gendl-network\n")))


(defun dashboard-insert-active-projects (list-size)
  "Insert active projects from /projects directory."
  (dashboard-insert-heading "Active Projects:")
  (insert "\n")
  (let* ((project-dirs (seq-filter (lambda (dir) 
                                    (and (file-directory-p (expand-file-name dir "/projects"))
                                         (file-exists-p (expand-file-name (concat dir "/.git") "/projects"))))
                                  (directory-files "/projects" nil "^[^.]")))
         (display-projects (seq-take project-dirs (or list-size 8))))
    (if display-projects
        (dolist (proj display-projects)
          (insert (format "    %s [git]\n" proj)))
      (insert "    --- No projects detected ---\n"))))

;; Register custom dashboard generators
(setq dashboard-item-generators
      (append '((help . dashboard-insert-help-info)
		(system-info . dashboard-insert-system-info)
                (mcp-status . dashboard-insert-mcp-status)
		(other-status . dashboard-insert-other-status)
                (active-projects . dashboard-insert-active-projects))
              (cl-remove-if (lambda (item) 
                              (memq (car item) '(system-info mcp-status active-projects projects)))
                            dashboard-item-generators)))


        
;; Enhanced refresh function using dashboard's proper mechanisms
(defun enhanced-dashboard-refresh ()
  "Refresh dashboard using the dashboard package's proper refresh mechanism."

(defun open-enhanced-dashboard ()
  "Open or switch to enhanced dashboard."
  (interactive)
  (if (get-buffer dashboard-buffer-name)
      (switch-to-buffer dashboard-buffer-name)
    (dashboard-open))
  (enhanced-dashboard-refresh))

(defvar dashboard-auto-refresh-timer nil
  "Timer for automatically refreshing the dashboard")

(defun toggle-enhanced-dashboard-auto-refresh (&optional interval)
  "Toggle auto-refresh of the dashboard using proper dashboard refresh."
  (interactive "P")
  (let ((refresh-interval (or interval 30)))
    (if dashboard-auto-refresh-timer
        (progn
          (cancel-timer dashboard-auto-refresh-timer)
          (setq dashboard-auto-refresh-timer nil)
          (message "Dashboard auto-refresh disabled"))
      (setq dashboard-auto-refresh-timer
            (run-with-timer refresh-interval refresh-interval 'enhanced-dashboard-refresh))
      (message "Dashboard auto-refresh enabled (every %d seconds)" refresh-interval))))


  (interactive)
  (dashboard-refresh-buffer)
  (message "Dashboard refreshed."))

;; Dashboard keybindings
(global-set-key (kbd "C-c d d") 'open-enhanced-dashboard)
(global-set-key (kbd "C-c d r") 'enhanced-dashboard-refresh)
(global-set-key (kbd "C-c d a") 'toggle-enhanced-dashboard-auto-refresh)


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






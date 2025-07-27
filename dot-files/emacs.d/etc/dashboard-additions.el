;; Dashboard additions with real Docker service discovery
;; This file provides actual discovery of running Docker services

(defun discover-running-docker-services ()
  "Discover currently running Docker containers that might be Lisply backends."
  (condition-case nil
      (let ((output (shell-command-to-string "docker ps --format '{{.Names}}\t{{.Ports}}'"))
            (services '()))
        (dolist (line (split-string output "\n" t))
          (when (string-match "\\([^\t]+\\)\t\\(.+\\)" line)
            (let ((name (match-string 1 line))
                  (ports-str (match-string 2 line)))
              
              ;; Look for services with relevant names
              (when (or (string-match "emacs" name)
                        (string-match "gendl" name)
                        (string-match "genworks-gdl" name))
                
                ;; Parse port mappings from the ports string
                (let ((http-ports '())
                      (swank-ports '())
                      (pos 0))
                  
                  ;; Extract HTTP ports (70xx, 90xx)
                  (while (string-match "0\\.0\\.0\\.0:\\([79][0-9][0-9][0-9]\\)->" ports-str pos)
                    (push (string-to-number (match-string 1 ports-str)) http-ports)
                    (setq pos (match-end 0)))
                  
                  ;; Extract SWANK ports (42xx)
                  (setq pos 0)
                  (while (string-match "0\\.0\\.0\\.0:\\(42[0-9][0-9]\\)->" ports-str pos)
                    (push (string-to-number (match-string 1 ports-str)) swank-ports)
                    (setq pos (match-end 0)))
                  
                  (push (list :name name
                             :http-ports (nreverse http-ports)
                             :swank-ports (nreverse swank-ports)
                             :running t)
                        services))))))
        services)
    (error '())))

(defun discover-network-lisply-backends ()
  "Discover lisply backends from actual running Docker containers."
  (let ((services (discover-running-docker-services))
        (backends '())
        (in-container-p (file-exists-p "/projects")))
    
    (dolist (service services)
      (let ((name (plist-get service :name))
            (http-ports (plist-get service :http-ports)))
        
        (dolist (port http-ports)
          (let ((service-type (cond 
                              ((string-match "emacs" name) "emacs-lisp")
                              (t "common-lisp"))))
            (push (list :host (if in-container-p name "localhost")
                       :port (if in-container-p 
                               ;; Container ports: 7080, 9080, 9088, 9090, 9098
                               (cond 
                                ((= port 7081) 7080)
                                ((= port 9081) 9080)
                                ((= port 9089) 9088)
                                ((= port 9091) 9090)
                                ((= port 9099) 9098)
                                (t port))
                             port)
                       :name name
                       :type service-type
                       :running t)
                  backends))
    
    (nreverse backends))))))

(defun discover-swank-services ()
  "Discover SWANK services from actual running Docker containers."
  (let ((services (discover-running-docker-services))
        (swank-services '()))
    
    (dolist (service services)
      (let ((name (plist-get service :name))
            (swank-ports (plist-get service :swank-ports)))
        
        (dolist (port swank-ports)
          (let ((lisp-impl (cond
                           ((string-match "ccl" name) "CCL")
                           ((string-match "sbcl" name) "SBCL") 
                           ((string-match "non-smp" name) "Commercial")
                           ((string-match "smp" name) "Commercial SMP")
                           (t "Lisp"))))
            (push (list :host "localhost"
                       :port port
                       :name name
                       :impl lisp-impl
                       :running t)
                  swank-services)))))
    
    (nreverse swank-services)))

(defun dashboard-insert-other-status (list-size)
  "Insert SWANK services discovered from running containers."
  (when list-size
    (dashboard-insert-heading "SWANK Services (SLIME):")
    (insert "\n")
    (let ((swank-services (discover-swank-services)))
      (if swank-services
          (progn
            (dolist (service swank-services)
              (let ((host (plist-get service :host))
                    (port (plist-get service :port))
                    (name (plist-get service :name))
                    (impl (plist-get service :impl)))
                (insert (format "    * %s:%d (%s - %s)\n" host port impl name))))
            (insert "\n")
            (insert "    Connect with: M-x slime-connect\n"))
        (insert "    No SWANK services detected\n")))))

(defun lisply-backends-string-uncached ()
  "Return backend status string using actual Docker discovery."
  (with-output-to-string 
    (dolist (backend (discover-network-lisply-backends))
      (let ((host (plist-get backend :host))
            (port (plist-get backend :port))
            (name (plist-get backend :name))
            (type (plist-get backend :type)))
        (let ((result (silent-http-ping host port "/lisply/ping-lisp" 0.5)))
          (let ((status (plist-get result :status))
                (time (plist-get result :time))
                (display-name (or name (format "%s:%s" host port))))
            (princ (format "    [%s] %s (%s)%s\n" 
                          (if (string= status "OK") "OK" "DN")
                          display-name
                          type
                          (if (string= status "OK") 
                              (format " - %s" (or time "?ms"))
                            " - not responding")))))))))

(provide 'dashboard-additions)

;;; dashboard-additions --- Look up services more dynamically

;;; Code:

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
        (backends '()))
    
    (dolist (service services)
      (let ((name (plist-get service :name))
            (http-ports (plist-get service :http-ports)))
        
        (dolist (port http-ports)
          (let ((service-type (cond 
                              ((string-match "emacs" name) "emacs-lisp")
                              (t "common-lisp"))))
            (push
	     (list :host (if skewed-emacs-container? name "localhost")
		   :port (if skewed-emacs-container? (cond 
						      ((= port 7081) 7080)
						      ((= port 9081) 9080)
						      ((= port 9089) 9088)
						      ((= port 9091) 9090)
						      ((= port 9099) 9098)
						      (t port)) port)
                   :name name
                   :type service-type
                   :running t)
                  backends)))))
    
    (nreverse backends)))

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
                           (t "Lisp")))
                ;; Convert external port to internal port when in container
                (internal-port (cond
                               ((= port 4201) 4200)  ;; gendl-ccl
                               ((= port 4209) 4208)  ;; genworks-gdl-non-smp  
                               ((= port 4211) 4210)  ;; gendl-sbcl
                               ((= port 4219) 4218)  ;; genworks-gdl-smp
                               (t port))))
            (push (list :host (if skewed-emacs-container? name "localhost")
                       :port (if skewed-emacs-container? internal-port port)
                       :name name
                       :impl lisp-impl
                       :running t)
                  swank-services)))))
    
    (nreverse swank-services)))



(provide 'dashboard-additions)

;;; dashboard-additions.el ends here

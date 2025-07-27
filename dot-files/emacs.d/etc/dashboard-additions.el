;;; dashboard-additions --- Look up services more dynamically
;;; Commentary: supplement for dashboard-config.el
;;; Code:

(defun discover-docker-services ()
  "Discover Docker containers with dynamic port mapping - no hardcoded ports."
  (condition-case nil
      (let ((services '())
            (output (shell-command-to-string "docker ps --format '{{.Names}}\t{{.Ports}}'")))
        (dolist (line (split-string output "\n" t))
          (when (string-match "\\([^\t]+\\)\t\\(.+\\)" line)
            (let ((name (match-string 1 line))
                  (ports-str (match-string 2 line)))
              (when (or (string-match "emacs" name)
                        (string-match "gendl" name)
                        (string-match "genworks" name))
                (let ((port-mappings '())
                      (pos 0))
                  (while (string-match "0\\.0\\.0\\.0:\\([0-9]+\\)->\\([0-9]+\\)" ports-str pos)
                    (let ((host-port (string-to-number (match-string 1 ports-str)))
                          (container-port (string-to-number (match-string 2 ports-str))))
                      (push (list :host-port host-port :container-port container-port) port-mappings))
                    (setq pos (match-end 0)))
                  (let ((http-mappings (seq-filter 
                                       (lambda (m) 
                                         (let ((cp (plist-get m :container-port)))
                                           (or (and (>= cp 7080) (<= cp 7089))
                                               (and (>= cp 9080) (<= cp 9099)))))
                                       port-mappings))
                        (swank-mappings (seq-filter 
                                        (lambda (m)
                                          (let ((cp (plist-get m :container-port)))
                                            (and (>= cp 4200) (<= cp 4299))))
                                        port-mappings)))
                    (when (or http-mappings swank-mappings)
                      (push (list :name name
                                  :http-mappings http-mappings
                                  :swank-mappings swank-mappings
                                  :running t)
                            services))))))))
        ;; Sort services for better display order
        (sort services 
              (lambda (a b)
                (let ((name-a (plist-get a :name))
                      (name-b (plist-get b :name)))
                  ;; Define sort priority: skewed-emacs first, then gendl, then genworks-gdl, then others
                  (cond
                   ;; skewed-emacs always first
                   ((string-match "^skewed-emacs" name-a) t)
                   ((string-match "^skewed-emacs" name-b) nil)
                   ;; gendl (open source) before genworks-gdl (commercial)
                   ((and (string-match "^gendl-" name-a) (string-match "^genworks-gdl-" name-b)) t)
                   ((and (string-match "^genworks-gdl-" name-a) (string-match "^gendl-" name-b)) nil)
                   ;; within same category, sort alphabetically
                   (t (string< name-a name-b)))))))
    (error '())))

(defun discover-network-lisply-backends ()
  "Discover lisply backends using improved docker service discovery."
  (let ((services (discover-docker-services))
        (backends '()))
    (dolist (service services)
      (let ((name (plist-get service :name))
            (http-mappings (plist-get service :http-mappings)))
        (dolist (mapping http-mappings)
          (let* ((host-port (plist-get mapping :host-port))
                 (container-port (plist-get mapping :container-port))
                 (service-type (cond 
                               ((string-match "emacs" name) "emacs-lisp")
                               (t "common-lisp")))
                 (effective-host (if skewed-emacs-container? name "localhost"))
                 (effective-port (if skewed-emacs-container? container-port host-port)))
            (push (list :host effective-host
                       :port effective-port
                       :name name
                       :type service-type
                       :running t)
                  backends)))))
    (nreverse backends)))

(defun discover-swank-services ()
  "Discover SWANK services using improved docker service discovery."
  (let ((services (discover-docker-services))
        (swank-services '()))
    (dolist (service services)
      (let ((name (plist-get service :name))
            (swank-mappings (plist-get service :swank-mappings)))
        (dolist (mapping swank-mappings)
          (let* ((host-port (plist-get mapping :host-port))
                 (container-port (plist-get mapping :container-port))
                 (lisp-impl (cond
                            ((string-match "ccl" name) "CCL")
                            ((string-match "sbcl" name) "SBCL") 
                            ((string-match "non-smp" name) "Commercial")
                            ((string-match "smp" name) "Commercial SMP")
                            (t "Lisp")))
                 (effective-host (if skewed-emacs-container? name "localhost"))
                 (effective-port (if skewed-emacs-container? container-port host-port)))
            (push (list :host effective-host
                       :port effective-port
                       :name name
                       :impl lisp-impl
                       :running t)
                  swank-services)))))
    (nreverse swank-services)))

(provide 'dashboard-additions)

;;; dashboard-additions.el ends here

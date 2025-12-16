;;; services-discovery.el --- Load services from services.json (Single Source of Truth) -*- lexical-binding: t; -*-

;;; Commentary:
;; This module loads service configuration from services.json, providing
;; a single source of truth for the dashboard, SLIME connections, and
;; other Emacs integrations.
;;
;; The services.json file is also used to generate docker-compose.yml
;; and mcp-config.json, ensuring consistency across all configurations.

;;; Code:

(require 'json)

(defvar skewed-services-json-path nil
  "Path to services.json - computed at load time.")

(setq skewed-services-json-path
      (seq-find #'file-exists-p
                (delq nil
                      (list (getenv "SKEWED_SERVICES_JSON")
                            "/projects/skewed-emacs/services.json"
                            (expand-file-name "~/skewed-emacs/services.json")
                            "/home/emacs-user/skewed-emacs/services.json"))))

(when skewed-services-json-path
  (setq skewed-services-private-json-path
        (expand-file-name "services-private.json"
                          (file-name-directory skewed-services-json-path))))

(defvar skewed-services-private-json-path nil
  "Path to optional private services overlay - computed after skewed-services-json-path is set.")


(defvar skewed-services-config nil
  "Cached services configuration loaded from JSON.")

(defvar skewed-services-cache-time nil
  "Time when services config was last loaded.")

(defvar skewed-services-cache-timeout 60
  "Seconds before reloading services.json.")

(defun skewed-load-services-json ()
  "Load and parse services.json, merging with private overlay if present."
  (let ((base-config nil)
        (private-config nil))
    ;; Load base services.json
    (when (and skewed-services-json-path (file-exists-p skewed-services-json-path))
      (with-temp-buffer
        (insert-file-contents skewed-services-json-path)
        (goto-char (point-min))
        (setq base-config (json-parse-buffer :object-type 'plist :array-type 'list))))
    
    ;; Load and merge private overlay if present
    (when (and skewed-services-private-json-path 
               (file-exists-p skewed-services-private-json-path))
      (with-temp-buffer
        (insert-file-contents skewed-services-private-json-path)
        (goto-char (point-min))
        (setq private-config (json-parse-buffer :object-type 'plist :array-type 'list)))
      ;; Merge private services into base
      (when (and base-config private-config)
        (let ((base-services (plist-get base-config :services))
              (private-services (plist-get private-config :services)))
          (when private-services
            ;; Merge each private service into base-services
            (cl-loop for (key val) on private-services by #'cddr
                     do (setq base-services (plist-put base-services key val)))
            (setq base-config (plist-put base-config :services base-services))))))
    
    base-config))

(defun skewed-get-services-config ()
  "Get services config, reloading if cache expired."
  (let ((now (float-time)))
    (when (or (null skewed-services-config)
              (null skewed-services-cache-time)
              (> (- now skewed-services-cache-time) skewed-services-cache-timeout))
      (setq skewed-services-config (skewed-load-services-json))
      (setq skewed-services-cache-time now)))
  skewed-services-config)

(defun skewed-get-services ()
  "Return list of services as plists with normalized keys."
  (let* ((config (skewed-get-services-config))
         (services-plist (plist-get config :services))
         (result '()))
    (when services-plist
      (cl-loop for (name props) on services-plist by #'cddr
               do (let* ((name-str (if (keywordp name)
                                       (substring (symbol-name name) 1)
                                     (format "%s" name)))
                         (ports (plist-get props :ports))
                         (http-ports (plist-get ports :http))
                         (swank-ports (plist-get ports :swank))
                         (service-plist
                          (list :name name-str
                                :type (plist-get props :type)
                                :lisp-impl (or (plist-get props :lisp_impl)
                                              (plist-get props :lisp-impl)
                                              "Unknown")
                                :image (plist-get props :image))))
                    ;; Add HTTP ports if present
                    (when http-ports
                      (setq service-plist
                            (plist-put service-plist :http-host name-str))
                      (setq service-plist
                            (plist-put service-plist :http-port
                                       (plist-get http-ports :container)))
                      (setq service-plist
                            (plist-put service-plist :http-host-port
                                       (plist-get http-ports :host))))
                    ;; Add SWANK ports if present
                    (when swank-ports
                      (setq service-plist
                            (plist-put service-plist :swank-host name-str))
                      (setq service-plist
                            (plist-put service-plist :swank-port
                                       (plist-get swank-ports :container)))
                      (setq service-plist
                            (plist-put service-plist :swank-host-port
                                       (plist-get swank-ports :host))))
                    (push service-plist result))))
    (nreverse result)))

(defun skewed-get-lisply-backends ()
  "Return services suitable for lisply backend display (emacs-lisp or common-lisp)."
  (seq-filter (lambda (svc)
                (member (plist-get svc :type) '("emacs-lisp" "common-lisp")))
              (skewed-get-services)))

(defun skewed-get-swank-services ()
  "Return services with SWANK ports configured."
  (seq-filter (lambda (svc) (plist-get svc :swank-port))
              (skewed-get-services)))

(defun skewed-reload-services ()
  "Force reload of services.json."
  (interactive)
  (setq skewed-services-config nil)
  (setq skewed-services-cache-time nil)
  (message "Reloaded %d services from services.json"
           (length (skewed-get-services))))

(provide 'services-discovery)
;;; services-discovery.el ends here

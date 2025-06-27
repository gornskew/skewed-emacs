(defvar bootstrap-version)


;;
;; Note: Needs Nodejs 22+ now
;;
(setq copilot-node-executable "node")
;;
;;

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el")) :ensure t)

(defvar copilot-local-server? nil)

(setq copilot-local-server? (ignore-errors (copilot-install-server)))
(if copilot-local-server?
    (message "Copilot server installed successfully.")
  (warn "Copilot server did not install - Falling back to lisply-mcp container."))
    
;;(add-hook 'prog-mode-hook 'copilot-mode)


;; Configuration for using copilot.el with Docker
;; This bypasses all installation checks and directly connects to your Docker container

;; 1. Override the command function to return your Docker exec command
(unless copilot-local-server?

  (setq copilot-server-executable
	"/home/node/skewed-emacs/docker/copilot-docker.sh")

  ;; That's it! Now you can use:
  ;; - (copilot--start-server) to start the server
  ;; - M-x copilot-mode to enable copilot in a buffer
  ;; - M-x global-copilot-mode to enable globally

  ;; Optional: If you prefer using a wrapper script instead of direct docker exec:
  ;; (defun copilot--command ()
  ;;   "Return the command-line to start copilot server via wrapper script."
  ;;   (list "/path/to/copilot-docker-wrapper.sh" "--stdio"))
  )

(require 'bind-key)
(bind-key* "C-." 'copilot-accept-completion)
(bind-key* "M-," 'copilot-accept-completion)
(bind-key* "C-," 'copilot-accept-completion-by-word)

(provide 'copilot-config)
;;; copilot.el ends here

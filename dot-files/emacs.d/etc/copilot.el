;;; package --- Summary: copilot.el customizations
;;; Commentary:

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
  (progn
    (warn "Copilot server did not install - Falling back to lisply-mcp container.")
    (setq copilot-server-executable
	"/home/emacs-user/skewed-emacs/docker/copilot-docker.sh")))

;;(add-hook 'prog-mode-hook 'copilot-mode)


(require 'bind-key)
(bind-key* "C-." 'copilot-accept-completion)
(bind-key* "M-," 'copilot-accept-completion)
(bind-key* "C-," 'copilot-accept-completion-by-word)

(provide 'copilot-config)
;;; copilot.el ends here

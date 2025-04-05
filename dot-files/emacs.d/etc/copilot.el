(defvar bootstrap-version)


;;
;; Note: Needs Nodejs 22+ now
;;
(setq copilot-node-executable "node")
;;
;;

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el")) :ensure t)

;;(add-hook 'prog-mode-hook 'copilot-mode)

(require 'bind-key)
(bind-key* "C-." 'copilot-accept-completion)
(bind-key* "M-," 'copilot-accept-completion)
(bind-key* "C-," 'copilot-accept-completion-by-word)

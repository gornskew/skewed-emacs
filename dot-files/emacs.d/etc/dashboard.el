;; Dashboard setup

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome to Emacs!")
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (agenda . 5)
                          (bookmarks . 5)
                          (registers . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-footer-messages '("Emacs is the ultimate text editor!"))
  )
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

(provide 'dashboard)

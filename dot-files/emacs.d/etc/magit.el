
(package-install 'magit)

(setq magit-git-executable (locate-file "git" exec-path))

(global-set-key (kbd "C-x g") 'magit-status)




;;(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
;;(setq global-magit-file-mode t) ;; the default anyway.

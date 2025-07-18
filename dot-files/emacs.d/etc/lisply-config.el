
(add-to-list
 'load-path
 (concat emacs-config-directory
	 "sideloaded/lisply-backend/source/"))
(load "http-setup")
(load "endpoints")

(provide 'lisply-config)

;; <!-- language: lang-lisp -->


(package-install 'markdown-mode)
(package-install 'impatient-mode)

;;(defun strapdown-html (buffer)
;;    (princ (with-current-buffer buffer
;;      (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
;;           (current-buffer)))


(defun strapdown-html (buffer)
    (princ (with-current-buffer buffer
             (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>"
                     (buffer-substring-no-properties (point-min) (point-max))))
           (current-buffer)))

(setq httpd-port 8058)

(defun start-impatient-server ()
  (httpd-start)
  (message "Serving on port %s" httpd-port))



(message "Do M-x imp-set-user-filter RET strapdown-html RET")
;;
;; (imp-set-user-filter 'markdown-html) ;; need to do this with M-x
;; imp-set-user-filter until we figure out how to do it
;; programatically.


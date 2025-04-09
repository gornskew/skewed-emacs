;; ielm-stdio.el — Basic Elisp REPL for stdio without terminal UI

(require 'ielm)

;; Create an IELM buffer to initialize IELM's environment
(with-current-buffer (get-buffer-create "*ielm*")
  (inferior-emacs-lisp-mode))

;; Main REPL loop - use lexical binding instead of IELM's environment
(while t
  (condition-case err
      (let* ((form (condition-case nil
                      (read)  ; Read directly from stdin
                    (end-of-file (kill-emacs 0))))
            (result (eval form lexical-binding)))
        ;; Print result to stdout
        (prin1 result)
        (princ "\n"))
    (error
     (princ (format "Error: %S\n" err)))))
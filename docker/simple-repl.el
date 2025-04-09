;; simple-repl.el — Simple Elisp REPL for stdio

;; Enable non-interactive mode to allow proper stdio handling
(setq default-directory "/home/dcooper8/projects/skewed-emacs/docker/")
(setq noninteractive nil)
(setq buffer-file-coding-system 'utf-8-unix)

;; Basic read-eval-print loop
(message "Starting REPL...")
(while t
  (condition-case err
      (let* ((form (read-from-minibuffer "")))
        (message "Evaluating: %S" form)
        (let ((result (eval (read form) t)))
          (message "Result: %S" result)
          (prin1 result)
          (princ "\n")))
    (error
     (message "Error: %S" err)
     (princ (format "Error: %S\n" err)))))
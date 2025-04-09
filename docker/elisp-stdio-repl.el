;; elisp-stdio-repl.el — Pure Elisp REPL using stdio

;; Core REPL function - reads s-expressions one at a time, evaluates, and prints result
(defun elisp-stdio-repl ()
  (while t
    (condition-case err
        (let* ((form (read))
               (result (eval form t)))
          (prin1 result)
          (terpri))
      (end-of-file (kill-emacs 0))
      (error
       (prin1 (format "ERROR: %S" err))
       (terpri)))))

;; Start the REPL
(elisp-stdio-repl)
;; elisp-repl-daemon.el — REPL that runs in an Emacs daemon

;; Setup REPL implementation
(defun elisp-stdio-repl ()
  "Run a simple Elisp REPL on standard input/output."
  (interactive)
  ;; Read from standard input until EOF
  (let ((standard-input (get-buffer-create "*stdin*"))
        (standard-output (get-buffer-create "*stdout*")))
    (with-current-buffer standard-input
      (insert-file-contents-literally "/dev/stdin"))
    (with-current-buffer standard-input
      (goto-char (point-min))
      (while (not (eobp))
        (condition-case err
            (let* ((form (read (current-buffer)))
                   (result (eval form t)))
              (with-current-buffer standard-output
                (prin1 result)
                (terpri)
                (write-region (point-min) (point-max) "/dev/stdout")
                (erase-buffer)))
          (end-of-file (keyboard-quit))
          (error
           (with-current-buffer standard-output
             (princ (format "ERROR: %S" err))
             (terpri)
             (write-region (point-min) (point-max) "/dev/stdout")
             (erase-buffer))))))))

;; Export the function so emacsclient can call it
(provide 'elisp-repl-daemon)
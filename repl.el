;; repl.el — Persistent Emacs Lisp REPL for stdin/stdout

(defun read-form-from-lines (stream)
  "Accumulate characters from STREAM until a full s-expression is read."
  (let ((form-string "")
        (form nil))
    (while (not form)
      (let ((char (condition-case nil
                      (read-char stream)
                    (end-of-file nil))))
        (if (null char)
            (if (string-empty-p form-string)
                (signal 'end-of-file nil)
              (error "Unexpected EOF while reading form")))
        (setq form-string (concat form-string (char-to-string char)))
        (condition-case _
            (setq form (with-temp-buffer
                         (insert form-string)
                         (goto-char (point-min))
                         (read (current-buffer))))
          (end-of-file nil)
          (invalid-read-syntax nil))))
    form))

(defun start-repl ()
  ;; Open a real file stream from /dev/stdin to simulate persistent input
  (let ((stdin (with-current-buffer (generate-new-buffer "*stdin*")
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally "/dev/stdin")
                 (goto-char (point-min))
                 (current-buffer))))
    (while t
      (condition-case err
          (let ((form (read-form-from-lines stdin)))
            (let ((result (eval form)))
              (prin1 result)
              (terpri)
              (terpri)))
        (end-of-file (kill-emacs 0))
        (error
         (princ (format "Error: %S" err))
         (terpri)
         (terpri))))))

(start-repl)

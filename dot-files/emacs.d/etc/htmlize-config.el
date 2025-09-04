

(defun dc/html-export-buffer-exact (&optional file)
  "Export current buffer to a self-contained HTML file that matches on-screen appearance."
  (interactive)
  (require 'htmlize)
  ;; Ensure everything is actually fontified (jit-lock is lazy by default).
  (save-restriction
    (widen)
    (when (fboundp 'font-lock-ensure)
      (font-lock-ensure (point-min) (point-max))))

  (let* ((file (or file
                   (concat (file-name-nondirectory
                            (or (buffer-file-name) (buffer-name))) ".html")))
         (bg (or (face-background 'default nil t)
                 (frame-parameter nil 'background-color)
                 "white"))
         (fg (or (face-foreground 'default nil t)
                 (frame-parameter nil 'foreground-color)
                 "black"))
         (tabw (or tab-width 8))
         (html-buf (htmlize-buffer)))
    (with-current-buffer html-buf
      ;; Inject a tiny stylesheet for page bg + monospace + tab width + no wrapping.
      (goto-char (point-min))
      (when (re-search-forward "</title>\\s-*\n" nil t)
        (insert (format
                 "<style>
  html,body{height:100%%}
  body{background:%s;color:%s;margin:0}
  /* htmlize already emits inline colors for every span; this just sets page basics */
  pre{
    margin:0; padding:1rem;
    font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, 'Liberation Mono', monospace;
    white-space: pre;           /* preserve indentation & line endings; no wrapping */
    tab-size:%d;
    overflow-x:auto;            /* allow horizontal scroll for long lines */
  }
</style>\n" bg fg tabw)))
      (write-file file))
    (kill-buffer html-buf)
    (message "Wrote %s" (expand-file-name file))))

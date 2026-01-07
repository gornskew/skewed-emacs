;;; lisply-skewed-search-build.el --- build skewed_search index -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Genworks

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Build-time skewed_search index generator. Not used at runtime.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom emacs-lisply-skewed-search-services-path
  (expand-file-name "../../services.sexp" (file-truename user-emacs-directory))
  "Path to services.sexp for skewed_search config."
  :type 'string
  :group 'emacs-lisply)

(defcustom emacs-lisply-skewed-search-index-path
  "~/.emacs.d/sideloaded/lisply-backend/skewed-search-index.sexp"
  "Path to skewed_search index s-expression file."
  :type 'string
  :group 'emacs-lisply)

(defconst emacs-lisply-skewed-search-index-version 1
  "Index format version. Increment when format changes.")

(defconst emacs-lisply-skewed-search-max-file-bytes (* 1024 1024))
(defconst emacs-lisply-skewed-search-preextract-default-lines 24)
(defconst emacs-lisply-skewed-search-preextract-default-chars 1200)

(defconst emacs-lisply-skewed-search-default-exts
  '(".lisp" ".lsp" ".cl" ".gdl" ".gendl" ".asd" ".isc"
    ".md" ".markdown" ".org" ".txt" ".rst"
    ".el" ".js" ".ts" ".json" ".yml" ".yaml" ".html" ".css"))

(defconst emacs-lisply-skewed-search-language-exts
  '(("lisp" . (".lisp" ".lsp" ".cl" ".asd" ".el"))
    ("gendl" . (".gendl"))
    ("gdl" . (".gdl" ".gendl" ".lisp" ".lsp" ".cl"))
    ("markdown" . (".md" ".markdown" ".org" ".rst"))))

(defun emacs-lisply-skewed-search--projects-root ()
  (let* ((candidates (list "/projects" (expand-file-name "~/projects")))
         (found (cl-find-if (lambda (root) (file-exists-p (expand-file-name "gendl" root)))
                            candidates)))
    (or found "/projects")))

(defun emacs-lisply-skewed-search--read-services ()
  (when (and emacs-lisply-skewed-search-services-path
             (file-exists-p emacs-lisply-skewed-search-services-path))
    (with-temp-buffer
      (insert-file-contents emacs-lisply-skewed-search-services-path)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun emacs-lisply-skewed-search--skewed-search-config (services)
  (plist-get services :skewed-search-config))

(defun emacs-lisply-skewed-search--services-extensions-alist (extensions)
  (when extensions
    `(("default" . ,(plist-get extensions :default))
      ("lisp" . ,(plist-get extensions :lisp))
      ("gendl" . ,(plist-get extensions :gendl))
      ("gdl" . ,(plist-get extensions :gdl))
      ("markdown" . ,(plist-get extensions :markdown)))))

(defun emacs-lisply-skewed-search--services-sources-alist (sources)
  (when sources
    (mapcar
     (lambda (source)
       (let ((name (plist-get source :name))
             (entries (plist-get source :entries)))
         (cons name
               (mapcar
                (lambda (entry)
                  `(("root" . ,(plist-get entry :root))
                    ("repo" . ,(plist-get entry :repo))
                    ("repo_url" . ,(plist-get entry :repo-url))
                    ("repo_root" . ,(plist-get entry :repo-root))))
                entries))))
     sources)))

(defun emacs-lisply-skewed-search--config-from-services (services)
  (let* ((gdl-config (emacs-lisply-skewed-search--skewed-search-config services))
         (sources (plist-get gdl-config :sources))
         (ignore-dirs (plist-get gdl-config :ignore-dirs))
         (exclude-paths (plist-get gdl-config :exclude-paths))
         (extensions (plist-get gdl-config :extensions))
         (index-path (plist-get gdl-config :index-path))
         (preextract-snippets (plist-get gdl-config :preextract-snippets))
         (preextract-max-lines (plist-get gdl-config :preextract-max-lines))
         (preextract-max-chars (plist-get gdl-config :preextract-max-chars)))
    (when gdl-config
      (let ((items `(("sources" . ,(emacs-lisply-skewed-search--services-sources-alist sources))
                     ("ignore_dirs" . ,ignore-dirs)
                     ("exclude_paths" . ,exclude-paths)
                     ("extensions" . ,(emacs-lisply-skewed-search--services-extensions-alist extensions)))))
        (when index-path
          (push (cons "index_path" index-path) items))
        (when preextract-snippets
          (push (cons "preextract_snippets" preextract-snippets) items))
        (when preextract-max-lines
          (push (cons "preextract_max_lines" preextract-max-lines) items))
        (when preextract-max-chars
          (push (cons "preextract_max_chars" preextract-max-chars) items))
        (nreverse items)))))

(defun emacs-lisply-skewed-search--config-value (config key default)
  (let ((entry (and config (assoc key config))))
    (if entry (cdr entry) default)))

(defun emacs-lisply-skewed-search--config-sources (config root)
  (let ((sources (emacs-lisply-skewed-search--config-value config "sources" nil)))
    (when sources
      (mapcar
       (lambda (pair)
         (let ((source-key (car pair))
               (entries (cdr pair)))
           (cons source-key
                 (mapcar
                  (lambda (entry)
                    (let* ((path (cdr (assoc "root" entry)))
                           (repo (cdr (assoc "repo" entry)))
                           (repo-url (cdr (assoc "repo_url" entry)))
                           (repo-root (cdr (assoc "repo_root" entry)))
                           (root-path (if (and path (file-name-absolute-p path))
                                          path
                                        (expand-file-name (or path "") root)))
                           (repo-root-path (if (and repo-root (file-name-absolute-p repo-root))
                                               repo-root
                                             (expand-file-name (or repo-root (or path "")) root))))
                      `((:root . ,root-path)
                        (:repo . ,repo)
                        (:repo-url . ,repo-url)
                        (:repo-root . ,repo-root-path))))
                  entries))))
       sources))))

(defun emacs-lisply-skewed-search--source-map (config)
  (let* ((root (emacs-lisply-skewed-search--projects-root))
         (config-sources (emacs-lisply-skewed-search--config-sources config root)))
    (when config-sources
      config-sources)))

(defun emacs-lisply-skewed-search--clone-entry (entry branch)
  (let* ((root (cdr (assoc :root entry)))
         (repo-url (cdr (assoc :repo-url entry)))
         (branch (or branch "master")))
    (cond
     ((or (not root) (string-empty-p root)) nil)
     ((or (not repo-url) (string-empty-p repo-url)) nil)
     ((file-exists-p root) t)
     (t
      (make-directory (file-name-directory root) t)
      (let ((exit (process-file
                   "git" nil "*skewed-search-clone*" t
                   "clone" "--depth" "1" "--branch" branch repo-url root)))
        (and (integerp exit) (zerop exit)))))))

(defun emacs-lisply-skewed-search--clone-corpora (source-map branch)
  (let ((entries '())
        (failed nil))
    (dolist (source source-map)
      (dolist (entry (cdr source))
        (let ((repo-url (cdr (assoc :repo-url entry))))
          (when (and repo-url (not (string-empty-p repo-url)))
            (push entry entries)))))
    (when (null entries)
      (message "skewed_search index skipped: no repo_url entries in config.")
      (setq failed t))
    (dolist (entry entries)
      (unless (emacs-lisply-skewed-search--clone-entry entry branch)
        (setq failed t)
        (message "skewed_search clone failed: %s (%s)"
                 (cdr (assoc :root entry))
                 (cdr (assoc :repo-url entry)))))
    (not failed)))

(defun emacs-lisply-skewed-search--index-path (&optional _config)
  emacs-lisply-skewed-search-index-path)

(defun emacs-lisply-skewed-search--config-ignore-dirs (config)
  (emacs-lisply-skewed-search--config-value config "ignore_dirs" nil))

(defun emacs-lisply-skewed-search--vector-to-list (value)
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun emacs-lisply-skewed-search--compile-path-filters (filters)
  (let (compiled)
    (dolist (filter filters (nreverse compiled))
      (when (and (stringp filter) (not (string-empty-p filter)))
        (let ((normalized (emacs-lisply-skewed-search--normalize-path filter)))
          (if (string-match-p "[*?]" normalized)
              (push (cons 'regex (wildcard-to-regexp normalized) ) compiled)
            (push (cons 'prefix normalized) compiled)))))))

(defun emacs-lisply-skewed-search--config-exclude-filters (config)
  (let ((exclude (emacs-lisply-skewed-search--config-value config "exclude_paths" nil)))
    (emacs-lisply-skewed-search--compile-path-filters
     (emacs-lisply-skewed-search--vector-to-list exclude))))

(defun emacs-lisply-skewed-search--normalize-path (path)
  (replace-regexp-in-string "\\\\" "/" path))

(defun emacs-lisply-skewed-search--path-filters-match-p (filters paths)
  (if (null filters)
      t
    (catch 'match
      (dolist (path paths)
        (let ((normalized (emacs-lisply-skewed-search--normalize-path path)))
          (dolist (filter filters)
            (let ((type (car filter))
                  (value (cdr filter)))
              (when (or (and (eq type 'prefix) (string-prefix-p value normalized))
                        (and (eq type 'regex) (string-match-p value normalized)))
                (throw 'match t))))))
      nil)))

(defun emacs-lisply-skewed-search--excluded-path-p (exclude-filters path)
  (and exclude-filters
       (not (null exclude-filters))
       (emacs-lisply-skewed-search--path-filters-match-p
        exclude-filters
        (list (emacs-lisply-skewed-search--normalize-path path)))))

(defun emacs-lisply-skewed-search--config-exts (config)
  (let* ((exts (emacs-lisply-skewed-search--config-value config "extensions" nil))
         (default (cdr (assoc "default" exts)))
         (lisp (cdr (assoc "lisp" exts)))
         (gendl (cdr (assoc "gendl" exts)))
         (gdl (cdr (assoc "gdl" exts)))
         (markdown (cdr (assoc "markdown" exts))))
    (list
     (cons "default" (or default emacs-lisply-skewed-search-default-exts))
     (cons "lisp" (or lisp (cdr (assoc "lisp" emacs-lisply-skewed-search-language-exts))))
     (cons "gendl" (or gendl (cdr (assoc "gendl" emacs-lisply-skewed-search-language-exts))))
     (cons "gdl" (or gdl (cdr (assoc "gdl" emacs-lisply-skewed-search-language-exts))))
     (cons "markdown" (or markdown (cdr (assoc "markdown" emacs-lisply-skewed-search-language-exts)))))))

(defun emacs-lisply-skewed-search--allowed-exts (config language)
  (let* ((exts (emacs-lisply-skewed-search--config-exts config))
         (key (and language (downcase (format "%s" language))))
         (entry (assoc key exts)))
    (or (cdr entry) (cdr (assoc "default" exts)))))

(defun emacs-lisply-skewed-search--file-ext (path)
  (downcase (file-name-extension path t)))

(defun emacs-lisply-skewed-search--allowed-file-p (path allowed-exts)
  (member (emacs-lisply-skewed-search--file-ext path) allowed-exts))

(defun emacs-lisply-skewed-search--list-files (root allowed-exts ignore-dirs exclude-filters)
  (let (results)
    (when (file-exists-p root)
      (dolist (entry (directory-files root t "\\`[^.]"))
        (cond
         ((file-directory-p entry)
          (unless (member (file-name-nondirectory entry) ignore-dirs)
            (setq results (nconc results (emacs-lisply-skewed-search--list-files entry allowed-exts ignore-dirs exclude-filters)))))
         ((and (file-regular-p entry)
               (emacs-lisply-skewed-search--allowed-file-p entry allowed-exts)
               (not (emacs-lisply-skewed-search--excluded-path-p exclude-filters entry)))
          (push entry results)))))
    results))

(defun emacs-lisply-skewed-search--guess-language (path)
  (let ((ext (emacs-lisply-skewed-search--file-ext path)))
    (cond
     ((member ext (cdr (assoc "lisp" emacs-lisply-skewed-search-language-exts))) "lisp")
     ((member ext (cdr (assoc "gendl" emacs-lisply-skewed-search-language-exts))) "gendl")
     ((member ext (cdr (assoc "gdl" emacs-lisply-skewed-search-language-exts))) "gdl")
     ((member ext (cdr (assoc "markdown" emacs-lisply-skewed-search-language-exts))) "markdown")
     (t nil))))

(defun emacs-lisply-skewed-search--section-heading (lines start-line path)
  (let ((ext (emacs-lisply-skewed-search--file-ext path)))
    (when (member ext (cdr (assoc "markdown" emacs-lisply-skewed-search-language-exts)))
      (catch 'found
        (let ((i start-line))
          (while (>= i (max 0 (- start-line 50)))
            (let ((line (nth i lines)))
              (cond
               ((and (string= ext ".org")
                     (string-match "^\\*+\\s-+\\(.+\\)$" line))
                (throw 'found (string-trim (match-string 1 line))))
               ((and (not (string= ext ".org"))
                     (string-match "^#+\\s-+\\(.+\\)$" line))
                (throw 'found (string-trim (match-string 1 line))))))
            (setq i (1- i))))
        nil))))

(defun emacs-lisply-skewed-search--trim-lines (lines max-chars)
  (if (not max-chars)
      lines
    (let ((total 0)
          (kept '()))
      (catch 'done
        (dolist (line lines)
          (let ((next (+ total (length line) 1)))
            (when (and (> next max-chars) kept)
              (throw 'done nil))
            (push line kept)
            (setq total next))))
      (nreverse kept))))

(defun emacs-lisply-skewed-search--preextract-snippets (lines max-lines max-chars)
  (let ((snippets '())
        (total (length lines))
        (start 0))
    (while (< start total)
      (let* ((end (min (1- total) (+ start (1- max-lines))))
             (slice (cl-subseq lines start (1+ end)))
             (trimmed (emacs-lisply-skewed-search--trim-lines slice max-chars)))
        (when (and trimmed (> (length trimmed) 0))
          (let ((trimmed-end (+ start (1- (length trimmed)))))
            (push (list :start-line start :end-line trimmed-end :lines trimmed) snippets)))
        (setq start (1+ end))))
    (nreverse snippets)))

(defun emacs-lisply-skewed-search--preextract-file (file max-lines max-chars)
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((lines (split-string (buffer-string) "\n" nil))
           (snippets (emacs-lisply-skewed-search--preextract-snippets
                      lines max-lines max-chars))
           (language (emacs-lisply-skewed-search--guess-language file)))
      (mapcar
       (lambda (snippet)
         (let* ((snippet-lines (plist-get snippet :lines))
                (snippet-text (mapconcat #'identity snippet-lines "\n"))
                (preview (or (cl-find-if (lambda (line) (> (length (string-trim line)) 0))
                                         snippet-lines)
                             "")))
           `(("start_line" . ,(plist-get snippet :start-line))
             ("end_line" . ,(plist-get snippet :end-line))
             ("snippet" . ,snippet-text)
             ("preview" . ,(string-trim preview))
             ("section" . ,(emacs-lisply-skewed-search--section-heading
                            lines (plist-get snippet :start-line) file))
             ("language" . ,language))))
       snippets))))

(defun emacs-lisply-skewed-search--preextract-enabled (config)
  (let ((value (emacs-lisply-skewed-search--config-value config "preextract_snippets" nil)))
    (and value (not (eq value nil)))))

(defun emacs-lisply-skewed-search--preextract-max-lines (config)
  (or (emacs-lisply-skewed-search--config-value config "preextract_max_lines" nil)
      emacs-lisply-skewed-search-preextract-default-lines))

(defun emacs-lisply-skewed-search--preextract-max-chars (config)
  (or (emacs-lisply-skewed-search--config-value config "preextract_max_chars" nil)
      emacs-lisply-skewed-search-preextract-default-chars))

(defun emacs-lisply-skewed-search--build-index-entry (source-key entry file attrs preextract-enabled max-lines max-chars)
  (let* ((language (emacs-lisply-skewed-search--guess-language file))
         (size (file-attribute-size attrs))
         (item `(("source" . ,source-key)
                 ("path" . ,(emacs-lisply-skewed-search--normalize-path file))
                 ("mtime" . ,(format "%s" (file-attribute-modification-time attrs)))
                 ("language" . ,language))))
    (when (and preextract-enabled
               size
               (< size emacs-lisply-skewed-search-max-file-bytes))
      (let ((snippets (emacs-lisply-skewed-search--preextract-file file max-lines max-chars)))
        (when snippets
          (setq item (append item (list (cons "snippets" (vconcat snippets))))))))
    item))

(defun emacs-lisply-skewed-search--compute-checksum (files)
  (let ((count (length files))
        (total-size 0))
    (dolist (file files)
      (let ((path (cdr (assoc "path" file))))
        (when (and path (file-exists-p path))
          (setq total-size (+ total-size
                              (or (file-attribute-size (file-attributes path)) 0))))))
    (format "%d-%d" count total-size)))

(defun emacs-lisply-skewed-search--validate-sources (source-map)
  (let (missing)
    (dolist (source source-map)
      (let ((source-key (car source))
            (entries (cdr source)))
        (dolist (entry entries)
          (let ((root (cdr (assoc :root entry))))
            (unless (and root (file-exists-p root))
              (push (list :source source-key :path root) missing))))))
    (nreverse missing)))

(defun emacs-lisply-skewed-search-build-index ()
  "Write a lightweight index snapshot to the configured index_path."
  (interactive)
  (let* ((services (emacs-lisply-skewed-search--read-services))
         (config (and services (emacs-lisply-skewed-search--config-from-services services)))
         (source-map (and config (emacs-lisply-skewed-search--source-map config))))
    (unless source-map
      (error "Missing skewed_search config sources in services.sexp"))
    (let ((missing (emacs-lisply-skewed-search--validate-sources source-map)))
      (when missing
        (message "WARNING: Missing sources: %S" missing)))
    (let* ((index-path (emacs-lisply-skewed-search--index-path))
           (allowed-exts (emacs-lisply-skewed-search--allowed-exts config nil))
           (ignore-dirs (emacs-lisply-skewed-search--config-ignore-dirs config))
           (exclude-filters (emacs-lisply-skewed-search--config-exclude-filters config))
           (preextract-enabled (emacs-lisply-skewed-search--preextract-enabled config))
           (preextract-max-lines (emacs-lisply-skewed-search--preextract-max-lines config))
           (preextract-max-chars (emacs-lisply-skewed-search--preextract-max-chars config))
           (items '()))
      (dolist (source-key (mapcar #'car source-map))
        (let ((entries (cdr (assoc source-key source-map))))
          (dolist (entry entries)
            (let ((root (cdr (assoc :root entry))))
              (when (and root (file-exists-p root))
                (dolist (file (emacs-lisply-skewed-search--list-files root allowed-exts ignore-dirs exclude-filters))
                  (let ((attrs (file-attributes file)))
                    (push (emacs-lisply-skewed-search--build-index-entry
                           source-key entry file attrs
                           preextract-enabled preextract-max-lines preextract-max-chars)
                          items))))))))
      (let* ((files (nreverse items))
             (checksum (emacs-lisply-skewed-search--compute-checksum files)))
        (with-temp-file index-path
          (let ((print-length nil)
                (print-level nil))
            (prin1
             (list (cons "generated_at" (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))
                   (cons "version" emacs-lisply-skewed-search-index-version)
                   (cons "checksum" checksum)
                   (cons "config" config)
                   (cons "files" (vconcat files)))
             (current-buffer))))))))

(defun emacs-lisply-skewed-search-build-index-with-clone (&optional branch)
  "Clone corpora from config repo_url entries, then build the index.
If any clone fails, log a warning and skip index generation."
  (let* ((services (emacs-lisply-skewed-search--read-services))
         (config (and services (emacs-lisply-skewed-search--config-from-services services)))
         (source-map (and config (emacs-lisply-skewed-search--source-map config))))
    (if (not source-map)
        (message "skewed_search index skipped: missing config sources in services.sexp.")
      (if (emacs-lisply-skewed-search--clone-corpora source-map branch)
          (emacs-lisply-skewed-search-build-index)
        (message "skewed_search index skipped: clone failed. Set SKEWED_BUILD_SKEWED_INDEX=false or use docker/build-skewed-search-index.sh.")))))

(provide 'lisply-skewed-search-build)
;;; lisply-skewed-search-build.el ends here

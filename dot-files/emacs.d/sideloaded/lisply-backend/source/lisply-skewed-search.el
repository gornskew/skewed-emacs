;;; skewed-search.el --- skewed_search support -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Genworks

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Build-time stuff followed by runtime stuff are all in this one file.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom emacs-lisply-skewed-search-services-path
  (expand-file-name "../../services.sexp" (file-truename user-emacs-directory))
  "Path to services.sexp for skewed_search config."
  :type 'string
  :group 'emacs-lisply)

(defcustom emacs-lisply-skewed-search-index-path
  (expand-file-name "~/.emacs.d/sideloaded/lisply-backend/skewed-search-index.sexp")
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


;;
;; FLAG this projects-root may or may not ever be needed for normal usage i.e. when not doing
;;      internal skewed-search development. 
;;
(defun emacs-lisply-skewed-search--projects-root ()
  (let* ((candidates (list "/projects" (expand-file-name "~/projects")))
         (found (cl-find-if (lambda (root) (file-exists-p (expand-file-name "gendl" root)))
                            candidates)))
    (or found "/projects")))

(defun emacs-lisply-skewed-search--read-services ()
  "Read the services sexp into memory.  FLAG - this `with-temp-buffer` dance should be replaced with a utility function."
  (when (and emacs-lisply-skewed-search-services-path
             (file-exists-p emacs-lisply-skewed-search-services-path))
    (with-temp-buffer
      (insert-file-contents emacs-lisply-skewed-search-services-path)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun emacs-lisply-skewed-search--skewed-search-config (services)
  "The config is embedded in SERVICES which comes from the SSoT file."
  (plist-get services :skewed-search-config))

(defun emacs-lisply-skewed-search--services-extensions-alist (extensions)
  "Return an alist of names / relevant file extensions found in EXTENSIONS.  FLAG Consider deriving this mapping from SSoT rather than hardcoding the name/keyword list here."
  (when extensions
    `(("default" . ,(plist-get extensions :default))
      ("lisp" . ,(plist-get extensions :lisp))
      ("gendl" . ,(plist-get extensions :gendl))
      ("gdl" . ,(plist-get extensions :gdl))
      ("markdown" . ,(plist-get extensions :markdown)))))

(defun emacs-lisply-skewed-search--services-sources-alist (sources)
  "Return an alist of names / relevant info for each corpus in the search corpora found in SOURCES."
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
  ""
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
      (let ((items
	     `(("sources" . ,(emacs-lisply-skewed-search--services-sources-alist sources))
               ("ignore_dirs" . ,ignore-dirs)
               ("exclude_paths" . ,exclude-paths)
               ("extensions" .
		,(emacs-lisply-skewed-search--services-extensions-alist extensions)))))
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
  "Return an alist with coordinates for each corpus source according to CONFIG.  Use ROOT only if CONFIG is given as a relative pathname.  FLAG: This extra scaffolding around s-expressions appears vestigial from when we were round-tripping with json.  We probably can look at manipulate s-expressions more directly at this juncture now that we have json out of the loop."
  (let ((sources (emacs-lisply-skewed-search--config-value config "sources" nil)))
    (when sources
      (mapcar
       (lambda (pair)
         (let ((source-key (car pair))
               (entries (cdr pair)))
           (cons
	    source-key
            (mapcar
             (lambda (entry)
               (let*
		   ((path (cdr (assoc "root" entry)))
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

;;
;; FLAG -- i'm going to skip commenting the rest of these, but in
;; general it looks like there's still quite some leftover code from
;; when we were using JSON for the index format which required
;; significantly more marshalling/unmarshalling than our current pure
;; s-expression approach should require.
;;
;; Perhaps a fundamental fresh rewrite would reveal that by keeping
;; everything in s-expressions, we can lean into plist destructuring
;; patterns and getters/setters and end up with significantly less
;; code for all of skewed-search.el. 
;;


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

(defun emacs-lisply-skewed-search--to-vector (items)
  "Convert ITEMS list to vector, or return empty vector if nil."
  (if items (vconcat items) []))

(defun emacs-lisply-skewed-search--compile-path-filters (filters)
  (let (compiled)
    (dolist (filter filters (nreverse compiled))
      (when (and (stringp filter) (not (string-empty-p filter)))
        (let ((normalized (emacs-lisply-skewed-search--normalize-path filter)))
          (if (string-match-p "[*?]" normalized)
              (push (cons 'regex (wildcard-to-regexp normalized)) compiled)
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

;; And Now the Runtime stuff:

(require 'simple-httpd nil t)
(require 'lisply-http-setup nil t)


;; Provide a fallback logger when running outside the full lisply server.
(unless (fboundp 'emacs-lisply-log)
  (defun emacs-lisply-log (fmt &rest args)
    (apply #'message (concat "[skewed-search] " fmt) args)))

(defconst emacs-lisply-skewed-search-default-k 8)
(defconst emacs-lisply-skewed-search-default-max-snippet-tokens 512)
(defconst emacs-lisply-skewed-search-index-version 1
  "Index format version. Increment when format changes.")

(defvar emacs-lisply-skewed-search--index-cache nil
  "Cached index data keyed by index path and mtime.")


(defun emacs-lisply-skewed-search--index-config (index)
  (cdr (assoc "config" index)))



(defun emacs-lisply-skewed-search--read-index-raw (index-path)
  (when (and index-path (file-exists-p index-path))
    (with-temp-buffer
      (insert-file-contents index-path)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun emacs-lisply-skewed-search--validate-index (index)
  (cond
   ((null index) "Index is nil")
   ((not (assoc "version" index)) "Index missing version")
   ((not (= (cdr (assoc "version" index)) emacs-lisply-skewed-search-index-version))
    (format "Index version mismatch: expected %d, got %s"
            emacs-lisply-skewed-search-index-version
            (cdr (assoc "version" index))))
   ((not (assoc "files" index)) "Index missing files array")
   (t nil)))



(defun emacs-lisply-skewed-search--read-index (index-path)
  (let ((index (emacs-lisply-skewed-search--read-index-raw index-path)))
    (when index
      (setq index (emacs-lisply-skewed-search--normalize-index index))
      (let ((err (emacs-lisply-skewed-search--validate-index index)))
        (when err
          (emacs-lisply-log "WARNING: Index validation: %s" err)
          (setq index nil))))
    index))




(defun emacs-lisply-skewed-search--index-cache-valid-p (path mtime source-map)
  (and emacs-lisply-skewed-search--index-cache
       (equal (plist-get emacs-lisply-skewed-search--index-cache :path) path)
       (equal (plist-get emacs-lisply-skewed-search--index-cache :mtime) mtime)
       (equal (plist-get emacs-lisply-skewed-search--index-cache :source-map) source-map)))


(defun emacs-lisply-skewed-search--assoc-any (key alist)
  (or (assoc key alist)
      (let ((alt (cond
                  ((stringp key) (intern-soft key))
                  ((symbolp key) (symbol-name key)))))
        (and alt (assoc alt alist)))))

(defun emacs-lisply-skewed-search--alist-get (key alist)
  (let ((entry (emacs-lisply-skewed-search--assoc-any key alist)))
    (when entry (cdr entry))))

(defun emacs-lisply-skewed-search--normalize-file-entry (entry)
  (let* ((snippets-entry (emacs-lisply-skewed-search--assoc-any "snippets" entry))
         (snippets (and snippets-entry
                        (emacs-lisply-skewed-search--vector-to-list
                         (cdr snippets-entry)))))
    (when snippets-entry
      (setcdr snippets-entry snippets))
    entry))

(defun emacs-lisply-skewed-search--normalize-index (index)
  (let* ((files-entry (emacs-lisply-skewed-search--assoc-any "files" index))
         (files (and files-entry
                      (emacs-lisply-skewed-search--vector-to-list
                       (cdr files-entry)))))
    (when files-entry
      (setcdr files-entry
              (mapcar #'emacs-lisply-skewed-search--normalize-file-entry files))))
  index)


;;


(defun emacs-lisply-skewed-search--path-candidates (file entry)
  (let* ((repo-root (cdr (assoc :repo-root entry)))
         (source-root (cdr (assoc :root entry)))
         (repo-rel (when repo-root (file-relative-name file repo-root)))
         (source-rel (when source-root (file-relative-name file source-root))))
    (delq nil (list repo-rel source-rel file))))

(defun emacs-lisply-skewed-search--index-entry->source-entry (index-entry source-map)
  (let* ((source-key (emacs-lisply-skewed-search--alist-get "source" index-entry))
         (repo (emacs-lisply-skewed-search--alist-get "repo" index-entry))
         (repo-root (emacs-lisply-skewed-search--alist-get "repo_root" index-entry))
         (root (emacs-lisply-skewed-search--alist-get "root" index-entry))
         (fallback (and source-key
                        (cdr (emacs-lisply-skewed-search--assoc-any source-key source-map))))
         (fallback-entry (car fallback)))
    (list
     (cons :root (or root (and fallback-entry (cdr (assoc :root fallback-entry)))))
     (cons :repo (or repo (and fallback-entry (cdr (assoc :repo fallback-entry)))))
     (cons :repo-root (or repo-root (and fallback-entry (cdr (assoc :repo-root fallback-entry))))))))

(defun emacs-lisply-skewed-search--extract-terms (query)
  (let* ((lower (downcase (format "%s" query)))
         (parts (split-string lower "[^a-z0-9_]+" t))
         (filtered (cl-remove-if (lambda (term) (< (length term) 2)) parts)))
    (if filtered filtered (or parts (list lower)))))

(defun emacs-lisply-skewed-search--unique-terms (terms)
  (let ((copy (copy-sequence terms)))
    (delete-dups copy)))

(defun emacs-lisply-skewed-search--count-occurrences (line term)
  (let ((count 0)
        (start 0)
        (needle (regexp-quote term)))
    (while (string-match needle line start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

(defun emacs-lisply-skewed-search--score (snippet term-count)
  (if (<= term-count 0)
      0.0
    (let* ((terms (plist-get snippet :terms))
           (coverage (min 1.0 (/ (float (hash-table-count terms)) term-count)))
           (density (min 1.0 (/ (float (plist-get snippet :match-count)) 8.0))))
      (min 1.0 (+ (* 0.7 coverage) (* 0.3 density))))))

(defun emacs-lisply-skewed-search--metadata (language section terms)
  (let (items)
    (when language (push (cons "language" language) items))
    (when section (push (cons "section" section) items))
    (when terms (push (cons "tags" (emacs-lisply-skewed-search--to-vector terms)) items))
    (nreverse items)))

(defun emacs-lisply-skewed-search--index-entry-snippets (index-entry)
  (let ((snippets (and index-entry (cdr (assoc "snippets" index-entry)))))
    (cond
     ((vectorp snippets) (append snippets nil))
     ((listp snippets) snippets)
     (t nil))))

(defun emacs-lisply-skewed-search--build-snippet-cache (index source-map)
  (let ((files (cdr (assoc "files" index)))
        (snippet-map (make-hash-table :test 'equal))
        (snippet-count 0))
    (dolist (entry files)
      (let* ((source-key (cdr (assoc "source" entry)))
             (path (cdr (assoc "path" entry)))
             (snippets (emacs-lisply-skewed-search--index-entry-snippets entry)))
        (when (and path snippets)
          (let ((source-entry (emacs-lisply-skewed-search--index-entry->source-entry entry source-map)))
            (dolist (snippet snippets)
              (let* ((snippet-text (or (cdr (assoc "snippet" snippet)) ""))
                     (terms (emacs-lisply-skewed-search--unique-terms
                             (emacs-lisply-skewed-search--extract-terms snippet-text)))
                     (candidate (list :snippet snippet
                                      :file path
                                      :source source-key
                                      :entry source-entry)))
                (dolist (term terms)
                  (push candidate (gethash term snippet-map)))
                (setq snippet-count (1+ snippet-count))))))))
    (when (> snippet-count 0)
      (list :snippet-map snippet-map :snippet-count snippet-count))))

(defun emacs-lisply-skewed-search--get-index-cache
    (&optional _config)
  (let* ((index-path (emacs-lisply-skewed-search--index-path))
         (attrs
          (and index-path (file-exists-p index-path)
               (file-attributes index-path)))
         (mtime (and attrs (file-attribute-modification-time attrs)))
         (cached emacs-lisply-skewed-search--index-cache))
    (if (and cached
             (emacs-lisply-skewed-search--index-cache-valid-p
              index-path mtime (plist-get cached :source-map)))
        cached
      (let* ((index
              (and index-path
                   (emacs-lisply-skewed-search--read-index index-path)))
             (config (and index (emacs-lisply-skewed-search--index-config index)))
             (source-map (and config (emacs-lisply-skewed-search--source-map config)))
             (snippet-cache
              (and index source-map
                   (emacs-lisply-skewed-search--build-snippet-cache
                    index source-map))))
        (setq emacs-lisply-skewed-search--index-cache
              (when index
                (list :path index-path
                      :mtime mtime
                      :index index
                      :config config
                      :snippet-cache snippet-cache
                      :source-map source-map)))
        emacs-lisply-skewed-search--index-cache))))


(defun emacs-lisply-skewed-search--snippet-map-candidates
    (snippet-map terms sources-to-search path-filters allowed-exts
		 exclude-filters)
  (let ((seen (make-hash-table :test 'eq)) (candidates 'nil))
    (dolist (term terms)
      (dolist (candidate (gethash term snippet-map))
	(unless (gethash candidate seen)
	  (puthash candidate t seen)
	  (let*
	      ((file (plist-get candidate :file))
	       (source-key (plist-get candidate :source))
	       (entry (plist-get candidate :entry)))
	    (when
		(and file
		     (or (null sources-to-search)
			 (member source-key sources-to-search))
		     (emacs-lisply-skewed-search--allowed-file-p file
								 allowed-exts)
		     (not
		      (emacs-lisply-skewed-search--excluded-path-p
		       exclude-filters file)))
	      (let
		  ((paths
		    (emacs-lisply-skewed-search--path-candidates file
								 entry)))
		(when
		    (emacs-lisply-skewed-search--path-filters-match-p
		     path-filters paths)
		  (push candidate candidates))))))))
    (nreverse candidates)))


(defun emacs-lisply-skewed-search--scan-index-snippets
    (candidates terms include-metadata hits hit-index &optional max-chars)
  (let ((local-hits hits)
        (local-index hit-index))
    (dolist (candidate candidates)
      (let* ((snippet (plist-get candidate :snippet))
             (file-path (plist-get candidate :file))
             (source-key (plist-get candidate :source))
             (entry (plist-get candidate :entry))
             (snippet-text (or (cdr (assoc "snippet" snippet)) ""))
             (lower (downcase snippet-text))
             (match-count 0)
             (matched (make-hash-table :test 'equal)))
        (dolist (term terms)
          (let ((occ (emacs-lisply-skewed-search--count-occurrences lower term)))
            (when (> occ 0)
              (setq match-count (+ match-count occ))
              (puthash term t matched))))
        (when (> match-count 0)
          (let* ((preview (or (cdr (assoc "preview" snippet)) ""))
                 (language-guess (cdr (assoc "language" snippet)))
                 (section (cdr (assoc "section" snippet)))
                 (tags (cl-subseq terms 0 (min 8 (length terms))))
                 (metadata (and include-metadata
                                (emacs-lisply-skewed-search--metadata
                                 language-guess section tags)))
                 (start-line (or (cdr (assoc "start_line" snippet)) 0))
                 (end-line (or (cdr (assoc "end_line" snippet)) 0))
                 (snippet-struct (list :terms matched :match-count match-count))
                 (final-text (if (and max-chars (> (length snippet-text) max-chars))
                                 (substring snippet-text 0 max-chars)
                               snippet-text)))
            (push
             `(("id" . ,(format "hit-%03d" local-index))
               ("score" . ,(emacs-lisply-skewed-search--score snippet-struct (length terms)))
               ("source" . ,source-key)
               ("repo" . ,(cdr (assoc :repo entry)))
               ("path" . ,(emacs-lisply-skewed-search--normalize-path
                           (or (car (emacs-lisply-skewed-search--path-candidates file-path entry))
                               file-path)))
               ("start_line" . ,(1+ start-line))
               ("end_line" . ,(1+ end-line))
               ("snippet" . ,final-text)
               ("preview" . ,(string-trim preview))
               ("metadata" . ,metadata))
             local-hits)
            (setq local-index (1+ local-index))))))
    (list local-hits local-index)))

(defun emacs-lisply-skewed-search--search (params)
  (let* ((query (cdr (assoc 'query params)))
         (requested-mode (cdr (assoc 'search_mode params)))
         (search-mode (if (string= requested-mode "lexical") "lexical" "lexical"))
         (k (or (cdr (assoc 'k params)) emacs-lisply-skewed-search-default-k))
         (max-tokens (or (cdr (assoc 'max_snippet_tokens params))
                         emacs-lisply-skewed-search-default-max-snippet-tokens))
         (include-metadata (if (assoc 'include_metadata params)
                               (not (eq (cdr (assoc 'include_metadata params)) :json-false))
                             t))
         (language (cdr (assoc 'language params)))
         (cache (emacs-lisply-skewed-search--get-index-cache))
         (index (and cache (plist-get cache :index)))
         (config (and cache (plist-get cache :config)))
         (source-map (and cache (plist-get cache :source-map)))
         (path-filters (emacs-lisply-skewed-search--compile-path-filters
                        (emacs-lisply-skewed-search--vector-to-list
                         (cdr (assoc 'path_filters params)))))
         (sources (emacs-lisply-skewed-search--vector-to-list (cdr (assoc 'sources params))))
         (sources-to-search (and source-map
                                (if (and sources (> (length sources) 0))
                                    (cl-remove-if-not (lambda (key) (assoc key source-map)) sources)
                                  (mapcar #'car source-map))))
         (allowed-exts (and config (emacs-lisply-skewed-search--allowed-exts config language)))
         (exclude-filters (and config (emacs-lisply-skewed-search--config-exclude-filters config)))
         (terms (emacs-lisply-skewed-search--extract-terms query))
         (max-chars (and max-tokens (* max-tokens 4)))
         (snippet-cache (and cache (plist-get cache :snippet-cache)))
         (snippet-map (and snippet-cache (plist-get snippet-cache :snippet-map)))
         (hits '())
         (hit-index 1))
    (let ((missing (delq nil (list (and (null index) "index")
                                   (and (null config) "config")
                                   (and (null source-map) "source-map")
                                   (and (null snippet-map) "snippet-map")))))
      (cond
       (missing
        (list
         (cons "error"
               (format "missing index (%s) (expected: %s)"
                       (string-join missing ", ")
                       emacs-lisply-skewed-search-index-path))))
       (t
        (emacs-lisply-log "skewed_search using index: %s" (emacs-lisply-skewed-search--index-path config))
        (let* ((candidates (emacs-lisply-skewed-search--snippet-map-candidates
                            snippet-map terms sources-to-search path-filters allowed-exts exclude-filters))
               (result (emacs-lisply-skewed-search--scan-index-snippets
                        candidates terms include-metadata hits hit-index max-chars)))
          (setq hits (car result)
                hit-index (cadr result)))
        (setq hits (sort hits (lambda (a b) (> (cdr (assoc "score" a)) (cdr (assoc "score" b))))))
        (list (cons "query" query)
              (cons "search_mode" search-mode)
              (cons "sources" (emacs-lisply-skewed-search--to-vector sources-to-search))
              (cons "hits" (emacs-lisply-skewed-search--to-vector (cl-subseq hits 0 (min k (length hits)))))))))))


(when (featurep 'simple-httpd)
  (defservlet* lisply/skewed-search application/json ()
    "Handle skewed-search endpoint for Lisply."
    (emacs-lisply-log "Handling skewed-search request")
    (let* ((json-input (emacs-lisply-parse-json-body))
           (query (and json-input (cdr (assoc 'query json-input)))))
      (if (not (and query (stringp query) (not (string-empty-p query))))
          (emacs-lisply-send-response
           `(("error" . "Missing required parameter: query")))
        (condition-case err
            (let ((result (emacs-lisply-skewed-search--search json-input)))
              (emacs-lisply-send-response result))
          (error
           (emacs-lisply-send-response
            `(("error" . ,(format "%s" err))))))))))

(provide 'lisply-skewed-search)
;;; lisply-skewed-search.el ends here

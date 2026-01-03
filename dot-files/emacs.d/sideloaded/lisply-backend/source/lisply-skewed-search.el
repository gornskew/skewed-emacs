;;; lisply-skewed-search.el --- skewed_search helpers and endpoint

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
;; skewed_search indexing and query endpoint for Lisply.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'simple-httpd nil t)
(require 'lisply-http-setup nil t)

;; Provide a fallback logger when running outside the full lisply server.
(unless (fboundp 'emacs-lisply-log)
  (defun emacs-lisply-log (fmt &rest args)
    (apply #'message (concat "[skewed-search] " fmt) args)))

;; skewed_search helpers

(defconst emacs-lisply-skewed-search-default-k 8)
(defconst emacs-lisply-skewed-search-default-max-snippet-tokens 512)
(defconst emacs-lisply-skewed-search-max-file-bytes (* 1024 1024))
(defconst emacs-lisply-skewed-search-context-lines 3)
(defconst emacs-lisply-skewed-search-merge-gap 3)
(defconst emacs-lisply-skewed-search-ignore-dirs
  '(".git" "node_modules" "dist" "build" "vendor" "target" ".cache" "logs" "tmp" "docker" "docker-v2"))

(defconst emacs-lisply-skewed-search-default-exts
  '(".lisp" ".lsp" ".cl" ".gdl" ".gendl" ".asd" ".isc"
    ".md" ".markdown" ".org" ".txt" ".rst"
    ".el" ".js" ".ts" ".json" ".yml" ".yaml" ".html" ".css"))

(defconst emacs-lisply-skewed-search-language-exts
  '(("lisp" . (".lisp" ".lsp" ".cl" ".asd" ".el"))
    ("gendl" . (".gendl"))
    ("gdl" . (".gdl" ".gendl" ".lisp" ".lsp" ".cl"))
    ("markdown" . (".md" ".markdown" ".org" ".rst"))))

(defcustom emacs-lisply-skewed-search-index-path "/tmp/lisply-skewed-search-index.json"
  "Path to write the optional skewed_search index."
  :type 'string
  :group 'emacs-lisply)

(defcustom emacs-lisply-skewed-search-config-path
  "/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/skewed-search-config.json"
  "Path to skewed_search config JSON file."
  :type 'string
  :group 'emacs-lisply)

(defcustom emacs-lisply-skewed-search-preextract-snippets nil
  "When non-nil, include pre-extracted snippets in the skewed_search index."
  :type 'boolean
  :group 'emacs-lisply)

(defcustom emacs-lisply-skewed-search-preextract-max-lines 24
  "Max lines per pre-extracted snippet."
  :type 'integer
  :group 'emacs-lisply)

(defcustom emacs-lisply-skewed-search-preextract-max-chars 1200
  "Max characters per pre-extracted snippet."
  :type 'integer
  :group 'emacs-lisply)

(defvar emacs-lisply-skewed-search--index-cache nil
  "Cached index data keyed by index path and mtime.")

(defun emacs-lisply-skewed-search--index-path ()
  (let* ((config (emacs-lisply-skewed-search--read-config))
         (override (emacs-lisply-skewed-search--config-value config "index_path" nil)))
    (or override emacs-lisply-skewed-search-index-path)))

(defun emacs-lisply-skewed-search--index-cache-valid-p (path mtime source-map)
  (and emacs-lisply-skewed-search--index-cache
       (equal (plist-get emacs-lisply-skewed-search--index-cache :path) path)
       (equal (plist-get emacs-lisply-skewed-search--index-cache :mtime) mtime)
       (equal (plist-get emacs-lisply-skewed-search--index-cache :source-map) source-map)))

(defun emacs-lisply-skewed-search--projects-root ()
  (let* ((candidates (list "/projects" "/home/emacs-user/projects" (expand-file-name "~/projects")))
         (found (cl-find-if (lambda (root)
                              (file-exists-p (expand-file-name "gendl" root)))
                            candidates)))
    (or found "/projects")))

(defun emacs-lisply-skewed-search--read-config ()
  (when (and emacs-lisply-skewed-search-config-path
             (file-exists-p emacs-lisply-skewed-search-config-path))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'string))
      (json-read-file emacs-lisply-skewed-search-config-path))))

(defun emacs-lisply-skewed-search--read-index-raw ()
  (let ((index-path (emacs-lisply-skewed-search--index-path)))
    (when (and index-path (file-exists-p index-path))
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'string))
        (json-read-file index-path)))))

(defun emacs-lisply-skewed-search--read-index ()
  "Read and validate the index."
  (let ((index (emacs-lisply-skewed-search--read-index-raw)))
    (when index
      (let ((err (emacs-lisply-skewed-search--validate-index index)))
        (when err
          (emacs-lisply-log "WARNING: Index validation: %s" err))))
    index))

(defun emacs-lisply-skewed-search--preextract-enabled (config)
  (let ((value (emacs-lisply-skewed-search--config-value config "preextract_snippets" nil)))
    (if (null value)
        emacs-lisply-skewed-search-preextract-snippets
      (not (eq value :json-false)))))

(defun emacs-lisply-skewed-search--preextract-max-lines (config)
  (let ((value (emacs-lisply-skewed-search--config-value config "preextract_max_lines" nil)))
    (or value emacs-lisply-skewed-search-preextract-max-lines)))

(defun emacs-lisply-skewed-search--preextract-max-chars (config)
  (let ((value (emacs-lisply-skewed-search--config-value config "preextract_max_chars" nil)))
    (or value emacs-lisply-skewed-search-preextract-max-chars)))

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
                           (repo-root (cdr (assoc "repo_root" entry)))
                           (root-path (if (and path (file-name-absolute-p path))
                                          path
                                        (expand-file-name (or path "") root)))
                           (repo-root-path (if (and repo-root (file-name-absolute-p repo-root))
                                               repo-root
                                             (expand-file-name (or repo-root (or path "")) root))))
                      `((:root . ,root-path)
                        (:repo . ,repo)
                        (:repo-root . ,repo-root-path))))
                  entries))))
       sources))))

(defun emacs-lisply-skewed-search--warn-missing-sources (source-map)
  (dolist (source source-map)
    (let ((source-key (car source))
          (entries (cdr source)))
      (dolist (entry entries)
        (let ((root (cdr (assoc :root entry))))
          (unless (and root (file-exists-p root))
            (emacs-lisply-log "skewed_search source missing: %s (%s)" (or root "nil") source-key)))))))

(defun emacs-lisply-skewed-search--source-map ()
  (let* ((root (emacs-lisply-skewed-search--projects-root))
         (config (emacs-lisply-skewed-search--read-config))
         (config-sources (emacs-lisply-skewed-search--config-sources config root)))
    (if config-sources
        (progn
          (emacs-lisply-skewed-search--warn-missing-sources config-sources)
          config-sources)
      (let* ((gendl-root (expand-file-name "gendl" root))
             (gdl-root (expand-file-name "gdl" root))
             (skewed-root (expand-file-name "skewed-emacs" root))
             (tutorials-root (expand-file-name "tutorials" root))
             (training-root (expand-file-name "training" root)))
        (emacs-lisply-skewed-search--warn-missing-sources
         `(("gdl-docs" . (((:root . ,(expand-file-name "docs" gendl-root))
                           (:repo . "gendl")
                           (:repo-root . ,gendl-root))))
           ("gendl-src" . (((:root . ,gendl-root)
                            (:repo . "gendl")
                            (:repo-root . ,gendl-root))))
           ("skewed-emacs-docs" . (((:root . ,skewed-root)
                                    (:repo . "skewed-emacs")
                                    (:repo-root . ,skewed-root))))
           ("examples" . (((:root . ,(expand-file-name "demos" gendl-root))
                           (:repo . "gendl")
                           (:repo-root . ,gendl-root))
                          ((:root . ,tutorials-root)
                           (:repo . "tutorials")
                           (:repo-root . ,tutorials-root))
                          ((:root . ,training-root)
                           (:repo . "training")
                           (:repo-root . ,training-root))
                          ((:root . ,gdl-root)
                           (:repo . "gdl")
                           (:repo-root . ,gdl-root))))))
        `(("gdl-docs" . (((:root . ,(expand-file-name "docs" gendl-root))
                          (:repo . "gendl")
                          (:repo-root . ,gendl-root))))
          ("gendl-src" . (((:root . ,gendl-root)
                           (:repo . "gendl")
                           (:repo-root . ,gendl-root))))
          ("skewed-emacs-docs" . (((:root . ,skewed-root)
                                   (:repo . "skewed-emacs")
                                   (:repo-root . ,skewed-root))))
          ("examples" . (((:root . ,(expand-file-name "demos" gendl-root))
                          (:repo . "gendl")
                          (:repo-root . ,gendl-root))
                         ((:root . ,tutorials-root)
                          (:repo . "tutorials")
                          (:repo-root . ,tutorials-root))
                         ((:root . ,training-root)
                          (:repo . "training")
                          (:repo-root . ,training-root))
                         ((:root . ,gdl-root)
                          (:repo . "gdl")
                          (:repo-root . ,gdl-root)))))))))

(defun emacs-lisply-skewed-search--normalize-path (path)
  (replace-regexp-in-string "\\\\" "/" path))

(defun emacs-lisply-skewed-search--path-candidates-simple (path)
  (let* ((root (emacs-lisply-skewed-search--projects-root))
         (root-dir (file-name-as-directory root))
         (relative (when (and root (string-prefix-p root-dir path))
                     (file-relative-name path root)))
         (normalized (emacs-lisply-skewed-search--normalize-path path)))
    (delq nil (list normalized
                    (and relative (emacs-lisply-skewed-search--normalize-path relative))))))

(defun emacs-lisply-skewed-search--compile-path-filters (filters)
  (let (compiled)
    (dolist (filter filters (nreverse compiled))
      (when (and (stringp filter) (not (string-empty-p filter)))
        (let ((normalized (emacs-lisply-skewed-search--normalize-path filter)))
          (if (string-match-p "[*?]" normalized)
              (push (cons 'regex (wildcard-to-regexp normalized)) compiled)
            (push (cons 'prefix normalized) compiled)))))))

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

(defun emacs-lisply-skewed-search--allowed-exts (language)
  (let* ((config (emacs-lisply-skewed-search--read-config))
         (exts (emacs-lisply-skewed-search--config-exts config))
         (key (and language (downcase (format "%s" language))))
         (entry (assoc key exts)))
    (or (cdr entry) (cdr (assoc "default" exts)))))

(defun emacs-lisply-skewed-search--file-ext (path)
  (downcase (file-name-extension path t)))

(defun emacs-lisply-skewed-search--allowed-file-p (path allowed-exts)
  (member (emacs-lisply-skewed-search--file-ext path) allowed-exts))

(defun emacs-lisply-skewed-search--config-ignore-dirs (config)
  (let ((ignore (emacs-lisply-skewed-search--config-value config "ignore_dirs" nil)))
    (or ignore emacs-lisply-skewed-search-ignore-dirs)))

(defun emacs-lisply-skewed-search--config-exclude-filters (config)
  (let ((exclude (emacs-lisply-skewed-search--config-value config "exclude_paths" nil)))
    (emacs-lisply-skewed-search--compile-path-filters
     (emacs-lisply-skewed-search--vector-to-list exclude))))

(defun emacs-lisply-skewed-search--excluded-path-p (exclude-filters path)
  (and exclude-filters
       (not (null exclude-filters))
       (emacs-lisply-skewed-search--path-filters-match-p
        exclude-filters
        (emacs-lisply-skewed-search--path-candidates-simple path))))

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
               (not (emacs-lisply-skewed-search--excluded-path-p
                     exclude-filters
                     (emacs-lisply-skewed-search--normalize-path entry))))
          (push entry results)))))
    results))

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

(defun emacs-lisply-skewed-search--build-snippets (lines terms max-chars)
  (let (matches)
    (cl-loop for line in lines
             for idx from 0 do
             (let ((lower (downcase line))
                   (count 0)
                   (matched (make-hash-table :test 'equal)))
               (dolist (term terms)
                 (let ((occ (emacs-lisply-skewed-search--count-occurrences lower term)))
                   (when (> occ 0)
                     (setq count (+ count occ))
                     (puthash term t matched))))
               (when (> count 0)
                 (push (list :line idx :count count :terms matched) matches))))
    (setq matches (nreverse matches))
    (let (snippets current)
      (dolist (match matches)
        (let ((line (plist-get match :line)))
          (if (or (null current)
                  (> line (+ (plist-get current :last-line) emacs-lisply-skewed-search-merge-gap)))
              (progn
                (when current (push current snippets))
                (setq current (list :first-line line
                                    :last-line line
                                    :match-count (plist-get match :count)
                                    :terms (plist-get match :terms))))
            (setf (plist-get current :last-line) line)
            (setf (plist-get current :match-count)
                  (+ (plist-get current :match-count) (plist-get match :count)))
            (maphash (lambda (k _v) (puthash k t (plist-get current :terms)))
                     (plist-get match :terms)))))
      (when current (push current snippets))
      (setq snippets (nreverse snippets))
      (mapcar
       (lambda (snippet)
         (let* ((start-line (max 0 (- (plist-get snippet :first-line) emacs-lisply-skewed-search-context-lines)))
                (end-line (min (1- (length lines))
                               (+ (plist-get snippet :last-line) emacs-lisply-skewed-search-context-lines)))
                (slice (cl-subseq lines start-line (1+ end-line)))
                (trimmed (emacs-lisply-skewed-search--trim-lines slice max-chars)))
           (list :start-line start-line
                 :end-line (+ start-line (1- (length trimmed)))
                 :lines trimmed
                 :match-count (plist-get snippet :match-count)
                 :terms (plist-get snippet :terms))))
       snippets))))

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

(defun emacs-lisply-skewed-search--score (snippet term-count)
  (if (<= term-count 0)
      0.0
    (let* ((terms (plist-get snippet :terms))
           (coverage (min 1.0 (/ (float (hash-table-count terms)) term-count)))
           (density (min 1.0 (/ (float (plist-get snippet :match-count)) 8.0))))
      (min 1.0 (+ (* 0.7 coverage) (* 0.3 density))))))

(defun emacs-lisply-skewed-search--vector-to-list (value)
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun emacs-lisply-skewed-search--to-vector (items)
  (if items (vconcat items) []))

(defun emacs-lisply-skewed-search--path-candidates (file entry)
  (let* ((repo-root (cdr (assoc :repo-root entry)))
         (source-root (cdr (assoc :root entry)))
         (repo-rel (when repo-root (file-relative-name file repo-root)))
         (source-rel (when source-root (file-relative-name file source-root))))
    (delq nil (list repo-rel source-rel file))))

(defun emacs-lisply-skewed-search--index-entry->source-entry (index-entry source-map)
  (let* ((source-key (cdr (assoc "source" index-entry)))
         (repo (cdr (assoc "repo" index-entry)))
         (repo-root (cdr (assoc "repo_root" index-entry)))
         (root (cdr (assoc "root" index-entry)))
         (fallback (and source-key (cdr (assoc source-key source-map))))
         (fallback-entry (car fallback)))
    (list
     (cons :root (or root (and fallback-entry (cdr (assoc :root fallback-entry)))))
     (cons :repo (or repo (and fallback-entry (cdr (assoc :repo fallback-entry)))))
     (cons :repo-root (or repo-root (and fallback-entry (cdr (assoc :repo-root fallback-entry))))))))

(defun emacs-lisply-skewed-search--index-files (index sources-to-search path-filters allowed-exts exclude-filters source-map)
  (let ((files (cdr (assoc "files" index)))
        (results '()))
    (dolist (entry files (nreverse results))
      (let* ((source-key (cdr (assoc "source" entry)))
             (path (cdr (assoc "path" entry))))
        (when (and path
                   (or (null sources-to-search) (member source-key sources-to-search))
                   (emacs-lisply-skewed-search--allowed-file-p path allowed-exts)
                   (not (emacs-lisply-skewed-search--excluded-path-p
                         exclude-filters
                         (emacs-lisply-skewed-search--normalize-path path))))
          (let* ((source-entry (emacs-lisply-skewed-search--index-entry->source-entry entry source-map))
                 (candidates (emacs-lisply-skewed-search--path-candidates path source-entry)))
            (when (emacs-lisply-skewed-search--path-filters-match-p path-filters candidates)
              (push (list :file path :source source-key :entry source-entry :index-entry entry) results))))))))

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
                (setq snippet-count (1+ snippet-count)))))))
      (when (> snippet-count 0)
	(list :snippet-map snippet-map :snippet-count snippet-count)))))

(defun emacs-lisply-skewed-search--get-index-cache (source-map)
  (let* ((index-path (emacs-lisply-skewed-search--index-path))
         (attrs (and index-path (file-exists-p index-path) (file-attributes index-path)))
         (mtime (and attrs (file-attribute-modification-time attrs))))
    (if (and index-path mtime (emacs-lisply-skewed-search--index-cache-valid-p index-path mtime source-map))
        emacs-lisply-skewed-search--index-cache
      (let ((index (and index-path (emacs-lisply-skewed-search--read-index-raw))))
        (setq emacs-lisply-skewed-search--index-cache
              (when index
                (let ((snippet-cache (emacs-lisply-skewed-search--build-snippet-cache index source-map)))
                  (list :path index-path
                        :mtime mtime
                        :index index
                        :snippet-cache snippet-cache
                        :source-map source-map))))
        emacs-lisply-skewed-search--index-cache))))

(defun emacs-lisply-skewed-search--snippet-map-candidates (snippet-map terms sources-to-search path-filters allowed-exts exclude-filters)
  (let ((seen (make-hash-table :test 'eq))
        (candidates '()))
    (dolist (term terms)
      (dolist (candidate (gethash term snippet-map))
        (unless (gethash candidate seen)
          (puthash candidate t seen)
          (let* ((file (plist-get candidate :file))
                 (source-key (plist-get candidate :source))
                 (entry (plist-get candidate :entry)))
            (when (and file
                       (or (null sources-to-search) (member source-key sources-to-search))
                       (emacs-lisply-skewed-search--allowed-file-p file allowed-exts)
                       (not (emacs-lisply-skewed-search--excluded-path-p
                             exclude-filters
                             (emacs-lisply-skewed-search--normalize-path file))))
              (let ((paths (emacs-lisply-skewed-search--path-candidates file entry)))
                (when (emacs-lisply-skewed-search--path-filters-match-p path-filters paths)
                  (push candidate candidates)))))))
      (nreverse candidates))))

(defun emacs-lisply-skewed-search--scan-index-snippets (candidates terms include-metadata hits hit-index)
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
                 (language-guess (or (cdr (assoc "language" snippet))
                                     (emacs-lisply-skewed-search--guess-language file-path)))
                 (section (cdr (assoc "section" snippet)))
                 (tags (cl-subseq terms 0 (min 8 (length terms))))
                 (metadata (and include-metadata
                                (emacs-lisply-skewed-search--metadata
                                 language-guess section tags)))
                 (start-line (or (cdr (assoc "start_line" snippet)) 0))
                 (end-line (or (cdr (assoc "end_line" snippet)) 0))
                 (snippet-struct (list :terms matched :match-count match-count)))
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
               ("snippet" . ,snippet-text)
               ("preview" . ,(string-trim preview))
               ("metadata" . ,metadata))
             local-hits)
            (setq local-index (1+ local-index))))))
    (list local-hits local-index)))

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
(defun emacs-lisply-skewed-search--scan-file (file source-key entry terms max-chars include-metadata hits hit-index)
  (let ((local-hits hits)
        (local-index hit-index))
    (when (and file (file-exists-p file))
      (let* ((attrs (file-attributes file))
             (size (file-attribute-size attrs)))
        (when (and size (< size emacs-lisply-skewed-search-max-file-bytes))
          (with-temp-buffer
            (insert-file-contents file)
            (let* ((lines (split-string (buffer-string) "\n" nil))
                   (snippets (emacs-lisply-skewed-search--build-snippets lines terms max-chars)))
              (dolist (snippet snippets)
                (let* ((snippet-lines (plist-get snippet :lines))
                       (snippet-text (mapconcat #'identity snippet-lines "\n"))
                       (preview (or (cl-find-if (lambda (line) (> (length (string-trim line)) 0))
                                                snippet-lines)
                                    ""))
                       (language-guess (emacs-lisply-skewed-search--guess-language file))
                       (section (emacs-lisply-skewed-search--section-heading
                                 lines (plist-get snippet :start-line) file))
                       (tags (cl-subseq terms 0 (min 8 (length terms))))
                       (metadata (and include-metadata
                                      (emacs-lisply-skewed-search--metadata
                                       language-guess section tags))))
                  (push
                   `(("id" . ,(format "hit-%03d" local-index))
                     ("score" . ,(emacs-lisply-skewed-search--score snippet (length terms)))
                     ("source" . ,source-key)
                     ("repo" . ,(cdr (assoc :repo entry)))
                     ("path" . ,(emacs-lisply-skewed-search--normalize-path
                                 (or (car (emacs-lisply-skewed-search--path-candidates file entry))
                                     file)))
                     ("start_line" . ,(1+ (plist-get snippet :start-line)))
                     ("end_line" . ,(1+ (plist-get snippet :end-line)))
                     ("snippet" . ,snippet-text)
                     ("preview" . ,(string-trim preview))
                     ("metadata" . ,metadata))
                   local-hits)
                  (setq local-index (1+ local-index)))))))))
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
         (config (emacs-lisply-skewed-search--read-config))
         (path-filters (emacs-lisply-skewed-search--compile-path-filters
                        (emacs-lisply-skewed-search--vector-to-list
                         (cdr (assoc 'path_filters params)))))
         (sources (emacs-lisply-skewed-search--vector-to-list (cdr (assoc 'sources params))))
         (source-map (emacs-lisply-skewed-search--source-map))
         (sources-to-search (if (and sources (> (length sources) 0))
                                (cl-remove-if-not (lambda (key) (assoc key source-map)) sources)
                              (mapcar #'car source-map)))
         (allowed-exts (emacs-lisply-skewed-search--allowed-exts language))
         (ignore-dirs (emacs-lisply-skewed-search--config-ignore-dirs config))
         (exclude-filters (emacs-lisply-skewed-search--config-exclude-filters config))
         (terms (emacs-lisply-skewed-search--extract-terms query))
         (max-chars (and max-tokens (* max-tokens 4)))
         (cache (or (emacs-lisply-skewed-search--get-index-cache source-map)
                    (condition-case err
                        (progn
                          (emacs-lisply-log "skewed_search index missing; building on demand")
                          (emacs-lisply-skewed-search-build-index)
                          (emacs-lisply-skewed-search--get-index-cache source-map))
                      (error
                       (emacs-lisply-log "skewed_search index build failed: %s" err)
                       nil))))
         (index (and cache (plist-get cache :index)))
         (snippet-cache (and cache (plist-get cache :snippet-cache)))
         (indexed-files (and index
                             (emacs-lisply-skewed-search--index-files
                              index sources-to-search path-filters allowed-exts exclude-filters source-map)))
         (hits '())
         (hit-index 1))
    (when indexed-files
      (emacs-lisply-log "skewed_search using index: %s" (emacs-lisply-skewed-search--index-path)))
    (if indexed-files
        (if (and snippet-cache (plist-get snippet-cache :snippet-map))
            (let* ((snippet-map (plist-get snippet-cache :snippet-map))
                   (candidates (emacs-lisply-skewed-search--snippet-map-candidates
                                snippet-map terms sources-to-search path-filters allowed-exts exclude-filters))
                   (result (emacs-lisply-skewed-search--scan-index-snippets
                            candidates terms include-metadata hits hit-index)))
              (setq hits (car result)
                    hit-index (cadr result)))
          (dolist (item indexed-files)
            (let* ((file (plist-get item :file))
                   (index-entry (plist-get item :index-entry))
                   (snippets (emacs-lisply-skewed-search--index-entry-snippets index-entry))
                   (candidates (when snippets
                                 (mapcar (lambda (snippet)
                                           (list :snippet snippet
                                                 :file file
                                                 :source (plist-get item :source)
                                                 :entry (plist-get item :entry)))
                                         snippets)))
                   (result (if candidates
                               (emacs-lisply-skewed-search--scan-index-snippets
                                candidates terms include-metadata hits hit-index)
                             (emacs-lisply-skewed-search--scan-file
                              file (plist-get item :source) (plist-get item :entry)
                              terms max-chars include-metadata hits hit-index))))
              (setq hits (car result)
                    hit-index (cadr result)))))
      (dolist (source-key sources-to-search)
        (let ((entries (cdr (assoc source-key source-map))))
          (dolist (entry entries)
            (let ((root (cdr (assoc :root entry))))
              (when (and root (file-exists-p root))
                (dolist (file (emacs-lisply-skewed-search--list-files root allowed-exts ignore-dirs exclude-filters))
                  (when (and (emacs-lisply-skewed-search--allowed-file-p file allowed-exts)
                             (emacs-lisply-skewed-search--path-filters-match-p
                              path-filters
                              (emacs-lisply-skewed-search--path-candidates file entry)))
                    (let ((result (emacs-lisply-skewed-search--scan-file
                                   file source-key entry
                                   terms max-chars include-metadata hits hit-index)))
                      (setq hits (car result)
                            hit-index (cadr result)))))))))))
    (setq hits (sort hits (lambda (a b) (> (cdr (assoc "score" a)) (cdr (assoc "score" b))))))
    `(("query" . ,query)
      ("search_mode" . ,search-mode)
      ("sources" . ,(emacs-lisply-skewed-search--to-vector sources-to-search))
      ("hits" . ,(emacs-lisply-skewed-search--to-vector (cl-subseq hits 0 (min k (length hits))))))))


(defconst emacs-lisply-skewed-search-index-version 1
  "Index format version. Increment when format changes.")

(defun emacs-lisply-skewed-search--validate-sources (source-map)
  "Validate that all configured sources exist. Returns list of missing sources."
  (let (missing)
    (dolist (source source-map)
      (let ((source-key (car source))
            (entries (cdr source)))
        (dolist (entry entries)
          (let ((root (cdr (assoc :root entry))))
            (unless (and root (file-exists-p root))
              (push (list :source source-key :path root) missing))))))
    (nreverse missing)))

(defun emacs-lisply-skewed-search--compute-checksum (files)
  "Compute a simple checksum from file count and total size."
  (let ((count (length files))
        (total-size 0))
    (dolist (file files)
      (let ((path (cdr (assoc "path" file))))
        (when (and path (file-exists-p path))
          (setq total-size (+ total-size 
                              (or (file-attribute-size (file-attributes path)) 0))))))
    (format "%d-%d" count total-size)))

(defun emacs-lisply-skewed-search--validate-index (index)
  "Validate index structure and version. Returns nil if valid, error string if invalid."
  (cond
   ((null index) "Index is nil")
   ((not (assoc "version" index))
    "Index missing version (pre-v1 format)")
   ((not (= (cdr (assoc "version" index)) emacs-lisply-skewed-search-index-version))
    (format "Index version mismatch: expected %d, got %s"
            emacs-lisply-skewed-search-index-version
            (cdr (assoc "version" index))))
   ((not (assoc "files" index)) "Index missing files array")
   (t nil)))

(defun emacs-lisply-skewed-search-build-index ()
  "Write a lightweight index snapshot to `emacs-lisply-skewed-search-index-path`."
  (interactive)
 
    ;; Validate sources exist
    (let ((missing (emacs-lisply-skewed-search--validate-sources (emacs-lisply-skewed-search--source-map))))
      (when missing
        (emacs-lisply-log "WARNING: Missing sources: %S" missing)))
     (let* ((config (emacs-lisply-skewed-search--read-config))
         (index-path (emacs-lisply-skewed-search--index-path))
         (source-map (emacs-lisply-skewed-search--source-map))
         (allowed-exts (emacs-lisply-skewed-search--allowed-exts nil))
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
    (with-temp-file index-path
      (insert (json-encode `(("generated_at" . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))
                           ("version" . ,emacs-lisply-skewed-search-index-version)
                             ("checksum" . ,(emacs-lisply-skewed-search--compute-checksum (nreverse items)))
                               ("files" . ,(vconcat (nreverse items)))))))))

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

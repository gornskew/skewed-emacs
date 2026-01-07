;;; lisply-skewed-search.el --- skewed_search helpers and endpoint -*- lexical-binding: t; -*-

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
;; skewed_search index-only lookup and query endpoint for Lisply.

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

(defconst emacs-lisply-skewed-search-default-k 8)
(defconst emacs-lisply-skewed-search-default-max-snippet-tokens 512)
(defconst emacs-lisply-skewed-search-merge-gap 3)
(defconst emacs-lisply-skewed-search-context-lines 3)

(defcustom emacs-lisply-skewed-search-config-path
  (expand-file-name "~/.emacs.d/sideloaded/lisply-backend/skewed-search-config.json")
  "Path to skewed_search config JSON file."
  :type 'string
  :group 'emacs-lisply)

(setq emacs-lisply-skewed-search-config-path
      (expand-file-name "~/.emacs.d/sideloaded/lisply-backend/skewed-search-config.json"))
(defconst emacs-lisply-skewed-search-index-version 1
  "Index format version. Increment when format changes.")

(defvar emacs-lisply-skewed-search--index-cache nil
  "Cached index data keyed by index path and mtime.")

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
  (let* ((candidates (list "/projects" "/home/emacs-user/projects" (expand-file-name "~/projects")))
         (found (cl-find-if (lambda (root) (file-exists-p (expand-file-name "gendl" root)))
                            candidates)))
    (or found "/projects")))



(defun emacs-lisply-skewed-search--read-config ()
  (when (and emacs-lisply-skewed-search-config-path
             (file-exists-p emacs-lisply-skewed-search-config-path))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'string))
      (json-read-file emacs-lisply-skewed-search-config-path))))



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

(defun emacs-lisply-skewed-search--source-map (&optional config)
  (let*
      ((root (emacs-lisply-skewed-search--projects-root))
       (config (or config (emacs-lisply-skewed-search--read-config)))
       (config-sources
	(emacs-lisply-skewed-search--config-sources config root)))
    (when config-sources config-sources)))


(defun emacs-lisply-skewed-search--index-path (&optional config)
  (let* ((config (or config (emacs-lisply-skewed-search--read-config)))
         (configured (emacs-lisply-skewed-search--config-value config "index_path" nil)))
    (when configured
      (if (file-name-absolute-p configured)
          configured
        (expand-file-name configured
                          (file-name-directory emacs-lisply-skewed-search-config-path))))))




(defun emacs-lisply-skewed-search--read-index-raw (index-path)
  (when (and index-path (file-exists-p index-path))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'string))
      (json-read-file index-path))))

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

(defun emacs-lisply-skewed-search--normalize-path (path)
  (replace-regexp-in-string "\\\\" "/" path))

(defun emacs-lisply-skewed-search--vector-to-list (value)
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun emacs-lisply-skewed-search--to-vector (items)
  (if items (vconcat items) []))

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

(defun emacs-lisply-skewed-search--allowed-exts
    (&optional config language)
  (let*
      ((config-arg config) (language-arg language)
       (config
	(if (and config-arg (not (listp config-arg)))
	    (emacs-lisply-skewed-search--read-config)
	  config-arg))
       (language
	(if (and config-arg (not (listp config-arg))) config-arg
	  language-arg))
       (exts (emacs-lisply-skewed-search--config-exts config))
       (key (and language (downcase (format "%s" language))))
       (entry (assoc key exts)))
    (or (cdr entry) (cdr (assoc "default" exts)))))


(defun emacs-lisply-skewed-search--file-ext (path)
  (downcase (file-name-extension path t)))

(defun emacs-lisply-skewed-search--allowed-file-p (path allowed-exts)
  (member (emacs-lisply-skewed-search--file-ext path) allowed-exts))

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

(defun emacs-lisply-skewed-search--config-exclude-filters (config)
  (let ((exclude (emacs-lisply-skewed-search--config-value config "exclude_paths" nil)))
    (emacs-lisply-skewed-search--compile-path-filters
     (emacs-lisply-skewed-search--vector-to-list exclude))))

(defun emacs-lisply-skewed-search--excluded-path-p (exclude-filters path)
  (and exclude-filters
       (not (null exclude-filters))
       (emacs-lisply-skewed-search--path-filters-match-p
        exclude-filters
        (list (emacs-lisply-skewed-search--normalize-path path)))))

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
    (source-map &optional config)
  (let*
      ((config (or config (emacs-lisply-skewed-search--read-config)))
       (index-path (emacs-lisply-skewed-search--index-path config))
       (attrs
	(and index-path (file-exists-p index-path)
	     (file-attributes index-path)))
       (mtime (and attrs (file-attribute-modification-time attrs))))
    (if
	(and index-path mtime
	     (emacs-lisply-skewed-search--index-cache-valid-p
	      index-path mtime source-map))
	emacs-lisply-skewed-search--index-cache
      (let
	  ((index
	    (and index-path
		 (emacs-lisply-skewed-search--read-index index-path))))
	(setq emacs-lisply-skewed-search--index-cache
	      (when index
		(let
		    ((snippet-cache
		      (emacs-lisply-skewed-search--build-snippet-cache
		       index source-map)))
		  (list :path index-path :mtime mtime :index index
			:snippet-cache snippet-cache :source-map
			source-map))))
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
         (config (emacs-lisply-skewed-search--read-config))
         (source-map (and config (emacs-lisply-skewed-search--source-map config)))
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
         (cache (and source-map (emacs-lisply-skewed-search--get-index-cache source-map config)))
         (index (and cache (plist-get cache :index)))
         (snippet-cache (and cache (plist-get cache :snippet-cache)))
         (snippet-map (and snippet-cache (plist-get snippet-cache :snippet-map)))
         (hits '())
         (hit-index 1))
    (let ((missing (delq nil (list (and (null config) "config")
                                   (and (null source-map) "source-map")
                                   (and (null index) "index")
                                   (and (null snippet-map) "snippet-map")))))
      (cond
       (missing
        (list
         (cons "error"
               (format "missing index (%s) (expected: %s via config %s)"
                       (string-join missing ", ")
                       "/home/emacs-user/.emacs.d/sideloaded/lisply-backend/skewed-search-index.json"
                       emacs-lisply-skewed-search-config-path))))
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

;;; lisply-endpoints.el --- Lisply protocol endpoints for Emacs integration

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
;; This file defines the Lisply protocol endpoints for Emacs,
;; compatible with the Model Context Protocol for LLM integration.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'simple-httpd)
(require 'lisply-http-setup)

;; Lisply Tool Definitions

(defvar emacs-lisply-tools nil
  "List of tool definitions for the Emacs Lisply server.")

(setq emacs-lisply-tools
  (vector
   ;; ping_lisp tool (generic name)
   `(("name" . "ping_lisp")
     ("description" . "Check if the Lisp server is pingable")
     ("inputSchema" . (("type" . "object")
                       ("properties" . ,(make-hash-table))
                       ("required" . []))))

   ;; lisp_eval tool (generic name for consistent with Gendl)
   `(("name" . "lisp_eval")
     ("description" . "Evaluate Emacs Lisp code")
     ("inputSchema" . (("type" . "object")
                       ("properties" . (("code" . (("type" . "string")
                                                 ("description" . "The Emacs Lisp code to evaluate")))
                                       ("package" . (("type" . "string")
                                                   ("description" . "Not used in Emacs Lisp but kept for protocol compatibility")))))
                       ("required" . ["code"]))))

   ;; gdl_search tool
   `(("name" . "gdl_search")
     ("description" . "Unified search over GDL / GenDL / related docs & code")
     ("inputSchema" . (("type" . "object")
                       ("properties" . (("query" . (("type" . "string")
                                                   ("description" . "Natural-language or keyword query")))
                                       ("k" . (("type" . "integer")
                                               ("description" . "Max number of hits to return (default 8)")))
                                       ("sources" . (("type" . "array")
                                                     ("items" . (("type" . "string")))
                                                     ("description" . "Logical sources to restrict search")))
                                       ("path_filters" . (("type" . "array")
                                                          ("items" . (("type" . "string")))
                                                          ("description" . "Prefix or glob-style path filters")))
                                       ("language" . (("type" . "string")
                                                     ("description" . "Language hint, e.g. lisp, gdl, markdown")))
                                       ("search_mode" . (("type" . "string")
                                                        ("description" . "lexical, semantic, or hybrid (default lexical)")))
                                       ("max_snippet_tokens" . (("type" . "integer")
                                                                ("description" . "Soft cap for snippet length")))
                                       ("include_metadata" . (("type" . "boolean")
                                                              ("description" . "Include metadata in hits (default true)")))))
                       ("required" . ["query"]))))


   ;;
   ;; FLAG -- http_request is implemented in middleware, not in this backend.
   ;;
   
   
   ))

(defun emacs-lisply-generate-tool-description ()
  "Generate a tool description for LLM integration."
  `(("tools" . ,emacs-lisply-tools)))

;; Lisply HTTP handlers

(defservlet* lisply/ping-lisp text/plain ()
  "Handle generic ping-lisp endpoint for Lisply."
  (emacs-lisply-log "Handling ping-lisp request")
  (insert "pong"))

;; Note: The defservlet* macro already constructs paths using emacs-lisply-endpoint-prefix
;; for prefix "lisply/" but we keep the variables for documentation and API alignment

(defservlet* lisply/resources/list application/json ()
  "Handle resources/list endpoint for Lisply."
  (emacs-lisply-log "Handling tools/resources request")
  (emacs-lisply-send-response nil)) ;; no resources for you yet.

(defservlet* lisply/prompts/list application/json ()
  "Handle prompts/list endpoint for Lisply."
  (emacs-lisply-log "Handling prompts/resources request")
  (emacs-lisply-send-response nil)) ;; no prompts for you yet.

(defservlet* lisply/tools/list application/json ()
  "Handle tools tools/list endpoint for Lisply."
  (emacs-lisply-log "Handling tools/list request")
  (emacs-lisply-send-response (emacs-lisply-generate-tool-description)))

(defservlet* lisply/lisp-eval application/json ()
  "Handle Emacs Lisp evaluation endpoint for Lisply."
  ;; This endpoint aligns with emacs-lisply-endpoint-prefix + emacs-lisply-eval-endpoint
  (emacs-lisply-log "Handling lisp-eval request")
  (let* ((json-input (emacs-lisply-parse-json-body))
         (code (and json-input (cdr (assoc 'code json-input))))
         (stdout-string "")
         (result nil)
         (error nil)
         (success nil))
    
    (emacs-lisply-log "Attempting to evaluate code: %s"
		      (or code "nil"))
    
    (cond
     ((null json-input)
      (setq error "Malformed or missing input. This endpoint needs a JSON object with {code: <elisp-expression>}.")
      (setq success nil))

     ((null code)
      (setq error "Missing required 'code' parameter")
      (setq success nil))

     (t
      (emacs-lisply-log
       "About to evaluate apparently valid code..")
      
      (condition-case err
          (progn
            (setq stdout-string
                  (with-temp-buffer
                    (let ((standard-output (current-buffer)))
                      (setq result (eval (read code)))
                      (buffer-string))))
            (setq success t))
        (error
         (setq error (format "%s" err))
         (setq success nil)))))

    
    (emacs-lisply-log
       "About to send response with result.. %S" (or result "nil"))

    (let ((response `(("success" . ,success)
		      ("result" . ,(format "%s" (or result "")))
		      ("stdout" . ,stdout-string)
		      ,@(when error `(("error" . ,error))))))

      (emacs-lisply-log "About to send lisply response %S"
			response)
    
      ;; Send as formatted string instead of JSON for testing
      (emacs-lisply-send-response  response))))

(defservlet* lisply/specs application/json ()
  "Handle suggested specs endpoint for MCP client configuration."
  (emacs-lisply-log "Handling specs request")
  (let ((local-endpoint (format "http://127.0.0.1:%d/lisply" emacs-lisply-port)))
    (insert (format "
{
  \"tools\": {
    \"emacs\": {
      \"url\": \"%s\"
    }
  }
}
" local-endpoint))))

;; Documentation endpoints

(defservlet* lisply/docs/list application/json ()
  "Handle docs/list endpoint for Lisply."
  (emacs-lisply-log "Handling docs/list request")
  (let ((docs-list 
         `(("docs" . [
            (("id" . "claude-md")
             ("description" . "Skewed Emacs backend API and HTTP service documentation")
             ("path" . "/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/CLAUDE.md"))
            (("id" . "main-claude-md")
             ("description" . "Main Skewed Emacs development environment guide")
             ("path" . "/home/emacs-user/skewed-emacs/CLAUDE.md"))]))))
    (emacs-lisply-send-response docs-list)))

(defservlet* lisply/docs/claude-md text/markdown ()
  "Handle claude-md documentation endpoint."
  (emacs-lisply-log "Handling claude-md docs request")
  (let ((doc-path "/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/CLAUDE.md"))
    (if (file-exists-p doc-path)
        (insert-file-contents doc-path)
      (insert "Documentation file not found: " doc-path))))

(defservlet* lisply/docs/main-claude-md text/markdown ()
  "Handle main-claude-md documentation endpoint."
  (emacs-lisply-log "Handling main-claude-md docs request")
  (let ((doc-path "/home/emacs-user/skewed-emacs/CLAUDE.md"))
    (if (file-exists-p doc-path)
        (insert-file-contents doc-path)
      (insert "Documentation file not found: " doc-path))))

;; gdl_search helpers

(defconst emacs-lisply-gdl-search-default-k 8)
(defconst emacs-lisply-gdl-search-default-max-snippet-tokens 512)
(defconst emacs-lisply-gdl-search-max-file-bytes (* 1024 1024))
(defconst emacs-lisply-gdl-search-context-lines 3)
(defconst emacs-lisply-gdl-search-merge-gap 3)
(defconst emacs-lisply-gdl-search-ignore-dirs
  '(".git" "node_modules" "dist" "build" "vendor" "target" ".cache" "logs" "tmp" "docker" "docker-v2"))

(defconst emacs-lisply-gdl-search-default-exts
  '(".lisp" ".lsp" ".cl" ".gdl" ".gendl" ".asd" ".isc"
    ".md" ".markdown" ".org" ".txt" ".rst"
    ".el" ".js" ".ts" ".json" ".yml" ".yaml" ".html" ".css"))

(defconst emacs-lisply-gdl-search-language-exts
  '(("lisp" . (".lisp" ".lsp" ".cl" ".asd"))
    ("gdl" . (".gdl" ".gendl" ".lisp" ".lsp" ".cl"))
    ("markdown" . (".md" ".markdown" ".org" ".rst"))))

(defcustom emacs-lisply-gdl-search-index-path "/tmp/lisply-gdl-search-index.json"
  "Path to write the optional gdl_search index."
  :type 'string
  :group 'emacs-lisply)

(defcustom emacs-lisply-gdl-search-config-path
  "/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/gdl-search-config.json"
  "Path to gdl_search config JSON file."
  :type 'string
  :group 'emacs-lisply)

(defun emacs-lisply-gdl-search--projects-root ()
  (let* ((candidates (list "/projects" "/home/emacs-user/projects" (expand-file-name "~/projects")))
         (found (cl-find-if (lambda (root)
                              (file-exists-p (expand-file-name "gendl" root)))
                            candidates)))
    (or found "/projects")))

(defun emacs-lisply-gdl-search--read-config ()
  (when (and emacs-lisply-gdl-search-config-path
             (file-exists-p emacs-lisply-gdl-search-config-path))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'string))
      (json-read-file emacs-lisply-gdl-search-config-path))))

(defun emacs-lisply-gdl-search--config-value (config key default)
  (let ((entry (and config (assoc key config))))
    (if entry (cdr entry) default)))

(defun emacs-lisply-gdl-search--config-sources (config root)
  (let ((sources (emacs-lisply-gdl-search--config-value config "sources" nil)))
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

(defun emacs-lisply-gdl-search--warn-missing-sources (source-map)
  (dolist (source source-map)
    (let ((source-key (car source))
          (entries (cdr source)))
      (dolist (entry entries)
        (let ((root (cdr (assoc :root entry))))
          (unless (and root (file-exists-p root))
            (emacs-lisply-log "gdl_search source missing: %s (%s)" (or root "nil") source-key)))))))

(defun emacs-lisply-gdl-search--source-map ()
  (let* ((root (emacs-lisply-gdl-search--projects-root))
         (config (emacs-lisply-gdl-search--read-config))
         (config-sources (emacs-lisply-gdl-search--config-sources config root)))
    (if config-sources
        (progn
          (emacs-lisply-gdl-search--warn-missing-sources config-sources)
          config-sources)
      (let* ((gendl-root (expand-file-name "gendl" root))
             (gdl-root (expand-file-name "gdl" root))
             (skewed-root (expand-file-name "skewed-emacs" root))
             (tutorials-root (expand-file-name "tutorials" root))
             (training-root (expand-file-name "training" root)))
        (emacs-lisply-gdl-search--warn-missing-sources
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

(defun emacs-lisply-gdl-search--normalize-path (path)
  (replace-regexp-in-string "\\\\" "/" path))

(defun emacs-lisply-gdl-search--path-candidates-simple (path)
  (let* ((root (emacs-lisply-gdl-search--projects-root))
         (root-dir (file-name-as-directory root))
         (relative (when (and root (string-prefix-p root-dir path))
                     (file-relative-name path root)))
         (normalized (emacs-lisply-gdl-search--normalize-path path)))
    (delq nil (list normalized
                    (and relative (emacs-lisply-gdl-search--normalize-path relative))))))

(defun emacs-lisply-gdl-search--compile-path-filters (filters)
  (let (compiled)
    (dolist (filter filters (nreverse compiled))
      (when (and (stringp filter) (not (string-empty-p filter)))
        (let ((normalized (emacs-lisply-gdl-search--normalize-path filter)))
          (if (string-match-p "[*?]" normalized)
              (push (cons 'regex (wildcard-to-regexp normalized)) compiled)
            (push (cons 'prefix normalized) compiled)))))))

(defun emacs-lisply-gdl-search--path-filters-match-p (filters paths)
  (if (null filters)
      t
    (catch 'match
      (dolist (path paths)
        (let ((normalized (emacs-lisply-gdl-search--normalize-path path)))
          (dolist (filter filters)
            (let ((type (car filter))
                  (value (cdr filter)))
              (when (or (and (eq type 'prefix) (string-prefix-p value normalized))
                        (and (eq type 'regex) (string-match-p value normalized)))
                (throw 'match t))))))
      nil)))

(defun emacs-lisply-gdl-search--config-exts (config)
  (let* ((exts (emacs-lisply-gdl-search--config-value config "extensions" nil))
         (default (cdr (assoc "default" exts)))
         (lisp (cdr (assoc "lisp" exts)))
         (gdl (cdr (assoc "gdl" exts)))
         (markdown (cdr (assoc "markdown" exts))))
    (list
     (cons "default" (or default emacs-lisply-gdl-search-default-exts))
     (cons "lisp" (or lisp (cdr (assoc "lisp" emacs-lisply-gdl-search-language-exts))))
     (cons "gdl" (or gdl (cdr (assoc "gdl" emacs-lisply-gdl-search-language-exts))))
     (cons "markdown" (or markdown (cdr (assoc "markdown" emacs-lisply-gdl-search-language-exts)))))))

(defun emacs-lisply-gdl-search--allowed-exts (language)
  (let* ((config (emacs-lisply-gdl-search--read-config))
         (exts (emacs-lisply-gdl-search--config-exts config))
         (key (and language (downcase (format "%s" language))))
         (entry (assoc key exts)))
    (or (cdr entry) (cdr (assoc "default" exts)))))

(defun emacs-lisply-gdl-search--file-ext (path)
  (downcase (file-name-extension path t)))

(defun emacs-lisply-gdl-search--allowed-file-p (path allowed-exts)
  (member (emacs-lisply-gdl-search--file-ext path) allowed-exts))

(defun emacs-lisply-gdl-search--config-ignore-dirs (config)
  (let ((ignore (emacs-lisply-gdl-search--config-value config "ignore_dirs" nil)))
    (or ignore emacs-lisply-gdl-search-ignore-dirs)))

(defun emacs-lisply-gdl-search--config-exclude-filters (config)
  (let ((exclude (emacs-lisply-gdl-search--config-value config "exclude_paths" nil)))
    (emacs-lisply-gdl-search--compile-path-filters (or exclude []))))

(defun emacs-lisply-gdl-search--excluded-path-p (exclude-filters path)
  (and exclude-filters
       (not (null exclude-filters))
       (emacs-lisply-gdl-search--path-filters-match-p
        exclude-filters
        (emacs-lisply-gdl-search--path-candidates-simple path))))

(defun emacs-lisply-gdl-search--list-files (root allowed-exts ignore-dirs exclude-filters)
  (let (results)
    (when (file-exists-p root)
      (dolist (entry (directory-files root t "\\`[^.]"))
        (cond
         ((file-directory-p entry)
          (unless (member (file-name-nondirectory entry) ignore-dirs)
            (setq results (nconc results (emacs-lisply-gdl-search--list-files entry allowed-exts ignore-dirs exclude-filters)))))
         ((and (file-regular-p entry)
               (emacs-lisply-gdl-search--allowed-file-p entry allowed-exts)
               (not (emacs-lisply-gdl-search--excluded-path-p
                     exclude-filters
                     (emacs-lisply-gdl-search--normalize-path entry))))
          (push entry results)))))
    results))

(defun emacs-lisply-gdl-search--extract-terms (query)
  (let* ((lower (downcase (format "%s" query)))
         (parts (split-string lower "[^a-z0-9_]+" t))
         (filtered (cl-remove-if (lambda (term) (< (length term) 2)) parts)))
    (if filtered filtered (or parts (list lower)))))

(defun emacs-lisply-gdl-search--count-occurrences (line term)
  (let ((count 0)
        (start 0)
        (needle (regexp-quote term)))
    (while (string-match needle line start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

(defun emacs-lisply-gdl-search--trim-lines (lines max-chars)
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

(defun emacs-lisply-gdl-search--build-snippets (lines terms max-chars)
  (let (matches)
    (cl-loop for line in lines
             for idx from 0 do
             (let ((lower (downcase line))
                   (count 0)
                   (matched (make-hash-table :test 'equal)))
               (dolist (term terms)
                 (let ((occ (emacs-lisply-gdl-search--count-occurrences lower term)))
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
                  (> line (+ (plist-get current :last-line) emacs-lisply-gdl-search-merge-gap)))
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
         (let* ((start-line (max 0 (- (plist-get snippet :first-line) emacs-lisply-gdl-search-context-lines)))
                (end-line (min (1- (length lines))
                               (+ (plist-get snippet :last-line) emacs-lisply-gdl-search-context-lines)))
                (slice (cl-subseq lines start-line (1+ end-line)))
                (trimmed (emacs-lisply-gdl-search--trim-lines slice max-chars)))
           (list :start-line start-line
                 :end-line (+ start-line (1- (length trimmed)))
                 :lines trimmed
                 :match-count (plist-get snippet :match-count)
                 :terms (plist-get snippet :terms))))
       snippets))))

(defun emacs-lisply-gdl-search--guess-language (path)
  (let ((ext (emacs-lisply-gdl-search--file-ext path)))
    (cond
     ((member ext (cdr (assoc "lisp" emacs-lisply-gdl-search-language-exts))) "lisp")
     ((member ext (cdr (assoc "gdl" emacs-lisply-gdl-search-language-exts))) "gdl")
     ((member ext (cdr (assoc "markdown" emacs-lisply-gdl-search-language-exts))) "markdown")
     (t nil))))

(defun emacs-lisply-gdl-search--section-heading (lines start-line path)
  (let ((ext (emacs-lisply-gdl-search--file-ext path)))
    (when (member ext (cdr (assoc "markdown" emacs-lisply-gdl-search-language-exts)))
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

(defun emacs-lisply-gdl-search--score (snippet term-count)
  (if (<= term-count 0)
      0.0
    (let* ((terms (plist-get snippet :terms))
           (coverage (min 1.0 (/ (float (hash-table-count terms)) term-count)))
           (density (min 1.0 (/ (float (plist-get snippet :match-count)) 8.0))))
      (min 1.0 (+ (* 0.7 coverage) (* 0.3 density))))))

(defun emacs-lisply-gdl-search--vector-to-list (value)
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun emacs-lisply-gdl-search--to-vector (items)
  (if items (vconcat items) []))

(defun emacs-lisply-gdl-search--path-candidates (file entry)
  (let* ((repo-root (cdr (assoc :repo-root entry)))
         (source-root (cdr (assoc :root entry)))
         (repo-rel (when repo-root (file-relative-name file repo-root)))
         (source-rel (when source-root (file-relative-name file source-root))))
    (delq nil (list repo-rel source-rel file))))

(defun emacs-lisply-gdl-search--metadata (language section terms)
  (let (items)
    (when language (push (cons "language" language) items))
    (when section (push (cons "section" section) items))
    (when terms (push (cons "tags" (emacs-lisply-gdl-search--to-vector terms)) items))
    (nreverse items)))

(defun emacs-lisply-gdl-search--search (params)
  (let* ((query (cdr (assoc 'query params)))
         (requested-mode (cdr (assoc 'search_mode params)))
         (search-mode (if (string= requested-mode "lexical") "lexical" "lexical"))
         (k (or (cdr (assoc 'k params)) emacs-lisply-gdl-search-default-k))
         (max-tokens (or (cdr (assoc 'max_snippet_tokens params))
                         emacs-lisply-gdl-search-default-max-snippet-tokens))
         (include-metadata (if (assoc 'include_metadata params)
                               (not (eq (cdr (assoc 'include_metadata params)) :json-false))
                             t))
         (language (cdr (assoc 'language params)))
         (config (emacs-lisply-gdl-search--read-config))
         (path-filters (emacs-lisply-gdl-search--compile-path-filters
                        (emacs-lisply-gdl-search--vector-to-list
                         (cdr (assoc 'path_filters params)))))
         (sources (emacs-lisply-gdl-search--vector-to-list (cdr (assoc 'sources params))))
         (source-map (emacs-lisply-gdl-search--source-map))
         (sources-to-search (if (and sources (> (length sources) 0))
                                (cl-remove-if-not (lambda (key) (assoc key source-map)) sources)
                              (mapcar #'car source-map)))
         (allowed-exts (emacs-lisply-gdl-search--allowed-exts language))
         (ignore-dirs (emacs-lisply-gdl-search--config-ignore-dirs config))
         (exclude-filters (emacs-lisply-gdl-search--config-exclude-filters config))
         (terms (emacs-lisply-gdl-search--extract-terms query))
         (max-chars (and max-tokens (* max-tokens 4)))
         (hits '())
         (hit-index 1))
    (dolist (source-key sources-to-search)
      (let ((entries (cdr (assoc source-key source-map))))
        (dolist (entry entries)
          (let ((root (cdr (assoc :root entry))))
            (when (and root (file-exists-p root))
              (dolist (file (emacs-lisply-gdl-search--list-files root allowed-exts ignore-dirs exclude-filters))
                (when (and (emacs-lisply-gdl-search--allowed-file-p file allowed-exts)
                           (emacs-lisply-gdl-search--path-filters-match-p
                            path-filters
                            (emacs-lisply-gdl-search--path-candidates file entry)))
                  (let* ((attrs (file-attributes file))
                         (size (file-attribute-size attrs)))
                    (when (and size (< size emacs-lisply-gdl-search-max-file-bytes))
                      (with-temp-buffer
                        (insert-file-contents file)
                        (let* ((lines (split-string (buffer-string) "\n" nil))
                               (snippets (emacs-lisply-gdl-search--build-snippets lines terms max-chars)))
                          (dolist (snippet snippets)
                            (let* ((snippet-lines (plist-get snippet :lines))
                                   (snippet-text (mapconcat #'identity snippet-lines "\n"))
                                   (preview (or (cl-find-if (lambda (line) (> (length (string-trim line)) 0))
                                                            snippet-lines)
                                                ""))
                                   (language-guess (emacs-lisply-gdl-search--guess-language file))
                                   (section (emacs-lisply-gdl-search--section-heading
                                             lines (plist-get snippet :start-line) file))
                                   (tags (cl-subseq terms 0 (min 8 (length terms))))
                                   (metadata (and include-metadata
                                                  (emacs-lisply-gdl-search--metadata
                                                   language-guess section tags))))
                              (push
                               `(("id" . ,(format "hit-%03d" hit-index))
                                 ("score" . ,(emacs-lisply-gdl-search--score snippet (length terms)))
                                 ("source" . ,source-key)
                                 ("repo" . ,(cdr (assoc :repo entry)))
                                 ("path" . ,(emacs-lisply-gdl-search--normalize-path
                                             (or (car (emacs-lisply-gdl-search--path-candidates file entry))
                                                 file)))
                                 ("start_line" . ,(1+ (plist-get snippet :start-line)))
                                 ("end_line" . ,(1+ (plist-get snippet :end-line)))
                                 ("snippet" . ,snippet-text)
                                 ("preview" . ,(string-trim preview))
                                 ("metadata" . ,metadata))
                               hits)
                              (setq hit-index (1+ hit-index)))))))))))))))
    (setq hits (sort hits (lambda (a b) (> (cdr (assoc "score" a)) (cdr (assoc "score" b))))))
    `(("query" . ,query)
      ("search_mode" . ,search-mode)
      ("sources" . ,(emacs-lisply-gdl-search--to-vector sources-to-search))
      ("hits" . ,(emacs-lisply-gdl-search--to-vector (cl-subseq hits 0 (min k (length hits))))))))

(defun emacs-lisply-gdl-search-build-index ()
  "Write a lightweight index snapshot to `emacs-lisply-gdl-search-index-path`."
  (interactive)
  (let* ((config (emacs-lisply-gdl-search--read-config))
         (source-map (emacs-lisply-gdl-search--source-map))
         (allowed-exts (emacs-lisply-gdl-search--allowed-exts nil))
         (ignore-dirs (emacs-lisply-gdl-search--config-ignore-dirs config))
         (exclude-filters (emacs-lisply-gdl-search--config-exclude-filters config))
         (items '()))
    (dolist (source-key (mapcar #'car source-map))
      (let ((entries (cdr (assoc source-key source-map))))
        (dolist (entry entries)
          (let ((root (cdr (assoc :root entry))))
            (when (and root (file-exists-p root))
              (dolist (file (emacs-lisply-gdl-search--list-files root allowed-exts ignore-dirs exclude-filters))
                (let ((attrs (file-attributes file)))
                  (push `(("source" . ,source-key)
                          ("path" . ,(emacs-lisply-gdl-search--normalize-path file))
                          ("mtime" . ,(format "%s" (file-attribute-modification-time attrs)))
                          ("language" . ,(emacs-lisply-gdl-search--guess-language file)))
                        items))))))))
    (with-temp-file emacs-lisply-gdl-search-index-path
      (insert (json-encode `(("generated_at" . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))
                             ("files" . ,(vconcat (nreverse items)))))))))

(defservlet* lisply/gdl-search application/json ()
  "Handle gdl-search endpoint for Lisply."
  (emacs-lisply-log "Handling gdl-search request")
  (let* ((json-input (emacs-lisply-parse-json-body))
         (query (and json-input (cdr (assoc 'query json-input)))))
    (if (not (and query (stringp query) (not (string-empty-p query))))
        (emacs-lisply-send-response
         `(("error" . "Missing required parameter: query")))
      (condition-case err
          (let ((result (emacs-lisply-gdl-search--search json-input)))
            (emacs-lisply-send-response result))
        (error
         (emacs-lisply-send-response
          `(("error" . ,(format "%s" err)))))))))


;; Initialize endpoints
(defun initialize-lisply-endpoints ()
  "Initialize all Lisply endpoints."
  (emacs-lisply-log "Initializing Lisply endpoints"))

;; Run initialization
(initialize-lisply-endpoints)

(provide 'lisply-endpoints)
;;; lisply-endpoints.el ends here

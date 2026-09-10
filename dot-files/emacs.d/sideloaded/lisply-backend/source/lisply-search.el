;;; lisply-search.el --- lisply_search: corpus search for GDL/Gendl and the console -*- lexical-binding: t; -*-

;; Copyright © 2026 Genworks
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:
;;
;; Plist-native search system for GDL/Gendl documentation and source code.
;; Build-time index generation and runtime search in a single file.
;;
;; Index format uses keyword keys throughout for idiomatic Elisp:
;;   (:version 1
;;    :generated-at "2026-..."
;;    :config (:sources ... :extensions ...)
;;    :files [(:source :gendl :path "..." :snippets [...]) ...])
;;
;; Symbol prefix: lisply-search-- (avoiding global collisions)

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; ============================================================
;;;; Customization
;;;; ============================================================

(defgroup lisply-search nil
  "lisply_search: corpus search for GDL/Gendl and the console."
  :group 'tools)

(defcustom lisply-search-services-path
  (let* ((dir (expand-file-name "sideloaded/lisply-backend/"
                                (file-truename user-emacs-directory)))
         (new (expand-file-name "lisply-search-config.sexp" dir))
         (old (expand-file-name "skewed-search-config.sexp" dir)))
    (if (or (file-exists-p new) (not (file-exists-p old))) new old))
  "Path to the file carrying the `:lisply-search-config' plist.

Until 2026-08-15 this pointed at the stack's services.sexp, because that
was the only single-source-of-truth file around.  The search corpus
config never belonged there -- nothing in the stack generator ever read
it, and the only consumer has always been this file -- so when the stack
machinery moved out to the Basilisk repo it went to a config file that
ships beside lisply-search.el.  Renamed with the tool on 2026-09-09
(skewed_search -> lisply_search); the old file name and the old
`:skewed-search-config' key are still read.

The variable name is kept for compatibility with callers that set it
explicitly."
  :type 'string
  :group 'lisply-search)

(defcustom lisply-search-index-path
  (expand-file-name "~/.emacs.d/sideloaded/lisply-backend/lisply-search-index.sexp")
  "Path to the search index file."
  :type 'string
  :group 'lisply-search)

;;;; ============================================================
;;;; Constants
;;;; ============================================================

(defconst lisply-search--index-version 3
  "Index format version. Increment when format changes.")

(defconst lisply-search--max-file-bytes (* 1024 1024)
  "Skip files larger than this.")

(defconst lisply-search--default-snippet-lines 24)
(defconst lisply-search--default-snippet-chars 1200)
(defconst lisply-search--default-k 8)
(defconst lisply-search--default-max-tokens 512)
(defconst lisply-search--default-match-mode :all)
(defconst lisply-search--default-any-max-candidates nil)

(defconst lisply-search--default-extensions
  '(".lisp" ".lsp" ".cl" ".gdl" ".gendl" ".asd" ".isc"
    ".md" ".markdown" ".org" ".txt" ".rst"
    ".el" ".js" ".ts" ".json" ".yml" ".yaml" ".html" ".css"))

(defconst lisply-search--language-extensions
  '((:lisp ".lisp" ".lsp" ".cl" ".asd" ".el")
    (:gendl ".gendl")
    (:gdl ".gdl" ".gendl" ".lisp" ".lsp" ".cl")
    (:markdown ".md" ".markdown" ".org" ".rst")))

(defconst lisply-search--default-ignore-dirs
  '(".git" "node_modules" "dist" "build" "vendor" "target" ".cache" "logs" "tmp" "docker"))

;;;; ============================================================
;;;; Runtime State
;;;; ============================================================

(defvar lisply-search--cache nil
  "Cached index: (:path PATH :mtime MTIME :index INDEX :snippet-map HASH).")

;;;; ============================================================
;;;; Utilities
;;;; ============================================================

(defun lisply-search--log (fmt &rest args)
  "Log message with FMT and ARGS."
  (apply #'message (concat "[lisply-search] " fmt) args))

(defun lisply-search--read-sexp-file (path)
  "Read a single s-expression from PATH, or nil if not found."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun lisply-search--pget (plist key &optional default)
  "Get KEY from PLIST, returning DEFAULT if missing or nil.
Note: Cannot distinguish between nil value and missing key."
  (let ((val (plist-get plist key)))
    (if val val default)))

(defun lisply-search--normalize-path (path)
  "Normalize PATH separators to forward slashes."
  (replace-regexp-in-string "\\\\" "/" path))

(defun lisply-search--file-ext (path)
  "Return lowercase file extension including dot."
  (downcase (or (file-name-extension path t) "")))

(defun lisply-search--to-list (thing)
  "Coerce THING to list (handle vectors)."
  (cond ((vectorp thing) (append thing nil))
        ((listp thing) thing)
        (t nil)))

(defun lisply-search--to-vector (list)
  "Convert LIST to vector, or empty vector if nil."
  (if list (vconcat list) []))


;;;; ============================================================
;;;; Config Handling (services.sexp -> search config)
;;;; ============================================================

(defun lisply-search--read-config ()
  "Read the search config from `lisply-search-services-path'.
Returns plist: (:sources ... :extensions ... :ignore-dirs ...).  The
top-level key is `:lisply-search-config'; the pre-rename
`:skewed-search-config' is still accepted."
  (let* ((services (lisply-search--read-sexp-file lisply-search-services-path))
         (cfg (or (plist-get services :lisply-search-config)
                  (plist-get services :skewed-search-config))))
    (when cfg
      ;; Pass through as-is: the config file already uses plists
      cfg)))

(defun lisply-search--config-sources (config)
  "Extract sources from CONFIG as a flat list of entry plists.
Each entry carries :name :root :repo :repo-root :repo-url, plus the
optional :sparse (paths for a sparse checkout when the corpus is one
directory of a larger repo) and :branch (overriding the build's
default branch for that clone)."
  (let ((sources (lisply-search--pget config :sources)))
    (mapcan
     (lambda (source)
       (cl-destructuring-bind (&key name entries) source
         (mapcar
          (lambda (entry)
            (cl-destructuring-bind (&key root repo repo-root repo-url sparse branch
                                         &allow-other-keys)
                entry
              (let* (;; Expand relative paths from /projects
                     (abs-root (if (and root (not (file-name-absolute-p root)))
                                   (expand-file-name root "/projects")
                                 root))
                     (abs-repo-root (if (and repo-root (not (file-name-absolute-p repo-root)))
                                        (expand-file-name repo-root "/projects")
                                      (or repo-root abs-root))))
                (list :name (intern (concat ":" name))  ; keyword-ify
                      :root abs-root
                      :repo repo
                      :repo-root abs-repo-root
                      :repo-url repo-url
                      :sparse (lisply-search--to-list sparse)
                      :branch branch))))
          entries)))
     sources)))

(defun lisply-search--config-extensions (config &optional language)
  "Get allowed extensions for LANGUAGE from CONFIG, or default."
  (let* ((exts (lisply-search--pget config :extensions))
         (lang-key (and language (if (keywordp language) language
                                   (intern (concat ":" (downcase (format "%s" language)))))))
         (lang-exts (and lang-key (plist-get exts lang-key))))
    (or lang-exts
        (plist-get exts :default)
        lisply-search--default-extensions)))

(defun lisply-search--config-ignore-dirs (config)
  "Get ignore-dirs list from CONFIG."
  (or (lisply-search--pget config :ignore-dirs)
      lisply-search--default-ignore-dirs))

(defun lisply-search--config-exclude-paths (config)
  "Get exclude-paths patterns from CONFIG."
  (lisply-search--to-list (lisply-search--pget config :exclude-paths)))


;;;; ============================================================
;;;; File Scanning
;;;; ============================================================

(defun lisply-search--path-excluded-p (path excludes)
  "Check if PATH matches any pattern in EXCLUDES."
  (cl-some (lambda (pattern)
             (let ((normalized (lisply-search--normalize-path pattern)))
               (if (string-match-p "[*?]" normalized)
                   (string-match-p (wildcard-to-regexp normalized)
                                   (lisply-search--normalize-path path))
                 (string-prefix-p normalized (lisply-search--normalize-path path)))))
           excludes))

(defun lisply-search--list-files (root extensions ignore-dirs excludes)
  "Recursively list files under ROOT matching EXTENSIONS.
Skip IGNORE-DIRS and paths matching EXCLUDES patterns."
  (let (results)
    (when (file-exists-p root)
      (dolist (entry (directory-files root t "\\`[^.]"))
        (cond
         ((file-directory-p entry)
          (unless (member (file-name-nondirectory entry) ignore-dirs)
            (setq results (nconc results
                                 (lisply-search--list-files
                                  entry extensions ignore-dirs excludes)))))
         ((and (file-regular-p entry)
               (member (lisply-search--file-ext entry) extensions)
               (not (lisply-search--path-excluded-p entry excludes)))
          (push entry results)))))
    results))

(defun lisply-search--guess-language (path)
  "Guess language keyword from PATH extension."
  (let ((ext (lisply-search--file-ext path)))
    (cond
     ((member ext (cdr (assq :lisp lisply-search--language-extensions))) :lisp)
     ((member ext (cdr (assq :gendl lisply-search--language-extensions))) :gendl)
     ((member ext (cdr (assq :gdl lisply-search--language-extensions))) :gdl)
     ((member ext (cdr (assq :markdown lisply-search--language-extensions))) :markdown)
     (t nil))))

;;;; ============================================================
;;;; Snippet Extraction
;;;; ============================================================

(defun lisply-search--extract-snippets (lines max-lines max-chars)
  "Split LINES into snippet chunks of MAX-LINES, trimmed to MAX-CHARS."
  (let ((total (length lines))
        (start 0)
        snippets)
    (while (< start total)
      (let* ((end (min (1- total) (+ start (1- max-lines))))
             (slice (cl-subseq lines start (1+ end)))
             (trimmed (let ((len 0) kept)
                        (catch 'done
                          (dolist (line slice)
                            (let ((next (+ len (length line) 1)))
                              (when (and (> next max-chars) kept)
                                (throw 'done nil))
                              (push line kept)
                              (setq len next))))
                        (nreverse kept))))
        (when trimmed
          (push (list :start start
                      :end (+ start (1- (length trimmed)))
                      :lines trimmed)
                snippets))
        (setq start (1+ end))))
    (nreverse snippets)))

(defun lisply-search--find-section-heading (lines start-line path)
  "Find nearest markdown/org heading above START-LINE in LINES."
  (let ((ext (lisply-search--file-ext path)))
    (when (member ext '(".md" ".markdown" ".org" ".rst"))
      (cl-loop for i from start-line downto (max 0 (- start-line 50))
               for line = (nth i lines)
               when (and (string= ext ".org")
                         (string-match "^\\*+\\s-+\\(.+\\)$" line))
               return (string-trim (match-string 1 line))
               when (and (not (string= ext ".org"))
                         (string-match "^#+\\s-+\\(.+\\)$" line))
               return (string-trim (match-string 1 line))))))


(defun lisply-search--extract-file-snippets (path max-lines max-chars)
  "Extract snippets from file at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (let* ((all-lines (split-string (buffer-string) "\n" nil))
           (raw-snippets (lisply-search--extract-snippets all-lines max-lines max-chars))
           (language (lisply-search--guess-language path)))
      (mapcar
       (lambda (snip)
         (cl-destructuring-bind (&key lines start end) snip
           (let* ((text (string-join lines "\n"))
                  (preview (or (cl-find-if (lambda (l) (> (length (string-trim l)) 0)) lines) "")))
             (list :start-line start
                   :end-line end
                   :snippet text
                   :preview (string-trim preview)
                   :section (lisply-search--find-section-heading all-lines start path)
                   :language language
                   :terms (lisply-search--to-vector
                           (delete-dups (lisply-search--extract-terms text)))))))
       raw-snippets))))


;;;; ============================================================
;;;; Index Building
;;;; ============================================================

(defun lisply-search--build-file-entry (source-info path config)
  "Build index entry for PATH from SOURCE-INFO using CONFIG."
  (cl-destructuring-bind (&key name repo repo-root &allow-other-keys) source-info
    (let* ((attrs (file-attributes path))
           (size (file-attribute-size attrs))
           (language (lisply-search--guess-language path))
           (preextract (lisply-search--pget config :preextract-snippets))
           (max-lines (or (lisply-search--pget config :preextract-max-lines)
                          lisply-search--default-snippet-lines))
           (max-chars (or (lisply-search--pget config :preextract-max-chars)
                          lisply-search--default-snippet-chars))
           (snippets (when (and preextract size (< size lisply-search--max-file-bytes))
                       (lisply-search--extract-file-snippets path max-lines max-chars))))
      (list :source name
            :path (lisply-search--normalize-path path)
            :repo repo
            :repo-root repo-root
            :language language
            :mtime (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                       (file-attribute-modification-time attrs) t)
            :snippets (when snippets (lisply-search--to-vector snippets))))))

(defun lisply-search--compute-checksum (file-entries)
  "Compute simple checksum from FILE-ENTRIES."
  (let ((count (length file-entries))
        (total-size 0))
    (dolist (entry file-entries)
      (let ((path (plist-get entry :path)))
        (when (and path (file-exists-p path))
          (cl-incf total-size (or (file-attribute-size (file-attributes path)) 0)))))
    (format "%d-%d" count total-size)))

(defun lisply-search-build-index ()
  "Build search index from services.sexp config."
  (interactive)
  (let* ((config (lisply-search--read-config))
         (sources (lisply-search--config-sources config))
         (extensions (lisply-search--config-extensions config))
         (ignore-dirs (lisply-search--config-ignore-dirs config))
         (excludes (lisply-search--config-exclude-paths config))
         entries)
    (unless sources
      (error "lisply-search: no sources configured under :lisply-search-config in %s"
             lisply-search-services-path))
    ;; Scan each source
    (dolist (source sources)
      (let ((root (plist-get source :root)))
        (if (not (file-exists-p root))
            (lisply-search--log "WARNING: Source root missing: %s" root)
          (dolist (path (lisply-search--list-files root extensions ignore-dirs excludes))
            (push (lisply-search--build-file-entry source path config) entries)))))
    (when (null entries)
      (error "lisply-search: nothing to index -- every configured source root is missing"))
    ;; Build index plist
    (let* ((files (nreverse entries))
           (index (list :version lisply-search--index-version
                        :generated-at (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
                        :checksum (lisply-search--compute-checksum files)
                        :config config
                        :files (lisply-search--to-vector files))))
      ;; Write index
      (with-temp-file lisply-search-index-path
        (let ((print-length nil)
              (print-level nil))
          (prin1 index (current-buffer))))
      (lisply-search--log "Index written: %s (%d files)"
                          lisply-search-index-path (length files))
      index)))


;;;; ============================================================
;;;; ============================================================
;;;; Clone Support (for Docker builds with empty /projects/)
;;;; ============================================================

(defun lisply-search--git (dir &rest args)
  "Run git with ARGS (in DIR when non-nil), logging to *lisply-search-clone*.
Returns the exit status."
  (apply #'process-file "git" nil "*lisply-search-clone*" t
         (append (when dir (list "-C" dir)) args)))

(defun lisply-search--clone-entry (source-info branch)
  "Make the :root of SOURCE-INFO exist, cloning its :repo-url if needed.
The clone lands in :repo-root -- the repository's own directory, which
may be an ancestor of :root -- so a corpus that is one directory of a
larger repository can be fetched as a sparse checkout of just that
directory (:sparse, a list of paths).  An entry's own :branch overrides
BRANCH, which defaults to master.  Returns non-nil when :root exists
afterwards."
  (let* ((root (plist-get source-info :root))
         (repo-root (or (plist-get source-info :repo-root) root))
         (repo-url (plist-get source-info :repo-url))
         (sparse (plist-get source-info :sparse))
         (branch (or (plist-get source-info :branch) branch "master")))
    (cond
     ((or (null root) (string-empty-p root))
      (lisply-search--log "Skip clone: no root path")
      nil)
     ((file-exists-p root)
      (lisply-search--log "Exists, skip clone: %s" root)
      t)
     ((or (null repo-url) (string-empty-p repo-url))
      (lisply-search--log "SOURCE MISSING (local-only, no repo-url): %s" root)
      nil)
     ((and sparse (file-exists-p repo-root))
      ;; The repository is already here but not our directory: widen it.
      (lisply-search--log "Widening sparse checkout %s by %s" repo-root sparse)
      (apply #'lisply-search--git repo-root "sparse-checkout" "add" sparse)
      (if (file-exists-p root)
          (progn (lisply-search--log "Sparse checkout widened: %s" root) t)
        (lisply-search--log "SOURCE MISSING after widening: %s" root)
        nil))
     (t
      (lisply-search--log "Cloning %s -> %s (branch: %s%s)" repo-url repo-root branch
                          (if sparse (format ", sparse: %s" sparse) ""))
      (make-directory (file-name-directory (directory-file-name repo-root)) t)
      (let ((exit (apply #'lisply-search--git nil
                         (append (list "clone" "--depth" "1" "--branch" branch)
                                 (when sparse (list "--filter=blob:none" "--sparse"))
                                 (list repo-url repo-root)))))
        (when (and sparse (integerp exit) (zerop exit))
          (setq exit (apply #'lisply-search--git repo-root "sparse-checkout" "set" sparse)))
        (cond
         ((not (and (integerp exit) (zerop exit)))
          (lisply-search--log "Clone FAILED: %s (exit %s)" repo-root exit)
          nil)
         ((file-exists-p root)
          (lisply-search--log "Clone succeeded: %s" root)
          t)
         (t
          (lisply-search--log "Clone landed but SOURCE MISSING inside it: %s" root)
          nil)))))))

(defun lisply-search--clone-all-sources (config branch)
  "Make every source in CONFIG present, cloning where needed.
Returns the list of source roots still missing afterwards; nil means
every source is present."
  (let ((sources (lisply-search--config-sources config))
        missing)
    (if (null sources)
        (progn
          (lisply-search--log "No sources configured, nothing to clone")
          nil)
      (dolist (source sources)
        (unless (lisply-search--clone-entry source branch)
          (push (plist-get source :root) missing)))
      (setq missing (nreverse missing))
      (lisply-search--log "Source summary: %d configured, %d present, %d missing%s"
                          (length sources)
                          (- (length sources) (length missing))
                          (length missing)
                          (if missing (format " -- %s" (string-join missing ", ")) ""))
      missing)))

(defun lisply-search-build-index-with-clone (&optional branch strict)
  "Clone corpora as needed, then build the index from every source present.
BRANCH is the default git branch for clones (master); an entry's own
:branch wins.  In Docker builds /projects/ is empty and the repos are
cloned fresh; in dev the existing repos are used as-is.

A source that cannot be fetched is logged as SOURCE MISSING and
skipped, and the index is still built from the rest: a partial corpus
beats none.  (From 2026-08-20 to 2026-09-09 every shipped console image
carried no index at all, because one corpus repository had gone away
and a failed clone used to skip the whole build -- silently, since the
build step still exited 0.)  When STRICT is non-nil, or the environment
variable LISPLY_INDEX_STRICT is \"true\", a missing source is an error
instead, so a CI build cannot go green with a short corpus."
  (interactive)
  (let ((config (lisply-search--read-config))
        (strict (or strict (equal (getenv "LISPLY_INDEX_STRICT") "true"))))
    (if (not config)
        (error "lisply-search: no config found at %s" lisply-search-services-path)
      (let ((missing (lisply-search--clone-all-sources config branch)))
        (when missing
          (lisply-search--log "SOURCE MISSING: %s" (string-join missing ", ")))
        (if (and missing strict)
            (error "lisply-search: %d source(s) missing in strict mode: %s"
                   (length missing) (string-join missing ", "))
          (lisply-search-build-index))))))


;;;; Runtime: Index Loading & Caching
;;;; ============================================================

(defun lisply-search--validate-index (index)
  "Validate INDEX format. Return error string or nil if valid."
  (cond
   ((null index) "Index is nil")
   ((not (plist-get index :version)) "Index missing :version")
   ((not (memq (plist-get index :version) '(1 2 3)))
    (format "Index version unsupported: %s (expected 1 or 2)"
            (plist-get index :version)))
   ((not (plist-get index :files)) "Index missing :files")
   (t nil)))

(defun lisply-search--build-snippet-map (index)
  "Build inverted term index from INDEX.
Returns hash-table: term -> list of (:snippet S :file F :source SRC :entry E)."
  (let ((snippet-map (make-hash-table :test 'equal))
        (files (lisply-search--to-list (plist-get index :files))))
    (dolist (file-entry files)
      (cl-destructuring-bind (&key path source snippets &allow-other-keys) file-entry
        (dolist (snippet (lisply-search--to-list snippets))
          (let ((snippet-plist snippet))
            (cl-destructuring-bind (&key ((:snippet snippet-text)) terms &allow-other-keys) snippet-plist
              (let* ((text (or snippet-text ""))
                     ;; Use pre-computed terms (v3+) or fall back to extraction (v2)
                     (term-list (or (lisply-search--to-list terms)
                                    (lisply-search--extract-terms text)))
                     (candidate (list :snippet snippet-plist
                                      :file path
                                      :source source
                                      :entry file-entry)))
                (dolist (term term-list)
                  (push candidate (gethash term snippet-map)))))))))
    snippet-map))

(defun lisply-search--load-index ()
  "Load and cache index. Returns cache plist or nil."
  (let* ((path lisply-search-index-path)
         (attrs (and (file-exists-p path) (file-attributes path)))
         (mtime (and attrs (file-attribute-modification-time attrs))))
    ;; Check cache validity
    (if (and lisply-search--cache
             (equal (plist-get lisply-search--cache :path) path)
             (equal (plist-get lisply-search--cache :mtime) mtime))
        lisply-search--cache
      ;; Reload
      (let* ((raw (lisply-search--read-sexp-file path))
             (index raw))
        (when index
          (let ((err (lisply-search--validate-index index)))
            (when err
              (lisply-search--log "WARNING: %s" err)
              (setq index nil))))
        (setq lisply-search--cache
              (when index
                (list :path path
                      :mtime mtime
                      :index index
                      :config (plist-get index :config)
                      :snippet-map (lisply-search--build-snippet-map index))))
        lisply-search--cache))))


;;;; ============================================================
;;;; Search Logic
;;;; ============================================================

(defun lisply-search--extract-terms (text)
  "Extract search terms from TEXT. Returns list of lowercase terms."
  (let* ((lower (downcase (format "%s" text)))
         (parts (split-string lower "[^a-z0-9_]+" t)))
    (cl-remove-if (lambda (term) (< (length term) 2)) parts)))

(defun lisply-search--count-term-occurrences (text term)
  "Count occurrences of TERM in TEXT."
  (let ((count 0) (start 0) (needle (regexp-quote term)))
    (while (string-match needle text start)
      (cl-incf count)
      (setq start (match-end 0)))
    count))

(defun lisply-search--score-candidate (candidate terms)
  "Score CANDIDATE based on matching TERMS. Returns 0.0-1.0."
  (cl-destructuring-bind (&key snippet &allow-other-keys) candidate
    (cl-destructuring-bind (&key ((:snippet snippet-text)) &allow-other-keys) snippet
      (let* ((text (downcase (or snippet-text "")))
             (term-count (length terms))
             (matched-terms 0)
             (total-matches 0))
        (dolist (term terms)
          (let ((occ (lisply-search--count-term-occurrences text term)))
            (when (> occ 0)
              (cl-incf matched-terms)
              (cl-incf total-matches occ))))
        (if (zerop term-count)
            0.0
          (let ((coverage (/ (float matched-terms) term-count))
                (density (min 1.0 (/ (float total-matches) 8.0))))
            (min 1.0 (+ (* 0.7 coverage) (* 0.3 density)))))))))

(defun lisply-search--candidate-matches-p (candidate sources extensions excludes)
  "Return non-nil if CANDIDATE matches SOURCES, EXTENSIONS, and EXCLUDES."
  (cl-destructuring-bind (&key file source &allow-other-keys) candidate
    (and file
         (or (null sources) (memq source sources))
         (member (lisply-search--file-ext file) extensions)
         (not (lisply-search--path-excluded-p file excludes)))))

(defun lisply-search--filter-candidates (snippet-map terms sources extensions excludes match-mode any-max-candidates)
  "Get matching candidates from SNIPPET-MAP for TERMS.
Filter by SOURCES, EXTENSIONS, and EXCLUDES. MATCH-MODE is :all or :any."
  (let* ((terms (delete-dups terms)))
    (cond
     ((or (null terms) (null snippet-map)) nil)
     ((eq match-mode :any)
      ;; OR semantics: union of term hits, optionally capped.
      (let* ((seen (make-hash-table :test 'eq))
             (count 0)
             (term-lists
              (delq nil
                    (mapcar (lambda (term)
                              (let ((lst (gethash term snippet-map)))
                                (when lst
                                  (list term lst (length lst)))))
                            terms)))
             (sorted (sort term-lists (lambda (a b) (< (nth 2 a) (nth 2 b)))))
             candidates)
        (catch 'done
          (dolist (pair sorted)
            (dolist (candidate (nth 1 pair))
              (unless (gethash candidate seen)
                (when (lisply-search--candidate-matches-p candidate sources extensions excludes)
                  (puthash candidate t seen)
                  (push candidate candidates)
                  (when any-max-candidates
                    (setq count (1+ count))
                    (when (>= count any-max-candidates)
                      (throw 'done nil))))))))
        (nreverse candidates)))
     (t
      ;; AND semantics: intersect rarest term postings first.
      (let* ((term-lists
              (delq nil
                    (mapcar (lambda (term)
                              (let ((lst (gethash term snippet-map)))
                                (when lst
                                  (list term lst (length lst)))))
                            terms))))
        (when term-lists
          (let* ((sorted (sort term-lists (lambda (a b) (< (nth 2 a) (nth 2 b)))))
                 (seed (nth 1 (car sorted)))
                 (candidate-set (make-hash-table :test 'eq))
                 results)
            (dolist (candidate seed)
              (when (lisply-search--candidate-matches-p candidate sources extensions excludes)
                (puthash candidate t candidate-set)))
            (dolist (pair (cdr sorted))
              (let ((present (make-hash-table :test 'eq)))
                (dolist (candidate (nth 1 pair))
                  (when (gethash candidate candidate-set)
                    (puthash candidate t present)))
                (setq candidate-set present)))
            (maphash (lambda (candidate _)
                       (push candidate results))
                     candidate-set)
            (nreverse results))))))))

(defun lisply-search--format-hit (candidate terms index include-metadata)
  "Format CANDIDATE as search hit."
  ;; FLAG:PERF - Called for ALL candidates (~625ms for 3750 hits).
  ;; Optimize: (1) score-only pass first, format only top-k, (2) cache downcase at build.
  (cl-destructuring-bind (&key snippet file source entry &allow-other-keys) candidate
    (cl-destructuring-bind (&key repo repo-root &allow-other-keys) entry
      (cl-destructuring-bind (&key start-line end-line preview language section &allow-other-keys) snippet
        (let* ((snippet-text (plist-get snippet :snippet))
               ;; Prefer relative path from repo-root
               (display-path (if repo-root
                                 (file-relative-name file repo-root)
                               file)))
          (list :id (format "hit-%03d" index)
                :score (lisply-search--score-candidate candidate terms)
                :source source
                :repo repo
                :path (lisply-search--normalize-path display-path)
                :start-line (1+ (or start-line 0))
                :end-line (1+ (or end-line 0))
                :snippet snippet-text
                :preview (or preview "")
                :metadata (when include-metadata
                            (list :language language
                                  :section section
                                  :tags (lisply-search--to-vector
                                         (cl-subseq terms 0 (min 8 (length terms))))))))))))


;;;; ============================================================
;;;; Main Search API
;;;; ============================================================

(defun lisply-search (params)
  "Execute search with PARAMS plist.
PARAMS: (:query Q :k K :sources [S...] :language L :include-metadata BOOL).
Returns plist: (:query Q :search-mode M :sources [S...] :hits [H...])."
  (cl-destructuring-bind (&key query k max-snippet-tokens match-mode
                               any-max-candidates language sources
                               include-metadata &allow-other-keys)
      params
    (let* ((k (or k lisply-search--default-k))
           (max-tokens (or max-snippet-tokens lisply-search--default-max-tokens))
           (match-mode (or match-mode lisply-search--default-match-mode))
           (any-max-candidates (or any-max-candidates lisply-search--default-any-max-candidates))
           (include-metadata (if (plist-member params :include-metadata)
                                 include-metadata
                               t))
           (requested-sources (lisply-search--to-list sources))
           ;; Load index
           (cache (lisply-search--load-index))
           (config (and cache (plist-get cache :config)))
           (snippet-map (and cache (plist-get cache :snippet-map))))
    (cond
     ((not cache)
      (list :error (format "Index not found: %s" lisply-search-index-path)))
     ((not snippet-map)
      (list :error "Index has no snippet map"))
     (t
      ;; Determine which sources to search
        (let* ((all-sources (mapcar (lambda (s) (plist-get s :name))
                                    (lisply-search--config-sources config)))
               (requested (when requested-sources
                            (mapcar (lambda (s)
                                      (if (keywordp s) s
                                        (intern (concat ":" s))))
                                    requested-sources)))
               (matched (when requested
                          (cl-remove-if-not (lambda (s) (memq s all-sources))
                                            requested)))
               (unknown (when requested
                          (cl-remove-if (lambda (s) (memq s all-sources))
                                        requested)))
               (sources (if matched matched all-sources))
               (extensions (lisply-search--config-extensions config language))
               (excludes (lisply-search--config-exclude-paths config))
               (terms (lisply-search--extract-terms query))
               (max-chars (* max-tokens 4)))
        ;; Filter and score candidates
        (let* ((candidates (lisply-search--filter-candidates
                            snippet-map terms sources extensions excludes match-mode any-max-candidates))
               (hits (cl-loop for c in candidates
                              for i from 1
                              collect (lisply-search--format-hit c terms i include-metadata)))
               ;; Sort by score descending
               (sorted (sort hits (lambda (a b)
                                    (> (plist-get a :score) (plist-get b :score)))))
               ;; Take top k and truncate snippets
               (top-k (cl-subseq sorted 0 (min k (length sorted))))
               (final (mapcar
                       (lambda (hit)
                         (let ((snippet (plist-get hit :snippet)))
                           (when (and snippet max-chars (> (length snippet) max-chars))
                             (setq hit (plist-put hit :snippet
                                                  (substring snippet 0 max-chars)))))
                         hit)
                       top-k)))
          (list :query query
                :search-mode :lexical
                :match-mode match-mode
                :sources (lisply-search--to-vector sources)
                :hits (lisply-search--to-vector final)
                :warning (when unknown
                           (format "Unknown sources ignored: %s"
                                   (mapconcat (lambda (s) (substring (symbol-name s) 1))
                                              unknown ", ")))
                :known-sources (when unknown
                                 (lisply-search--to-vector all-sources))))))))))

;;;; ============================================================
;;;; HTTP Endpoint (when simple-httpd loaded)
;;;; ============================================================

(require 'simple-httpd nil t)
(require 'lisply-http-setup nil t)

(defun lisply-search--plist-to-alist (plist)
  "Convert PLIST to alist with string keys for JSON."
  (let (result)
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             ;; Convert :keyword-name to "keyword_name" for JSON compatibility
             (str-key (let ((raw (if (keywordp key)
                                     (substring (symbol-name key) 1)
                                   (format "%s" key))))
                        (replace-regexp-in-string "-" "_" raw))))
        (push (cons str-key
                    (cond
                     ((and (listp val) (keywordp (car val)))
                      (lisply-search--plist-to-alist val))
                     ((vectorp val)
                      (vconcat (mapcar (lambda (v)
                                         (if (and (listp v) (keywordp (car v)))
                                             (lisply-search--plist-to-alist v)
                                           v))
                                       val)))
                     (t val)))
              result)))
    (nreverse result)))

(defun lisply-search--serve-http-query (json-input)
  "Answer one lisply_search HTTP request whose parsed JSON body is JSON-INPUT.
Shared by the lisply-search endpoint and its pre-rename alias."
  (let* ((query (and json-input (cdr (assoc 'query json-input)))))
    (if (not (and query (stringp query) (not (string-empty-p query))))
        (emacs-lisply-send-response '(("error" . "Missing required parameter: query")))
      (condition-case err
          (let* ((raw-match (or (cdr (assoc 'match_mode json-input))
                                (cdr (assoc 'match-mode json-input))))
                 (raw-any-max (or (cdr (assoc 'any_max_candidates json-input))
                                  (cdr (assoc 'any-max-candidates json-input))))
                 (match-mode (cond
                              ((or (eq raw-match :all) (equal raw-match "all")) :all)
                              ((or (eq raw-match :any) (equal raw-match "any")) :any)
                              (t nil)))
                 (any-max-candidates (cond
                                      ((numberp raw-any-max) raw-any-max)
                                      ((and (stringp raw-any-max)
                                            (string-match-p "\\`[0-9]+\\'" raw-any-max))
                                       (string-to-number raw-any-max))
                                      (t nil)))
                 (params (list :query query
                               :k (cdr (assoc 'k json-input))
                               :sources (cdr (assoc 'sources json-input))
                               :language (cdr (assoc 'language json-input))
                               :match-mode match-mode
                               :any-max-candidates any-max-candidates
                               :max-snippet-tokens (cdr (assoc 'max_snippet_tokens json-input))
                               :include-metadata (not (eq (cdr (assoc 'include_metadata json-input))
                                                          :json-false))))
                 (result (lisply-search params))
                 (json-result (lisply-search--plist-to-alist result)))
            (emacs-lisply-send-response json-result))
        (error
         (emacs-lisply-send-response
          `(("error" . ,(format "%s" err)))))))))

(when (featurep 'simple-httpd)
  (defservlet* lisply/lisply-search application/json ()
    "Handle the lisply_search endpoint."
    (lisply-search--serve-http-query
     (and (fboundp 'emacs-lisply-parse-json-body)
          (emacs-lisply-parse-json-body))))
  ;; The pre-rename endpoint (skewed_search, until 2026-09-09), kept
  ;; one release so MCP wrappers built before the rename keep working.
  (defservlet* lisply/skewed-search application/json ()
    "Deprecated alias of lisply/lisply-search."
    (lisply-search--serve-http-query
     (and (fboundp 'emacs-lisply-parse-json-body)
          (emacs-lisply-parse-json-body)))))

(provide 'lisply-search)
;;; lisply-search.el ends here

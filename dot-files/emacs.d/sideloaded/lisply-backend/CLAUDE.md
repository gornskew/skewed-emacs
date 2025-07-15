# Emacs Lisply Backend - Claude Usage Guide

## Overview

This Emacs Lisply Backend provides an HTTP API that allows Claude to interact with a running Emacs instance by evaluating Emacs Lisp code. This enables powerful text processing, buffer manipulation, file operations, and access to the full Emacs ecosystem.

## Related Documentation
- **Main Gendl Guide**: See `gendl` repository: `CLAUDE.md` for complete Gendl development workflow
- **Gendl Lisply Backend**: See `gendl` repository: `gwl/lisply-backend/CLAUDE.md` for Gendl REPL HTTP API
- This Emacs service (port 7080) complements the Gendl service (port 9081) for comprehensive development

## Endpoint

- **URL**: `http://localhost:7081/lisply/lisp-eval` (when using MCP via lisply-mcp)
- **Method**: POST
- **Content-Type**: application/json
- **Body**: `{"code": "<emacs-lisp-expression>"}`

## Response Format

```json
{
  "success": true,
  "result": "evaluation-result-as-string",
  "stdout": "any-printed-output"
}
```

On error:
```json
{
  "success": false,
  "result": "",
  "stdout": "",
  "error": "error-message"
}
```

## Key Capabilities

### 1. Basic Emacs Lisp Evaluation

```bash
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(+ 1 2 3)"}'
# Returns: {"success":true,"result":"6","stdout":""}

curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(list 1 2 3)"}'
# Returns: {"success":true,"result":"(1 2 3)","stdout":""}
```

### 2. Buffer Operations

**List all open buffers:**
```bash
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(mapcar (lambda (buf) (buffer-name buf)) (buffer-list))"}'
```

**Get buffer contents:**
```bash
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(with-current-buffer \"*Messages*\" (buffer-string))"}'
```

**Get current buffer name:**
```bash
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(buffer-name)"}'
```

**Switch to a buffer:**
```bash
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(switch-to-buffer \"buffer-name\")"}'
```

### 3. File Operations

**Read file contents:**
```bash
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(with-temp-buffer (insert-file-contents \"/path/to/file\") (buffer-string))"}'
```

**Write to file:**
```bash
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(with-temp-file \"/path/to/file\" (insert \"content to write\"))"}'
```

**Check if file exists:**
```bash
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(file-exists-p \"/path/to/file\")"}'
```

### 4. Directory Operations

**List directory contents:**
```bash
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(directory-files \"/path/to/directory\")"}'
```

**Get current directory:**
```bash
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(pwd)"}'
```

### 5. Text Processing

**Search and replace in buffer:**
```bash
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(with-current-buffer \"buffer-name\" (goto-char (point-min)) (while (search-forward \"old\" nil t) (replace-match \"new\")))"}'
```

**Count lines in buffer:**
```bash
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(with-current-buffer \"buffer-name\" (count-lines (point-min) (point-max)))"}'
```

### 6. System Information

**Get Emacs version:**
```bash
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "emacs-version"}'
```

**Get loaded packages:**
```bash
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(mapcar (lambda (pkg) (symbol-name (car pkg))) package-alist)"}'
```

### 7. Interactive Commands

**Execute Emacs commands:**
```bash
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(call-interactively (quote save-buffer))"}'
```

### 8. Advanced Examples

**Find all .el files in directory recursively:**
```bash
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(directory-files-recursively \"/path/to/search\" \"\\\\.el$\")"}'
```

**Get all functions defined in current buffer:**
```bash
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(save-excursion (goto-char (point-min)) (let (functions) (while (re-search-forward \"^(defun \\\\([^[:space:]]+\\\\)\" nil t) (push (match-string 1) functions)) (nreverse functions)))"}'
```

## Important Notes

### Security Considerations
- This backend allows arbitrary Emacs Lisp evaluation
- Only use in trusted, containerized environments
- Do not expose this endpoint to untrusted networks

### Data Types
- All results are returned as strings (using `format "%s"`)
- Complex Lisp data structures maintain their textual representation
- Boolean values: `t` for true, `nil` for false

### Error Handling
- Syntax errors and runtime errors are caught and returned in the `error` field
- The `success` field indicates whether evaluation completed without error

### Best Practices
- Use `with-current-buffer` to work with specific buffers
- Use `with-temp-buffer` for temporary operations
- Wrap file operations in appropriate error handling
- Use `save-excursion` to preserve cursor position when navigating

## File Editing Best Practices for AI Agents

### Understanding Buffer Operations vs String Manipulation

**The Right Way: Buffer-Based Editing (Recommended)**
```elisp
;; This is the efficient, Emacs-native approach
(progn
  (find-file "/path/to/file.js")
  (with-current-buffer "file.js"
    (goto-char (point-min))
    (search-forward "old-code")
    (replace-match "new-code")
    (save-buffer)))
```

**The Inefficient Way: String Manipulation (Not Recommended)**
```elisp
;; This approach wastes memory and CPU by duplicating Emacs's work
(let ((content (with-temp-buffer
                 (insert-file-contents "/path/to/file.js")
                 (buffer-string))))
  (setq modified-content 
        (replace-regexp-in-string "old-code" "new-code" content))
  (with-temp-file "/path/to/file.js"
    (insert modified-content)))
```

### Why Buffer Operations Are Superior

1. **Memory Efficiency**: Emacs manages buffers in optimized C code, rather than creating large Lisp strings
2. **Incremental Changes**: Make small, targeted edits instead of reprocessing entire files
3. **Native Features**: Access syntax highlighting, indentation, auto-formatting, and language-specific features
4. **Performance**: Buffer operations are optimized; string manipulation in Elisp is comparatively slow
5. **Undo/Redo**: Built-in change tracking and history
6. **Error Recovery**: Better error handling and state management

### A Cautionary Tale

*During development of this guide, there was a brief period where buffer editing seemed unreliable, leading to the mistaken conclusion that string manipulation was "more reliable for AI agents." This was actually due to improper buffer management - not using `with-current-buffer` correctly, editing the wrong buffers, or failing to call `save-buffer` properly. The lesson: when Emacs features seem broken, the problem is usually operator error, not fundamental limitations. Emacs has been optimized for text editing for decades - don't try to reinvent its buffer system with string operations!*

### Proper Buffer Editing Workflow

The key is following Emacs conventions properly:

```elisp
(defun edit-file-properly (filepath search-term replacement)
  "Demonstrate correct buffer-based file editing"
  (progn
    ;; Open file in buffer (creates buffer if needed)
    (find-file filepath)
    ;; Ensure we're working in the file buffer
    (with-current-buffer (file-name-nondirectory filepath)
      ;; Make changes
      (goto-char (point-min))
      (while (search-forward search-term nil t)
        (replace-match replacement))
      ;; Verify buffer is marked as modified
      (when (buffer-modified-p)
        ;; Save changes to disk
        (save-buffer)
        ;; Confirm save completed
        (not (buffer-modified-p))))))
```

### Buffer State Management

Always verify your buffer state:

```elisp
;; Check current buffer status
(list :current-buffer (buffer-name)
      :file-name (buffer-file-name)
      :modified (buffer-modified-p)
      :size (buffer-size))

;; List all buffers (helpful for debugging)
(mapcar (lambda (buf) 
          (with-current-buffer buf
            (list :name (buffer-name)
                  :file (buffer-file-name)
                  :modified (buffer-modified-p))))
        (buffer-list))
```

### Common Buffer Editing Patterns

**Find and Replace All Occurrences:**
```elisp
(with-current-buffer "filename.ext"
  (goto-char (point-min))
  (while (search-forward "old-text" nil t)
    (replace-match "new-text"))
  (save-buffer))
```

**Insert at Specific Location:**
```elisp
(with-current-buffer "filename.ext"
  (goto-char (point-min))
  (search-forward "function execPromise")
  (forward-line 1)
  (insert "\n// New function here\n")
  (save-buffer))
```

**Multi-Step Editing:**
```elisp
(with-current-buffer "filename.ext"
  ;; Multiple operations in sequence
  (goto-char (point-min))
  (search-forward "old-function-name")
  (replace-match "new-function-name")
  (goto-char (point-min))
  (search-forward "old-variable")
  (replace-match "new-variable")
  ;; Save all changes at once
  (save-buffer))
```

### Debugging Buffer Operations

When things go wrong:

```elisp
;; Verify you're in the right buffer
(message "Current buffer: %s, File: %s" 
         (buffer-name) (buffer-file-name))

;; Check if buffer needs saving
(if (buffer-modified-p)
    (message "Buffer has unsaved changes")
  (message "Buffer is clean"))

;; Force save if needed
(when (buffer-modified-p)
  (save-buffer)
  (message "Buffer saved"))
```

### Safe Editing with Backups

For critical changes, create backups first:

```elisp
(defun safe-edit-file (filepath edit-function)
  "Edit file with automatic backup"
  (let ((backup-path (concat filepath ".backup")))
    ;; Create backup
    (copy-file filepath backup-path t)
    ;; Make changes
    (find-file filepath)
    (with-current-buffer (file-name-nondirectory filepath)
      (funcall edit-function)
      (if (buffer-modified-p)
          (progn
            (save-buffer)
            (delete-file backup-path)  ; Remove backup on success
            t)
        (progn
          (delete-file backup-path)  ; No changes made
          nil)))))
```

## Safe Lisp Code Editing for AI Agents

Working with Lisp code requires special attention to maintaining balanced parentheses and leveraging Emacs's structural editing capabilities. This section provides patterns and techniques for safely editing Lisp code programmatically.

### Leveraging Buffer Modes and Structural Editing

When editing Lisp files, the buffer's active modes can provide valuable functionality:

**Check active modes in a buffer:**
```elisp
(with-current-buffer "file.lisp"
  (list :major-mode major-mode
        :minor-modes (seq-filter (lambda (mode) (and (boundp mode) (symbol-value mode)))
                                 (mapcar #'car minor-mode-alist))
        :paredit-mode (bound-and-true-p paredit-mode)
        :slime-mode (bound-and-true-p slime-mode)))
```

### Essential Functions for Safe Lisp Editing

**Always verify parentheses balance:**
```elisp
(with-current-buffer "file.lisp"
  (condition-case err
      (progn 
        (check-parens)
        "Parentheses are balanced")
    (error (format "Parentheses error: %s" err))))
```

**Use structural navigation instead of text search:**
```elisp
;; Move by complete expressions
(forward-sexp)    ; Move forward one s-expression
(backward-sexp)   ; Move backward one s-expression
(up-list)         ; Move up one level of parentheses
(down-list)       ; Move down one level of parentheses

;; Get complete expressions safely
(thing-at-point 'sexp)    ; Get the s-expression at point
(bounds-of-thing-at-point 'sexp)  ; Get start/end positions
```

**Function-level navigation:**
```elisp
(beginning-of-defun)  ; Go to start of current function
(end-of-defun)        ; Go to end of current function
(mark-defun)          ; Select entire function
```

### Safer Editing Patterns

**Pattern 1: Small, Targeted S-Expression Edits**
```elisp
(defun safe-replace-sexp (filepath target-sexp replacement-sexp)
  "Safely replace one s-expression with another"
  (with-current-buffer (find-file filepath)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward target-sexp nil t)
        (backward-sexp)
        (mark-sexp)
        (delete-region (point) (mark))
        (insert replacement-sexp)
        (check-parens)  ; Verify balance
        (save-buffer)))))
```

**Pattern 2: Add New Function Definition**
```elisp
(defun safe-add-function (filepath new-function-code)
  "Add a new function to a Lisp file"
  (with-current-buffer (find-file filepath)
    (save-excursion
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n" new-function-code "\n")
      (check-parens)
      (when (buffer-modified-p)
        (save-buffer)))))
```

**Pattern 3: Modify Function Body**
```elisp
(defun safe-modify-function-body (filepath function-name new-body)
  "Replace the body of a specific function"
  (with-current-buffer (find-file filepath)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward (format "(defun %s" function-name) nil t)
        (beginning-of-defun)
        (forward-sexp 3)  ; Move past defun, name, and parameter list
        (let ((body-start (point)))
          (end-of-defun)
          (backward-char)  ; Move inside closing paren
          (delete-region body-start (point))
          (insert new-body)
          (check-parens)
          (save-buffer))))))
```

### Leveraging Paredit (When Available)

If paredit-mode is available, use its functions for guaranteed balanced edits:

```elisp
;; Check if paredit is available
(when (fboundp 'paredit-mode)
  ;; Enable paredit temporarily for safe editing
  (paredit-mode 1)
  
  ;; Use paredit functions:
  (paredit-wrap-round)        ; Wrap selection in parentheses
  (paredit-splice-sexp)       ; Remove surrounding parentheses
  (paredit-forward-slurp-sexp) ; Include next expression
  (paredit-forward-barf-sexp)  ; Exclude last expression
  (paredit-kill)              ; Delete to end of expression safely
  
  ;; Disable paredit when done
  (paredit-mode -1))
```

### Common Pitfalls and How to Avoid Them

**Problem: Unbalanced parentheses from string operations**
```elisp
;; BAD: String replacement can break balance
(replace-regexp-in-string "(old-function" "(new-function" code)

;; GOOD: Use structural editing
(goto-char (point-min))
(search-forward "(old-function")
(backward-sexp)
(forward-char)
(kill-word 1)
(insert "new-function")
```

**Problem: Editing inside strings or comments**
```elisp
;; Use syntax-aware functions
(defun in-string-or-comment-p ()
  "Check if point is inside a string or comment"
  (let ((state (syntax-ppss)))
    (or (nth 3 state)    ; Inside string
        (nth 4 state))))  ; Inside comment

;; Only edit if not in string/comment
(unless (in-string-or-comment-p)
  (perform-edit))
```

**Problem: Large block replacements breaking structure**
```elisp
;; BAD: Replace large blocks of code
(delete-region start end)
(insert new-code)

;; GOOD: Use multiple small, targeted edits
(save-excursion
  (goto-char start)
  (while (< (point) end)
    (when (looking-at target-pattern)
      (replace-match replacement))
    (forward-sexp)))
```

### Debugging and Recovery

**Validate edits incrementally:**
```elisp
(defun safe-edit-with-validation (filepath edit-function)
  "Edit file with automatic validation"
  (with-current-buffer (find-file filepath)
    (let ((original-content (buffer-string)))
      (condition-case err
          (progn
            (funcall edit-function)
            (check-parens)
            (save-buffer))
        (error
         ;; Restore original content on error
         (delete-region (point-min) (point-max))
         (insert original-content)
         (signal (car err) (cdr err)))))))
```

**Check syntax before saving:**
```elisp
(defun validate-lisp-syntax ()
  "Check if buffer contains valid Lisp syntax"
  (condition-case nil
      (progn
        (check-parens)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (read (current-buffer))))
        t)
    (error nil)))
```

### Best Practices Summary

1. **Always call `check-parens` after edits** - Catch balance errors immediately
2. **Use `save-excursion` for temporary navigation** - Preserve cursor position
3. **Prefer `forward-sexp`/`backward-sexp` over string search** - Respect structure
4. **Make small, targeted changes** - Easier to debug and validate
5. **Test with `thing-at-point 'sexp`** - Verify you're editing complete expressions
6. **Use paredit functions when available** - Guaranteed balanced operations
7. **Validate syntax before saving** - Catch errors early
8. **Keep backups for complex operations** - Enable recovery from mistakes

### Example: Safe Refactoring Workflow

```elisp
(defun safe-rename-function (filepath old-name new-name)
  "Safely rename a function throughout a file"
  (with-current-buffer (find-file filepath)
    (save-excursion
      ;; Step 1: Rename function definition
      (goto-char (point-min))
      (when (search-forward (format "(defun %s" old-name) nil t)
        (backward-word)
        (kill-word 1)
        (insert new-name)
        (check-parens))
      
      ;; Step 2: Rename function calls
      (goto-char (point-min))
      (while (search-forward (format "(%s" old-name) nil t)
        (backward-word)
        (when (looking-at (regexp-quote old-name))
          (kill-word 1)
          (insert new-name)
          (check-parens)))
      
      ;; Step 3: Final validation and save
      (when (validate-lisp-syntax)
        (save-buffer)
        (message "Function renamed successfully"))
      (unless (validate-lisp-syntax)
        (error "Syntax error after renaming - check manually")))))
```

This approach ensures that Lisp code modifications maintain structural integrity while leveraging Emacs's powerful editing capabilities.

## Testing the Connection

```bash
# Simple test
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(message \"Hello from Emacs!\")"}'

# Check server status  
curl -s http://localhost:7080/lisply/ping-lisp
# Should return: pong

# Test basic arithmetic (confirmed working)
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(+ 1 2 3)"}' 2>&1 | cat
# Returns: {"success":true,"result":"6","stdout":""}

# Get current buffer and directory
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(buffer-name)"}' 2>&1 | cat
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(pwd)"}' 2>&1 | cat
```

### Combined Workflow with Gendl
```bash
# Work with Gendl REPL (port 9081)
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(ql:quickload :modern-site)"}' 2>&1 | cat

# Edit Lisp files with Emacs (port 7080) for S-expression manipulation
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(find-file \"/path/to/file.lisp\")"}' 2>&1 | cat
```

## Real-World Example: Bulk Find and Replace

This example demonstrates the power of combining multiple Emacs Lisp operations in a single `progn` form to perform sophisticated file operations. This was used to replace "GendL" with "Gendl" across an entire codebase.

### Step 1: Discover Files
```bash
# Find all relevant files, excluding temp directories
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(length (remove-if (lambda (path) (search \"tmp-copies\" path)) (mapcar (quote namestring) (append (directory \"/home/dcooper8/projects/training/**/*.lisp\") (directory \"/home/dcooper8/projects/training/**/*.gdl\") (directory \"/home/dcooper8/projects/training/**/*.gendl\"))))"}' 2>&1 | cat
# Result: {"success":true,"result":"152","stdout":""}
```

### Step 2: Find Files Containing Target Pattern
```bash
# Define search function and find files containing "GendL"
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(progn (defun file-contains-pattern (filepath pattern) \"Check if file contains pattern\" (when (probe-file filepath) (with-open-file (stream filepath :direction :input :if-does-not-exist nil) (when stream (let ((content (make-string (file-length stream)))) (read-sequence content stream) (search pattern content)))))) (setq training-files (remove-if (lambda (path) (search \"tmp-copies\" path)) (mapcar (quote namestring) (append (directory \"/home/dcooper8/projects/training/**/*.lisp\") (directory \"/home/dcooper8/projects/training/**/*.gdl\") (directory \"/home/dcooper8/projects/training/**/*.gendl\"))))) (setq files-with-gendl (remove-if-not (lambda (file) (file-contains-pattern file \"GendL\")) training-files)) (list :count (length files-with-gendl) :files (subseq files-with-gendl 0 (min 5 (length files-with-gendl)))))"}' 2>&1 | cat
# Result: {"success":true,"result":"(:COUNT 44 :FILES (...))","stdout":""}
```

### Step 3: Bulk Replace Across All Files
```bash
# Define replacement functions and execute bulk replacement
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(progn (defun replace-in-file (filepath old-text new-text) \"Replace all occurrences of old-text with new-text in file\" (when (probe-file filepath) (let ((content)) (with-open-file (stream filepath :direction :input) (setq content (make-string (file-length stream))) (read-sequence content stream)) (let ((new-content (substitute-string content old-text new-text))) (when (not (string= content new-content)) (with-open-file (stream filepath :direction :output :if-exists :supersede) (write-sequence new-content stream)) t))))) (defun substitute-string (string old new) \"Replace all occurrences of old with new in string\" (let ((result (copy-seq string)) (old-len (length old)) (new-len (length new))) (loop with pos = 0 while (setq pos (search old result :start2 pos)) do (setq result (concatenate (quote string) (subseq result 0 pos) new (subseq result (+ pos old-len)))) (incf pos new-len)) result)) (setq files-to-update files-with-gendl) (setq update-count 0) (dolist (file files-to-update) (when (replace-in-file file \"GendL\" \"Gendl\") (incf update-count))) (list :files-updated update-count :total-files (length files-to-update)))"}' 2>&1 | cat
# Result: {"success":true,"result":"(:FILES-UPDATED 44 :TOTAL-FILES 44)","stdout":""}
```

### Step 4: Verification
```bash
# Verify replacement success
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(progn (setq total-old-count 0) (setq total-new-count 0) (dolist (file training-files) (when (file-contains-pattern file \"GendL\") (incf total-old-count)) (when (file-contains-pattern file \"Gendl\") (incf total-new-count))) (list :old-gendl-remaining total-old-count :new-gendl-found total-new-count))"}' 2>&1 | cat
# Result: {"success":true,"result":"(:OLD-GENDL-REMAINING 0 :NEW-GENDL-FOUND 48)","stdout":""}
```

### Key Techniques Demonstrated

1. **Function Definition in Context**: Define helper functions within the same `progn` block where they're used
2. **File Content Processing**: Read entire files into strings for pattern matching and replacement
3. **Bulk Operations**: Process multiple files in a single operation with iteration
4. **String Manipulation**: Custom string replacement without external dependencies
5. **Verification**: Built-in verification to confirm operation success
6. **Filtering**: Exclude unwanted directories/files using predicates

This approach replaced "GendL" with "Gendl" across 44 files in a single operation, demonstrating the power of Lisp for text processing and file manipulation tasks.

## Advanced File Operations: Directory Movement

Building on the bulk editing example, here's how to move entire directory trees using Emacs Lisp, demonstrated with moving a `sanskrit` directory to a different location.

### Complete Directory Tree Copy and Move
```bash
# Step 1: Copy all files with proper relative path handling
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(progn (defun copy-file-contents (source target) \"Copy file contents from source to target\" (ensure-directories-exist (directory-namestring target)) (with-open-file (in source :direction :input :element-type (quote (unsigned-byte 8))) (with-open-file (out target :direction :output :element-type (quote (unsigned-byte 8)) :if-exists :supersede) (let ((buffer (make-array 4096 :element-type (quote (unsigned-byte 8))))) (loop for bytes-read = (read-sequence buffer in) while (> bytes-read 0) do (write-sequence buffer out :end bytes-read)))))) (setq source-base \"/home/dcooper8/projects/training/sanskrit/\") (setq target-base \"/home/dcooper8/projects/xfer/sanskrit/\") (setq sanskrit-files (remove-if-not (lambda (f) (pathname-name f)) (directory (concatenate (quote string) source-base \"**/*\")))) (setq copied-count 0) (dolist (file sanskrit-files) (let* ((relative-path (subseq (namestring file) (length source-base))) (target-file (concatenate (quote string) target-base relative-path))) (copy-file-contents file target-file) (incf copied-count))) (list :files-copied copied-count :source-base source-base :target-base target-base))"}' 2>&1 | cat
# Result: {"success":true,"result":"(:FILES-COPIED 16 :SOURCE-BASE \"/home/dcooper8/projects/training/sanskrit/\" :TARGET-BASE \"/home/dcooper8/projects/xfer/sanskrit/\")","stdout":""}

# Step 2: Verify the copy operation
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(progn (setq xfer-files (remove-if-not (lambda (f) (pathname-name f)) (directory \"/home/dcooper8/projects/xfer/sanskrit/**/*\"))) (list :xfer-files-count (length xfer-files) :sample-files (mapcar (lambda (f) (subseq (namestring f) (length \"/home/dcooper8/projects/xfer/sanskrit/\"))) (subseq xfer-files 0 (min 5 (length xfer-files))))))"}' 2>&1 | cat
# Result: {"success":true,"result":"(:XFER-FILES-COUNT 16 :SAMPLE-FILES (\"vocab/animals.csv\" \"vocab/asanas-gpt.csv\" \"vocab/asanas-test.csv\" \"vocab/asanas.csv\" \"vocab/asanas.iast\"))","stdout":""}

# Step 3: Verify source and target states
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(list :training-sanskrit-exists (probe-file \"/home/dcooper8/projects/training/sanskrit/\") :xfer-sanskrit-exists (probe-file \"/home/dcooper8/projects/xfer/sanskrit/\") :xfer-file-count (length (remove-if-not (lambda (f) (pathname-name f)) (directory \"/home/dcooper8/projects/xfer/sanskrit/**/*\"))))"}' 2>&1 | cat
```

### Key File Operation Techniques

1. **Binary File Copying**: Use `(unsigned-byte 8)` element type for reliable copying of any file type
2. **Relative Path Handling**: Use `subseq` and `namestring` to maintain directory structure
3. **Directory Creation**: `ensure-directories-exist` automatically creates parent directories
4. **Bulk File Processing**: Combine file discovery, filtering, and operations in single expressions
5. **Stream-Based Operations**: Use buffered reading/writing for efficient large file handling

## Why Choose Emacs Lisp for File Operations?

### Advantages Over Shell Scripts

1. **Unified Language**: Same language for logic, file operations, and text processing
2. **Rich Data Structures**: Lists, property lists, and hash tables for complex operations
3. **Powerful String Manipulation**: Built-in functions for pattern matching and replacement
4. **Interactive Development**: Test and refine operations interactively via REPL
5. **Error Handling**: Comprehensive condition system with `ignore-errors` and custom handlers
6. **Cross-Platform**: Works identically on Unix, Windows, and macOS

### Pattern: Single-Expression File Operations

Instead of multiple shell commands:
```bash
find /path -name "*.ext" | xargs grep "pattern" | cut -d: -f1 | sort | uniq > files.txt
sed -i 's/old/new/g' $(cat files.txt)
rm files.txt
```

Use single Emacs Lisp expression:
```lisp
(progn
  (defun find-and-replace-in-files (directory pattern old new)
    (let ((files (remove-if-not 
                   (lambda (file) (file-contains-pattern file pattern))
                   (directory (concat directory "**/*." ext)))))
      (mapcar (lambda (file) (replace-in-file file old new)) files)))
  (find-and-replace-in-files "/path/" "pattern" "old" "new"))
```

### Advanced Patterns

**Conditional File Processing:**
```lisp
(dolist (file files)
  (cond ((string-match "\\.lisp$" file) (process-lisp-file file))
        ((string-match "\\.gdl$" file) (process-gdl-file file))
        (t (process-generic-file file))))
```

**File Content Analysis:**
```lisp
(mapcar (lambda (file)
          (list :file file
                :lines (count-lines-in-file file)
                :functions (count-function-definitions file)
                :size (file-size file)))
        source-files)
```

**Backup Before Modification:**
```lisp
(defun safe-replace-in-file (file old new)
  (let ((backup (concat file ".bak")))
    (copy-file-contents file backup)
    (when (replace-in-file file old new)
      (delete-file backup)
      t)))
```

## Integration with Other Tools


### MCP Integration Notes

When using this backend through the lisply-mcp wrapper:

- **Container Names**: Recent updates provide meaningful container names (`lisply-emacs`, `lisply-gendl`) instead of timestamps
- **Shared Networking**: Containers now share a Docker network (`lisply-mcp-network`) enabling inter-container communication
- **Port Mapping**: The Emacs backend typically runs on port 7081 (host) â 7080 (container) when managed by lisply-mcp
- **SLIME Integration**: The emacs container can connect to the gendl container via hostname `gendl-backend:4200` for SLIME/Swank connections

### Container Access for Humans

With meaningful container names, direct access becomes easy:

```bash
# Access the Emacs container directly
docker exec -it lisply-emacs emacsclient -t

# Access the Gendl container  
docker exec -it lisply-gendl ccl
```

### Architecture Benefits

The containerized approach provides:
- **Isolation**: File operations are contained within the container
- **Reproducibility**: Consistent environment across deployments  
- **Security**: Limited access to host filesystem via mounted volumes only
- **Scalability**: Multiple backend instances can run simultaneously


This backend is designed to work with:
- MCP (Model Context Protocol) clients
- Custom scripts and automation tools
- Development environments
- CI/CD pipelines

The HTTP interface makes it easy to integrate with any programming language or tool that can make HTTP requests.


## Avoiding Interactive Prompts in Automated Operations

When using this backend programmatically (especially via Claude or other AI agents), it's crucial to avoid operations that trigger interactive prompts, since there may not be a human user available to respond.

### Common Interactive Prompt Scenarios

1. **File modification conflicts**: When a file is open in a buffer and has been modified on disk
2. **Overwrite confirmations**: When saving would overwrite existing files
3. **Buffer revert prompts**: When Emacs detects external file changes

### Safe File Editing Pattern for AI Agents

Instead of basic `find-file` + edit + `save-buffer`, use this pattern:

```elisp
(defun safe-edit-file-automated (filepath edit-function)
  "Edit file safely for automated operations, avoiding interactive prompts"
  (let ((original-revert-without-query revert-without-query))
    (unwind-protect
        (progn
          ;; Add this file to revert-without-query to avoid prompts
          (add-to-list 'revert-without-query (file-name-nondirectory filepath))
          ;; Check if buffer exists and revert if needed
          (let ((buf (get-file-buffer filepath)))
            (when buf
              (with-current-buffer buf
                (revert-buffer t t t)))) ; ignore-auto, noconfirm, preserve-modes
          ;; Now do the edit
          (find-file filepath)
          (with-current-buffer (file-name-nondirectory filepath)
            (funcall edit-function)
            (save-buffer)
            t))
      ;; Cleanup: restore original revert-without-query
      (setq revert-without-query original-revert-without-query))))
```

**Usage example:**
```elisp
(safe-edit-file-automated 
  "/path/to/file.txt"
  (lambda ()
    (goto-char (point-min))
    (search-forward "old-text")
    (replace-match "new-text")
    (insert "\n;; Additional content")))
```

### Alternative: String-Based File Operations

For simple text replacements, string manipulation avoids buffer conflicts entirely:

```elisp
(let ((content (with-temp-buffer 
                 (insert-file-contents "/path/to/file.txt")
                 (buffer-string))))
  (setq modified-content 
        (replace-regexp-in-string "old-text" "new-text" content))
  (with-temp-file "/path/to/file.txt"
    (insert modified-content)))
```

### Key Principles for AI Agents

1. **Always handle file conflicts gracefully** - Use `revert-without-query` or string manipulation
2. **Prefer deterministic operations** - Avoid functions that might prompt for user input
3. **Use `unwind-protect`** - Ensure cleanup happens even if operations fail
4. **Test buffer state** - Check if files are already open before operations
5. **Wrap complex operations** - Use helper functions like `safe-edit-file-automated`

### What NOT to do in Automated Contexts

- Direct `find-file` + `save-buffer` on files that might be open elsewhere
- Operations that call `yes-or-no-p` or `y-or-n-p`
- Functions with `interactive` forms that expect user input
- File operations without checking `buffer-modified-p` status

By following these patterns, AI agents can perform file operations reliably without getting stuck on interactive prompts.

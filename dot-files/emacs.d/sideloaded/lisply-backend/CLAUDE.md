# Emacs Lisply Backend - Claude Usage Guide

## Overview

This Emacs Lisply Backend provides an HTTP API that allows Claude to interact with a running Emacs instance by evaluating Emacs Lisp code. This enables powerful text processing, buffer manipulation, file operations, and access to the full Emacs ecosystem.

## Related Documentation
- **Main Gendl Guide**: See `gendl` repository: `CLAUDE.md` for complete Gendl development workflow
- **Gendl Lisply Backend**: See `gendl` repository: `gwl/lisply-backend/CLAUDE.md` for Gendl REPL HTTP API
- This Emacs service (port 7080) complements the Gendl service (port 9081) for comprehensive development

## Endpoint

- **URL**: `http://localhost:7080/lisply/lisp-eval`
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
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(+ 1 2 3)"}'
# Returns: {"success":true,"result":"6","stdout":""}

curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(list 1 2 3)"}'
# Returns: {"success":true,"result":"(1 2 3)","stdout":""}
```

### 2. Buffer Operations

**List all open buffers:**
```bash
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(mapcar (lambda (buf) (buffer-name buf)) (buffer-list))"}'
```

**Get buffer contents:**
```bash
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(with-current-buffer \"*Messages*\" (buffer-string))"}'
```

**Get current buffer name:**
```bash
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(buffer-name)"}'
```

**Switch to a buffer:**
```bash
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(switch-to-buffer \"buffer-name\")"}'
```

### 3. File Operations

**Read file contents:**
```bash
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(with-temp-buffer (insert-file-contents \"/path/to/file\") (buffer-string))"}'
```

**Write to file:**
```bash
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(with-temp-file \"/path/to/file\" (insert \"content to write\"))"}'
```

**Check if file exists:**
```bash
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(file-exists-p \"/path/to/file\")"}'
```

### 4. Directory Operations

**List directory contents:**
```bash
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(directory-files \"/path/to/directory\")"}'
```

**Get current directory:**
```bash
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(pwd)"}'
```

### 5. Text Processing

**Search and replace in buffer:**
```bash
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(with-current-buffer \"buffer-name\" (goto-char (point-min)) (while (search-forward \"old\" nil t) (replace-match \"new\")))"}'
```

**Count lines in buffer:**
```bash
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(with-current-buffer \"buffer-name\" (count-lines (point-min) (point-max)))"}'
```

### 6. System Information

**Get Emacs version:**
```bash
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "emacs-version"}'
```

**Get loaded packages:**
```bash
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(mapcar (lambda (pkg) (symbol-name (car pkg))) package-alist)"}'
```

### 7. Interactive Commands

**Execute Emacs commands:**
```bash
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(call-interactively (quote save-buffer))"}'
```

### 8. Advanced Examples

**Find all .el files in directory recursively:**
```bash
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(directory-files-recursively \"/path/to/search\" \"\\\\.el$\")"}'
```

**Get all functions defined in current buffer:**
```bash
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(save-excursion (goto-char (point-min)) (let (functions) (while (re-search-forward \"^(defun \\\\([^[:space:]]+\\\\)\" nil t) (push (match-string 1) functions)) (nreverse functions)))"}'
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

## Testing the Connection

```bash
# Simple test
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(message \"Hello from Emacs!\")"}'

# Check server status  
curl -s http://localhost:7080/lisply/ping-lisp
# Should return: pong

# Test basic arithmetic (confirmed working)
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(+ 1 2 3)"}' 2>&1 | cat
# Returns: {"success":true,"result":"6","stdout":""}

# Get current buffer and directory
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(buffer-name)"}' 2>&1 | cat
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(pwd)"}' 2>&1 | cat
```

### Combined Workflow with Gendl
```bash
# Work with Gendl REPL (port 9081)
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(ql:quickload :modern-site)"}' 2>&1 | cat

# Edit Lisp files with Emacs (port 7080) for S-expression manipulation
curl -X POST http://localhost:7080/lisply/lisp-eval -d '{"code": "(find-file \"/path/to/file.lisp\")"}' 2>&1 | cat
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

This backend is designed to work with:
- MCP (Model Context Protocol) clients
- Custom scripts and automation tools
- Development environments
- CI/CD pipelines

The HTTP interface makes it easy to integrate with any programming language or tool that can make HTTP requests.
# Emacs Lisply Backend - Claude Usage Guide

## Overview

This Emacs Lisply Backend provides MCP (Model Context Protocol) integration that allows Claude to interact with a running Emacs instance by evaluating Emacs Lisp code. This enables powerful text processing, buffer manipulation, file operations, and access to the full Emacs ecosystem.

## MCP Integration

**Current Access Method**: Direct MCP services (not HTTP)
- **Service Name**: `mcp__skewed-emacs__skewed-emacs__lisp_eval`
- **Purpose**: Evaluate Emacs Lisp code remotely via MCP
- **Usage**: `mcp__skewed-emacs__skewed-emacs__lisp_eval(code="(+ 1 2 3)")`

**Legacy HTTP Access** (deprecated):
- **URL**: `http://localhost:7081/lisply/lisp-eval` 
- **Method**: POST with JSON body

## Related Documentation
- **Main Development Guide**: See `/projects/CLAUDE.md` for complete development workflow
- **Gendl Integration**: MCP service `mcp__genworks-gdl-smp__genworks-gdl-smp__lisp_eval` for Gendl REPL
- **Container Environment**: See `/projects/skewed-emacs/CLAUDE.md` for Docker setup

## Basic MCP Usage Examples

### 1. Simple Evaluation
```python
# Basic arithmetic
mcp__skewed_emacs__skewed_emacs__lisp_eval(code="(+ 1 2 3)")
# Returns: {"Result": "6", "Stdout": ""}

# List operations  
mcp__skewed_emacs__skewed_emacs__lisp_eval(code="(list 1 2 3)")
# Returns: {"Result": "(1 2 3)", "Stdout": ""}
```

### 2. Buffer Operations
```python
# List all open buffers
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='(mapcar (lambda (buf) (buffer-name buf)) (buffer-list))'
)

# Get buffer contents
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='(with-current-buffer "*Messages*" (buffer-string))'
)

# Get current buffer info
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='(list :name (buffer-name) :file (buffer-file-name) :modified (buffer-modified-p))'
)
```

### 3. File Operations  
```python
# Read file contents
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='(with-temp-buffer (insert-file-contents "/path/to/file") (buffer-string))'
)

# Check if file exists
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='(file-exists-p "/path/to/file")'
)

# List directory contents
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='(directory-files "/path/to/directory")'
)
```

## CRITICAL: Safe Lisp Code Editing with Paredit

**â ï¸ WARNING: Always enable paredit mode before editing Lisp files!**

When editing any `.lisp`, `.el`, `.gendl`, or similar files, you MUST enable paredit mode first to prevent unbalanced parentheses that will break syntax.

### Step 1: Always Enable Paredit First

```python
# REQUIRED: Enable paredit mode before any Lisp editing
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(with-current-buffer "file.el"
               (when (fboundp 'paredit-mode)
                 (paredit-mode 1)))'''
)
```

### Step 2: Verify Parentheses Balance

```python
# REQUIRED: Check balance before and after edits
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(with-current-buffer "file.el"
               (condition-case err
                   (progn (check-parens) "Balanced")
                 (error (format "Error: %s" err))))'''
)
```

### Why Paredit Mode is Mandatory

**Problem**: Without paredit mode, simple text operations can easily break parentheses balance:
```elisp
;; BAD: This can break syntax
(replace-regexp-in-string "(old-function" "(new-function" code)
```

**Solution**: Paredit mode provides structural editing that maintains balance:
```elisp
;; GOOD: Structural editing with paredit
(when (fboundp 'paredit-mode) (paredit-mode 1))
(goto-char (point-min))
(search-forward "(old-function")
(backward-sexp)
(forward-char)
(paredit-kill-word)
(insert "new-function")
```

### Safe Lisp Editing Workflow

**Step-by-Step Safe Editing Pattern:**
```python
# 1. Open file and enable paredit
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(progn
               (find-file "/path/to/file.el")
               (when (fboundp 'paredit-mode)
                 (paredit-mode 1))
               "Ready for editing")'''
)

# 2. Make edits using structural navigation
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(with-current-buffer "file.el"
               (goto-char (point-min))
               (search-forward "(defun old-name")
               (backward-sexp)
               (forward-char)
               (forward-word)
               (paredit-kill-word)
               (insert "new-name"))'''
)

# 3. Verify balance before saving
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(with-current-buffer "file.el"
               (check-parens)
               (save-buffer))'''
)
```

### Essential Paredit Functions

**Basic Structural Movement:**
```python
# Move by complete expressions (safer than character movement)
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(progn
               (forward-sexp)    ; Move forward one s-expression
               (backward-sexp)   ; Move backward one s-expression  
               (up-list)         ; Move up one parentheses level
               (down-list))      ; Move down one parentheses level'''
)
```

**Safe Editing Operations:**
```python
# Guaranteed balanced operations
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(progn
               (paredit-wrap-round)        ; Wrap in parentheses
               (paredit-splice-sexp)       ; Remove surrounding parens
               (paredit-forward-slurp-sexp) ; Include next expression
               (paredit-kill))             ; Delete safely to end'''
)
```

### Common Editing Patterns

**Pattern 1: Safe Function Renaming**
```python
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(defun safe-rename-function (filepath old-name new-name)
               "Safely rename a function with paredit"
               (with-current-buffer (find-file filepath)
                 (when (fboundp 'paredit-mode) (paredit-mode 1))
                 (save-excursion
                   (goto-char (point-min))
                   (when (search-forward (format "(defun %s" old-name) nil t)
                     (backward-word)
                     (paredit-kill-word)
                     (insert new-name)
                     (check-parens)))
                 (save-buffer)))'''
)
```

**Pattern 2: Safe Code Insertion**
```python
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(defun safe-add-function (filepath new-function-code)
               "Add new function with validation"
               (with-current-buffer (find-file filepath)
                 (when (fboundp 'paredit-mode) (paredit-mode 1))
                 (save-excursion
                   (goto-char (point-max))
                   (unless (bolp) (insert "\n"))
                   (insert "\n" new-function-code "\n")
                   (check-parens))
                 (when (buffer-modified-p) (save-buffer))))'''
)
```

## General File Editing Best Practices

### Buffer Operations vs String Manipulation

**Recommended: Buffer-Based Editing**
```python
# Efficient, native Emacs approach
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(progn
               (find-file "/path/to/file.js")
               (with-current-buffer "file.js"
                 (goto-char (point-min))
                 (search-forward "old-code")
                 (replace-match "new-code")
                 (save-buffer)))'''
)
```

**Discouraged: String Manipulation**
```python
# Less efficient, bypasses Emacs optimizations
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(let ((content (with-temp-buffer
                             (insert-file-contents "/path/to/file.js")
                             (buffer-string))))
               (setq modified-content 
                     (replace-regexp-in-string "old-code" "new-code" content))
               (with-temp-file "/path/to/file.js"
                 (insert modified-content)))'''
)
```

### Why Buffer Operations Are Superior

1. **Memory Efficiency**: Emacs C code manages buffers optimally
2. **Incremental Changes**: Small targeted edits vs full file reprocessing  
3. **Native Features**: Syntax highlighting, auto-formatting, language modes
4. **Performance**: Optimized buffer operations vs slow string manipulation
5. **Undo/Redo**: Built-in change tracking
6. **Error Recovery**: Better state management

### Safe File Editing Pattern for Automation

```python
# Avoid interactive prompts in automated contexts
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(defun safe-edit-file-automated (filepath edit-function)
               "Edit file safely for automated operations"
               (let ((original-revert-without-query revert-without-query))
                 (unwind-protect
                     (progn
                       ;; Avoid file conflict prompts
                       (add-to-list 'revert-without-query (file-name-nondirectory filepath))
                       ;; Revert existing buffer if needed
                       (let ((buf (get-file-buffer filepath)))
                         (when buf
                           (with-current-buffer buf
                             (revert-buffer t t t))))
                       ;; Perform edit
                       (find-file filepath)
                       (with-current-buffer (file-name-nondirectory filepath)
                         (funcall edit-function)
                         (save-buffer)
                         t))
                   ;; Cleanup
                   (setq revert-without-query original-revert-without-query))))'''
)
```

## Advanced Examples

### Multi-File Operations
```python
# Process multiple files safely
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(progn
               (defun process-lisp-files (directory pattern old-text new-text)
                 "Process multiple Lisp files with paredit safety"
                 (let ((files (directory-files-recursively directory pattern)))
                   (dolist (file files)
                     (when (string-match "\\.\\(el\\|lisp\\|gendl\\)$" file)
                       (with-current-buffer (find-file file)
                         (when (fboundp 'paredit-mode) (paredit-mode 1))
                         (save-excursion
                           (goto-char (point-min))
                           (while (search-forward old-text nil t)
                             (replace-match new-text)))
                         (check-parens)
                         (when (buffer-modified-p) (save-buffer)))))))
               "Function defined")'''
)
```

### Debugging and Recovery
```python
# Validate edits incrementally  
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(defun safe-edit-with-validation (filepath edit-function)
               "Edit with automatic rollback on error"
               (with-current-buffer (find-file filepath)
                 (let ((original-content (buffer-string)))
                   (condition-case err
                       (progn
                         (when (fboundp 'paredit-mode) (paredit-mode 1))
                         (funcall edit-function)
                         (check-parens)
                         (save-buffer))
                     (error
                      ;; Restore on any error
                      (delete-region (point-min) (point-max))
                      (insert original-content)
                      (signal (car err) (cdr err)))))))'''
)
```

## Testing and Verification

### Basic Connection Test
```python
# Test MCP service availability
mcp__skewed_emacs__skewed_emacs__ping_lisp()
# Should return: "pong"

# Test basic evaluation
mcp__skewed_emacs__skewed_emacs__lisp_eval(code="(+ 1 2 3)")
# Should return: {"Result": "6", "Stdout": ""}
```

### Buffer State Verification
```python
# Check current Emacs state
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(list :current-buffer (buffer-name)
                  :emacs-version emacs-version
                  :paredit-available (fboundp 'paredit-mode)
                  :active-buffers (length (buffer-list)))'''
)
```

## Key Principles Summary

### For All File Editing:
1. **Use buffer operations** over string manipulation
2. **Check buffer state** before and after edits  
3. **Handle interactive prompts** with `revert-without-query`
4. **Use `save-excursion`** to preserve cursor position
5. **Wrap operations** in `condition-case` for error handling

### For Lisp Code Editing (MANDATORY):
1. **Always enable paredit mode first** - `(paredit-mode 1)`
2. **Check parentheses balance** - `(check-parens)` before saving
3. **Use structural navigation** - `forward-sexp`, `backward-sexp`  
4. **Make small targeted changes** instead of large replacements
5. **Use paredit functions** - `paredit-kill-word`, `paredit-wrap-round`
6. **Validate syntax** before saving with error recovery
7. **Test with `thing-at-point 'sexp`** to verify complete expressions

### What NOT to Do:
- Edit Lisp files without paredit mode
- Use string replacement on parentheses  
- Make large block replacements in Lisp code
- Skip `check-parens` validation
- Use `find-file` + `save-buffer` without prompt handling

## Error Recovery

If you encounter unbalanced parentheses:

```python
# Diagnose the problem
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(condition-case err
                (check-parens)
              (error (format "Balance error at: %s" err)))'''
)

# Restore from backup if available
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(when (file-exists-p "/path/to/file.el.backup")
               (copy-file "/path/to/file.el.backup" "/path/to/file.el" t))'''
)
```

## Integration Notes

- **Container Environment**: Runs in Docker containers with shared networking
- **MCP Protocol**: Direct integration without HTTP wrapper  
- **File Access**: Mounted volumes provide access to host filesystem
- **SLIME Integration**: Can connect to Gendl containers for Common Lisp development
- **Development Mode**: Live reloading available in development environments

## Lessons Learned

**Why Paredit Instructions Were Initially Missed:**

The original document buried paredit instructions deep in the text (around line 432) and presented them as optional ("when available") rather than mandatory. The basic editing examples didn't mention paredit at all, leading to syntax errors when editing Lisp files.

**Solution**: This updated guide makes paredit mode usage the first and most prominent instruction for any Lisp file editing, with clear warnings about the consequences of not using it.

The key insight: **Structural editing is not optional for Lisp - it's essential for maintaining code integrity.**

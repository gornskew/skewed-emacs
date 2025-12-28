# Emacs Lisply Backend - Claude Usage Guide

## ⚠️ CRITICAL WARNING: Shared Emacs Instance

**You are connected to a SHARED Emacs instance with an active user!**

### Key Risks and Safety Measures:

1. **Shared Global Current Buffer**: You and the user share the same "current buffer" state
   - When you do `(find-file "file.lisp")`, it changes the user's current buffer
   - When the user switches buffers, it affects your operations
   - **Solution**: ALWAYS use `(with-current-buffer "buffer-name" ...)` patterns

2. **User Interference**: The user may switch buffers while you're working
   - Your `(search-forward "text")` might search in the wrong buffer
   - **Solution**: Use explicit buffer targeting for ALL operations

3. **Cursor Position Conflicts**: Cursor movements affect both you and the user
   - **Solution**: Use `(save-excursion ...)` to preserve positions

### Safe MCP Patterns (REQUIRED):

```elisp
;; BAD: Relies on global state
(find-file "/path/to/file.lisp")
(search-forward "target")
(insert "new-text")

;; GOOD: Explicit buffer targeting
(with-current-buffer (find-file-noselect "/path/to/file.lisp")
  (save-excursion
    (goto-char (point-min))
    (search-forward "target")
    (insert "new-text"))
  (save-buffer))
```

**Always test operations with explicit buffer names, never rely on "current buffer"!**

---

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

## Shared Buffer Footgun Examples

### The Problem We Discovered
During viewport menu development, we encountered this exact issue:

```elisp
;; Claude executed this:
(find-file "/projects/gendl/gwl-graphics/gwl/source/viewport-html-div.lisp")
(search-forward "Test Menu")  ; This failed!

;; Why it failed: User had switched to *claude* buffer in between!
;; The search happened in the wrong buffer
```

### The Solution: Always Use Explicit Buffer Targeting

```elisp
;; SAFE: Works regardless of what user is doing
(with-current-buffer (find-file-noselect "/path/to/file.lisp")
  (goto-char (point-min))
  (search-forward "target-text")
  (point))  ; Returns position found
```

### Buffer State Verification

```python
# Always verify you're in the right buffer
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(with-current-buffer "viewport-html-div.lisp"
               (list :buffer-name (buffer-name)
                     :buffer-size (buffer-size)
                     :buffer-file (buffer-file-name)))'''
)
```

---

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

**Step-by-Step Safe Editing Pattern (Shared-Buffer-Safe):**
```python
# 1. Open file and enable paredit (SHARED-BUFFER-SAFE VERSION)
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(let ((buf (find-file-noselect "/path/to/file.el")))
               (with-current-buffer buf
                 (when (fboundp 'paredit-mode)
                   (paredit-mode 1))
                 "Ready for editing"))'''
)

# 2. Make edits using structural navigation (EXPLICIT BUFFER)
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(with-current-buffer "file.el"
               (save-excursion
                 (goto-char (point-min))
                 (search-forward "(defun old-name")
                 (backward-sexp)
                 (forward-char)
                 (forward-word)
                 (paredit-kill-word)
                 (insert "new-name")))'''
)

# 3. Verify balance before saving (BUFFER-SAFE)
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(with-current-buffer "file.el"
               (condition-case err
                   (progn (check-parens)
                          (save-buffer)
                          "File saved successfully")
                 (error (format "Error: %s" err))))'''
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

## CRITICAL: Detecting and Handling Unbalanced Buffers

**â ï¸ WARNING: LLM agents can accidentally circumvent paredit-mode guardrails!**

### How Unbalanced States Occur

**Two scenarios lead to unbalanced buffers:**

1. **LLM Agent Circumvention (Agent's Fault):**
   - The MCP `lisp_eval` tool allows arbitrary Elisp evaluation
   - This includes running shell commands: `(shell-command "sed -i 's/foo/bar/' file.el")`
   - External editors invoked via shell bypass paredit-mode completely
   - **This has been observed in the field** - agents sometimes resort to shell-based editing
   - Result: Paredit-mode enabled but buffer contains unbalanced code

2. **External File Modification (Not Agent's Fault):**
   - User edits file in external editor
   - Emacs auto-reverts the buffer
   - Paredit-mode remains enabled despite unbalanced content from disk
   - Result: Same inconsistent state

### Why This is Dangerous

**Normal users CANNOT create this state:**
- Paredit-mode prevents unbalanced edits through normal Emacs commands
- Paredit-mode refuses to enable in an already-unbalanced buffer

**But LLM agents CAN create this state:**
- Shell commands bypass Emacs entirely: `(shell-command "sed ...")`
- Direct file writes bypass buffer protections: `(write-region ... overwrite)`
- String manipulation then write: `(with-temp-file ... (insert (replace-regexp-in-string ...)))`

**This inconsistent state breaks all assumptions:**
- Paredit-mode thinks it's safe to edit, but isn't
- Structural navigation commands (`forward-sexp`) fail
- Further edits can cascade into worse corruption
- Syntax becomes unparseable

### MANDATORY: Pre-Edit Balance Check

**ALWAYS check balance BEFORE any editing or analysis of Lisp files:**

```python
# REQUIRED before any Lisp file operation
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(with-current-buffer "file.el"
               (condition-case err
                   (progn
                     (check-parens)
                     (list :status "BALANCED" :safe-to-edit t))
                 (error (list :status "UNBALANCED"
                             :safe-to-edit nil
                             :error (error-message-string err)))))'''
)
```

**If unbalanced detected:**
1. â **STOP IMMEDIATELY** - Do not attempt any edits
2. â **Report to user** with clear diagnostic information
3. â **Back off** - Let user fix manually
4. â **Do not try to fix it yourself** - This rarely goes well

### Detection Pattern for Confusing States

**Check for the impossible state (paredit-mode ON but unbalanced):**

```python
# Detect the confusing state
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(with-current-buffer "file.el"
               (list :paredit-enabled paredit-mode
                     :balanced (condition-case err
                                   (progn (check-parens) t)
                                 (error nil))
                     :auto-reverted auto-revert-mode))'''
)
```

**Diagnostic Results:**
- `{:paredit-enabled t, :balanced nil}` â **CONFUSING STATE** - agent or external edit caused this
- `{:paredit-enabled t, :balanced t}` â Safe to proceed
- `{:paredit-enabled nil, :balanced nil}` â Expected for non-Lisp or damaged files

### Best Practices to Avoid Creating Unbalanced States

**â DO NOT use these methods for editing Lisp files:**
```python
# WRONG: Shell-based editing bypasses paredit
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='(shell-command "sed -i \"s/old/new/\" /path/to/file.el")'
)

# WRONG: String manipulation then write bypasses paredit
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(let ((content (with-temp-buffer
                              (insert-file-contents "/path/to/file.el")
                              (buffer-string))))
                (with-temp-file "/path/to/file.el"
                  (insert (replace-regexp-in-string "old" "new" content))))'''
)

# WRONG: Direct file write bypasses paredit
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='(write-region "(defun foo" nil "/path/to/file.el" append)'
)
```

**â DO use native Emacs buffer operations with paredit:**
```python
# RIGHT: Buffer-based editing with paredit protection
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(with-current-buffer (find-file-noselect "/path/to/file.el")
               (save-excursion
                 (when (fboundp 'paredit-mode) (paredit-mode 1))
                 (goto-char (point-min))
                 (search-forward "old")
                 (paredit-kill-word)
                 (insert "new")
                 (check-parens)
                 (save-buffer)))'''
)
```

### When Rogue Editing Might Be Justified

**Very rarely, shell-based editing might be necessary for:**
- Bulk operations across hundreds of files where buffer operations are too slow
- Files too large for Emacs to handle efficiently
- Non-Lisp files where paredit doesn't apply

**If you must use rogue editing methods:**
1. â Document why native Emacs methods won't work
2. â Use only on non-Lisp files, or
3. â If on Lisp files, immediately verify balance afterward
4. â Be prepared to detect and report any resulting imbalance

### Recovery from Unbalanced State

**If you detect an unbalanced buffer, report like this:**

```
â ï¸ UNBALANCED BUFFER DETECTED

File: /path/to/file.el
Status: Paredit-mode enabled but buffer is UNBALANCED
Error: "Unbalanced parentheses" at position 2552

Possible Causes:
1. Shell-based editing bypassed paredit-mode
2. External file modification while buffer was open
3. String manipulation write bypassed paredit-mode

Action Required:
Please manually fix the unbalanced parentheses/quotes.
I am backing off and will not attempt any edits.
```

### Summary: The Golden Rules

1. **ALWAYS check balance** before editing or analyzing Lisp files
2. **NEVER use shell commands** to edit Lisp files
3. **NEVER use string manipulation + write** for Lisp files
4. **ALWAYS use buffer operations** with paredit-mode enabled
5. **If unbalanced detected**: Report clearly and back off immediately
6. **If using rogue methods**: Double-check balance afterward
7. **Remember**: Paredit-mode on + unbalanced = IMPOSSIBLE for humans, POSSIBLE for agents

## PAREDIT COMMAND REFERENCE

### Critical Paredit Commands for Structural Editing

**🔥 MOST IMPORTANT: Slurping and Barfing**
These are the core paredit operations that safely restructure code:

```python
# SLURP: Pull expressions into current parentheses group
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(progn
               ;; Forward slurp: (foo bar) baz -> (foo bar baz)
               (paredit-forward-slurp-sexp)
               ;; Backward slurp: foo (bar baz) -> (foo bar baz)  
               (paredit-backward-slurp-sexp))'''
)

# BARF: Push expressions out of current parentheses group  
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(progn
               ;; Forward barf: (foo bar baz) -> (foo bar) baz
               (paredit-forward-barf-sexp)
               ;; Backward barf: (foo bar baz) -> foo (bar baz)
               (paredit-backward-barf-sexp))'''
)
```

**Essential Structural Operations:**
```python
# Wrapping and unwrapping
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(progn
               (paredit-wrap-round)        ; Wrap selection in ()
               (paredit-wrap-square)       ; Wrap selection in []
               (paredit-wrap-curly)        ; Wrap selection in {}
               (paredit-splice-sexp))      ; Remove surrounding parens'''
)

# Safe deletion
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(progn
               (paredit-kill)              ; Kill to end of sexp
               (paredit-kill-word)         ; Kill word maintaining balance
               (paredit-backward-kill-word); Kill word backward
               (paredit-splice-sexp-killing-backward)  ; Remove parens, kill backward
               (paredit-splice-sexp-killing-forward))  ; Remove parens, kill forward'''
)
```

**Navigation Commands:**
```python
# Structural movement (ALWAYS use these instead of character movement)
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(progn
               (paredit-forward)           ; Move forward over sexp
               (paredit-backward)          ; Move backward over sexp
               (paredit-forward-down)      ; Move down into sexp
               (paredit-backward-up)       ; Move up out of sexp
               (paredit-forward-up)        ; Move up and forward
               (paredit-backward-down))    ; Move down and backward'''
)
```

### Paredit Editing Workflow Examples

**Example 1: Adding a parameter to a function call**
```python
# Start: (my-function arg1 arg2)
# Goal:  (my-function new-arg arg1 arg2)

mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(progn
               ;; 1. Position cursor after opening paren
               (search-forward "(my-function")
               (forward-char 1)  ; After the opening paren
               ;; 2. Insert new parameter
               (insert "new-arg ")
               ;; Paredit automatically maintains balance!
               "Parameter added safely")'''
)
```

**Example 2: Restructuring nested expressions**
```python
# Start: (if condition (do-something arg1 arg2))
# Goal:  (when condition (do-something arg1 arg2))

mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(progn
               ;; 1. Position on 'if'
               (search-forward "(if")
               (forward-char 1)
               ;; 2. Kill the word 'if'
               (paredit-kill-word)
               ;; 3. Insert 'when'
               (insert "when")
               "Converted if to when safely")'''
)
```

**Example 3: Extracting an expression**
```python
# Start: (foo (bar baz) qux)
# Goal:  (bar baz) (foo qux)

mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(progn
               ;; 1. Position on (bar baz)
               (search-forward "(bar")
               (backward-char 1)  ; On opening paren of (bar baz)
               ;; 2. Mark the expression
               (mark-sexp)
               ;; 3. Kill it
               (paredit-kill)
               ;; 4. Go to appropriate location and yank
               (beginning-of-line)
               (yank)
               (insert " ")
               "Expression extracted safely")'''
)
```

### Common Paredit Mistakes to Avoid

**❌ DON'T DO THESE:**
```python
# NEVER use these for Lisp editing:
# - delete-char, delete-backward-char
# - kill-line, kill-word  
# - replace-string, replace-regexp on parentheses
# - Manual parentheses insertion/deletion

# WRONG: This breaks balance
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(progn
               (search-forward "(old-name")
               (delete-char 10)  ; BAD: Can break parentheses
               (insert "(new-name"))'''  # DANGEROUS
)
```

**✅ DO THIS INSTEAD:**
```python
# RIGHT: Use paredit structural editing
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(progn
               (search-forward "(old-name")
               (forward-char 1)  ; Move inside parentheses
               (paredit-kill-word)  ; Safely remove 'old-name'
               (insert "new-name"))'''  # Safe replacement
)
```

### Quick Reference: Most Used Paredit Commands

| Task | Command | Example |
|------|---------|---------|
| Add to group | `paredit-forward-slurp-sexp` | `(a b) c` → `(a b c)` |
| Remove from group | `paredit-forward-barf-sexp` | `(a b c)` → `(a b) c` |
| Wrap in parens | `paredit-wrap-round` | `a b` → `(a b)` |
| Remove parens | `paredit-splice-sexp` | `(a b)` → `a b` |
| Safe kill word | `paredit-kill-word` | `(old-name ...)` → `( ...)` |
| Kill to end | `paredit-kill` | `(a b c d)` → `(a b)` |
| Move by sexp | `forward-sexp` / `backward-sexp` | Navigate structures |

### Learning Resources

**Official Paredit Documentation:**
- In Emacs: `C-h f paredit-mode` or `M-x describe-function paredit-mode`
- Paredit cheat sheet: `paredit-cheatsheet` (if installed)
- Tutorial: `paredit-tutorial` (interactive learning)

**Key Concept: "Slurping and Barfing"**
- **Slurp** = "eat" or "absorb" - pull expressions into the current parenthetical group
- **Barf** = "spit out" - push expressions out of the current parenthetical group
- These operations are the foundation of structural editing

**Memory Aid:**
- Think of parentheses as containers that can grow/shrink
- Slurp = container gets bigger (eats more)
- Barf = container gets smaller (spits out content)

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

### Safe File Editing Pattern for Automation (Shared-Buffer-Safe)

```python
# UPDATED: Avoid interactive prompts AND shared buffer conflicts
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(defun safe-edit-file-automated-shared-safe (filepath edit-function)
               "Edit file safely for automated operations with shared buffer protection"
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
                       ;; SHARED-BUFFER-SAFE: Use find-file-noselect + explicit buffer
                       (let ((buffer (find-file-noselect filepath)))
                         (with-current-buffer buffer
                           (funcall edit-function)
                           (save-buffer)
                           t)))
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
- Use `with-temp-buffer` then call org-mode functions (causes warnings)

## Org-Mode Buffer Considerations

When working with `.org` files, be careful about using `with-temp-buffer` combined with org functions:

**â BAD: Causes warnings in *Warnings* buffer**
```elisp
(with-temp-buffer
  (insert-file-contents "/path/to/file.org")
  (org-end-of-subtree t)  ; ERROR: temp buffer is in fundamental-mode
  ...)
```

**â GOOD: Use the actual org buffer**
```elisp
(with-current-buffer (find-file-noselect "/path/to/file.org")
  (save-excursion
    (goto-char (point-min))
    (search-forward "some heading")
    (org-end-of-subtree t)  ; Works: buffer is in org-mode
    ...))
```

**Why this matters:** Org functions like `org-end-of-subtree`, `org-entry-get`, `org-map-entries`, etc. require the buffer to be in `org-mode`. Temp buffers default to `fundamental-mode`, causing `'org-element-at-point' cannot be used in non-Org buffer` warnings.

**Simple rule:** 
- For **reading** file contents only â `with-temp-buffer` + `insert-file-contents` is fine
- For **org structural navigation** â use `find-file-noselect` and work in the real buffer

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

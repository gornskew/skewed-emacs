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

## Integration with Other Tools

This backend is designed to work with:
- MCP (Model Context Protocol) clients
- Custom scripts and automation tools
- Development environments
- CI/CD pipelines

The HTTP interface makes it easy to integrate with any programming language or tool that can make HTTP requests.
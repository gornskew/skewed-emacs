# Skewed Emacs + Gendl Docker Development Environment

This file provides guidance to Claude Code (claude.ai/code) when working with the integrated Skewed Emacs and Gendl development environment.

## Overview

This setup provides a complete Lisp development environment with:
- **Skewed Emacs Container**: Custom Emacs configuration with MCP integration
- **Gendl Container**: 3D CAD/modeling system with Common Lisp REPL and MCP integration
- **Network Integration**: Containers communicate via Docker network for SLIME connections
- **MCP Services**: Both containers expose services via Model Context Protocol for external tool integration

## MCP Integration

The containers are now wrapped as MCP (Model Context Protocol) services, providing seamless integration with Claude Code and other MCP-enabled tools.

### Available MCP Services

**Emacs Lisp Evaluation Service:**
- **Service Name**: `mcp__skewed-emacs__skewed-emacs__lisp_eval`
- **Purpose**: Evaluate Emacs Lisp code remotely
- **Usage**: `mcp__skewed-emacs__skewed-emacs__lisp_eval(code="(+ 1 2 3)")`

**Gendl Lisp Evaluation Service:**
- **Service Name**: `mcp__gendl__gendl__lisp_eval`  
- **Purpose**: Evaluate Common Lisp code in Gendl environment
- **Usage**: `mcp__gendl__gendl__lisp_eval(code="(+ 1 2 3)")`

**Ping Services:**
- `mcp__skewed-emacs__skewed-emacs__ping_lisp` - Check Emacs service availability
- `mcp__gendl__gendl__ping_lisp` - Check Gendl service availability

### MCP vs Raw HTTP

**Previous Approach (Deprecated):**
```bash
# Raw HTTP calls (no longer recommended)
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(+ 1 2 3)"}'
```

**Current Approach (Recommended):**
```python
# Through MCP services (seamless with Claude Code)
mcp__skewed_emacs__skewed_emacs__lisp_eval(code="(+ 1 2 3)")
```


### Verification Commands

Check if workarounds are active:
```bash
# Test current environment via MCP
mcp__skewed_emacs__skewed_emacs__lisp_eval(code='(list (getenv "SHELL") shell-file-name (getenv "PATH"))')

# Test native compilation settings
mcp__skewed_emacs__skewed_emacs__lisp_eval(code='native-comp-jit-compilation')

# Test assembler accessibility
mcp__skewed_emacs__skewed_emacs__lisp_eval(code='(shell-command-to-string "which as")')
```

## Quick Start

### 1. Start the Environment

```bash
# Navigate to skewed-emacs directory
cd ~/projects/skewed-emacs

# Create shared Docker network (one-time setup)
docker network create emacs-gendl-network

# Start Gendl container
cd dot-files/emacs.d/etc/gendl/docker && ./run --network

# Start Skewed Emacs container  
cd ~/projects/skewed-emacs/docker && ./run --network

# Verify both services are running via MCP
mcp__skewed_emacs__skewed_emacs__ping_lisp()  # Should return "pong"
mcp__gendl__gendl__ping_lisp()                # Should return "pong"
```

### 2. Connect to Development Environment

```bash
# Connect to Emacs in the container
docker exec -it skewed-emacs emacsclient -t

# From within Emacs, connect to Gendl SLIME
# M-x slime-connect RET gendl-ccl RET 4200 RET
```

## Container Details

### Skewed Emacs Container (`skewed-emacs`)
- **Base**: Custom Emacs configuration
- **Network Name**: `skewed-emacs` (accessible as `skewed-emacs:7080` from other containers)
- **Host Ports**: 
  - `7081` → `7080` (HTTP API - deprecated in favor of MCP)
- **MCP Service**: Available via `mcp__skewed-emacs__*` functions
- **Mount**: `/home/dcooper8/projects` → `/projects`

### Gendl Container (`gendl-ccl`)  
- **Base**: `genworks/gendl:devo-ccl`
- **Network Name**: `gendl-ccl` (accessible as `gendl-ccl:4200` and `gendl-ccl:9080` from other containers)
- **Host Ports**:
  - `4201` → `4200` (Swank/SLIME)
  - `9081` → `9080` (HTTP API - deprecated in favor of MCP)
- **MCP Service**: Available via `mcp__gendl__*` functions
- **Mount**: `/home/dcooper8/projects` → `/home/gendl-user/projects`

### Docker Network
- **Network Name**: `emacs-gendl-network`
- **Purpose**: Enables container-to-container communication
- **Key Benefit**: Allows SLIME connection from Skewed Emacs to Gendl Swank server

## Development Workflow

### 1. Basic SLIME Development
```elisp
;; In Skewed Emacs container, after slime-connect to gendl-ccl:4200
;; Load Quicklisp
(load-quicklisp)

;; Add project directories
(pushnew "~/projects/gendl/demos/" ql:*local-project-directories* :test #'equalp)

;; Enable development mode
(setq gwl:*developing?* t)

;; Load systems
(ql:quickload :wire-world)
(ql:quickload :bus)
```

### 2. MCP API Development  
```python
# Test Gendl MCP service
result = mcp__gendl__gendl__lisp_eval(code="(+ 1 2 3)")

# Test Emacs MCP service
result = mcp__skewed_emacs__skewed_emacs__lisp_eval(code="(+ 1 2 3)")

# Test connectivity
gendl_status = mcp__gendl__gendl__ping_lisp()
emacs_status = mcp__skewed_emacs__skewed_emacs__ping_lisp()
```

### 3. Monitoring Claude Code Activity
With MCP services running, you can monitor what Claude Code or other agents are doing:

```bash
# Watch Emacs activities
docker logs -f skewed-emacs

# Watch Gendl activities  
docker logs -f gendl-ccl

# Or from within Emacs, watch the *Messages* buffer for API calls
```

## File Editing with MCP Services

### Key Capabilities via MCP

**1. Basic Emacs Lisp Evaluation:**
```python
# Simple arithmetic
mcp__skewed_emacs__skewed_emacs__lisp_eval(code="(+ 1 2 3)")
# Returns: {"Result": "6", "Stdout": ""}

# List operations
mcp__skewed_emacs__skewed_emacs__lisp_eval(code="(list 1 2 3)")
# Returns: {"Result": "(1 2 3)", "Stdout": ""}
```

**2. Buffer Operations:**
```python
# List all open buffers
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='(mapcar (lambda (buf) (buffer-name buf)) (buffer-list))'
)

# Get buffer contents
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='(with-current-buffer "*Messages*" (buffer-string))'
)

# Switch to a buffer
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='(switch-to-buffer "buffer-name")'
)
```

**3. File Operations:**
```python
# Read file contents
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='(with-temp-buffer (insert-file-contents "/path/to/file") (buffer-string))'
)

# Write to file
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='(with-temp-file "/path/to/file" (insert "content to write"))'
)

# Check if file exists
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='(file-exists-p "/path/to/file")'
)
```

**4. Directory Operations:**
```python
# List directory contents
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='(directory-files "/path/to/directory")'
)

# Get current directory
mcp__skewed_emacs__skewed_emacs__lisp_eval(code='(pwd)')
```

**5. Text Processing:**
```python
# Search and replace in buffer
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(with-current-buffer "buffer-name" 
               (goto-char (point-min)) 
               (while (search-forward "old" nil t) 
                 (replace-match "new")))'''
)

# Count lines in buffer
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='(with-current-buffer "buffer-name" (count-lines (point-min) (point-max)))'
)
```

## File Editing Best Practices for AI Agents

### Understanding Buffer Operations vs String Manipulation

**The Right Way: Buffer-Based Editing (Recommended)**
```python
# This is the efficient, Emacs-native approach
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

**The Inefficient Way: String Manipulation (Not Recommended)**
```python
# This approach wastes memory and CPU by duplicating Emacs's work
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

1. **Memory Efficiency**: Emacs manages buffers in optimized C code, rather than creating large Lisp strings
2. **Incremental Changes**: Make small, targeted edits instead of reprocessing entire files
3. **Native Features**: Access syntax highlighting, indentation, auto-formatting, and language-specific features
4. **Performance**: Buffer operations are optimized; string manipulation in Elisp is comparatively slow
5. **Undo/Redo**: Built-in change tracking and history
6. **Error Recovery**: Better error handling and state management

### Proper Buffer Editing Workflow

The key is following Emacs conventions properly:

```python
# Demonstrate correct buffer-based file editing via MCP
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(defun edit-file-properly (filepath search-term replacement)
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
                     (not (buffer-modified-p))))))'''
)
```

### Avoiding Interactive Prompts in Automated Operations

When using MCP services programmatically (especially via Claude or other AI agents), it's crucial to avoid operations that trigger interactive prompts.

**Safe File Editing Pattern for AI Agents:**
```python
mcp__skewed_emacs__skewed_emacs__lisp_eval(
    code='''(defun safe-edit-file-automated (filepath edit-function)
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
                   (setq revert-without-query original-revert-without-query))))'''
)
```

### Key Principles for AI Agents

1. **Always handle file conflicts gracefully** - Use `revert-without-query` or string manipulation
2. **Prefer deterministic operations** - Avoid functions that might prompt for user input
3. **Use `unwind-protect`** - Ensure cleanup happens even if operations fail
4. **Test buffer state** - Check if files are already open before operations
5. **Wrap complex operations** - Use helper functions like `safe-edit-file-automated`

## Network Architecture

```
Host Machine
├── MCP Services (via lisply-mcp wrapper)
│   ├── mcp__skewed-emacs__* → skewed-emacs:7080
│   └── mcp__gendl__* → gendl-ccl:9080
├── Port 4201 → gendl-ccl:4200 (Gendl Swank)
└── [Legacy HTTP ports available but deprecated]

Docker Network: emacs-gendl-network
├── skewed-emacs container
│   ├── Can reach gendl-ccl:4200 (Swank)
│   ├── Can reach gendl-ccl:9080 (HTTP API)
│   └── Exposes 7080 (HTTP API)
└── gendl-ccl container
    ├── Exposes 4200 (Swank)
    ├── Exposes 9080 (HTTP API)
    └── Can reach skewed-emacs:7080 (if needed)
```

## Commands Reference

### Container Management
```bash
# Create network (one-time)
docker network create emacs-gendl-network

# List running containers
docker ps

# Stop containers
docker stop skewed-emacs gendl-ccl

# Remove containers
docker rm skewed-emacs gendl-ccl

# Remove network
docker network rm emacs-gendl-network
```

### Development Commands
```bash
# Connect to Emacs
docker exec -it skewed-emacs emacsclient -t

# Connect to Gendl REPL directly
docker exec -it gendl-ccl ccl

# Check logs
docker logs skewed-emacs
docker logs gendl-ccl
```

### Testing Connectivity
```python
# Test MCP services
emacs_status = mcp__skewed_emacs__skewed_emacs__ping_lisp()
gendl_status = mcp__gendl__gendl__ping_lisp()

# Test basic operations
emacs_result = mcp__skewed_emacs__skewed_emacs__lisp_eval(code="(+ 1 2 3)")
gendl_result = mcp__gendl__gendl__lisp_eval(code="(+ 1 2 3)")
```

## Troubleshooting

### Common Issues

1. **MCP services not responding**:
   - Check if containers are running: `docker ps`
   - Verify MCP wrapper is configured correctly
   - Check container logs: `docker logs <container-name>`

2. **Containers not communicating**:
   - Ensure both containers are on the same network: `docker network ls`
   - Check container names: `docker ps --format "table {{.Names}}\t{{.Networks}}"`

3. **SLIME connection fails**:
   - Verify Swank is running: `docker exec gendl-ccl netstat -an | grep 4200`
   - Check network connectivity: `docker exec skewed-emacs telnet gendl-ccl 4200`

4. **Mount issues**:
   - Ensure `/home/dcooper8/projects` exists on host
   - Check permissions: `ls -la /home/dcooper8/projects`

### Reset Environment
```bash
# Stop and remove everything
docker stop skewed-emacs gendl-ccl 2>/dev/null || true
docker rm skewed-emacs gendl-ccl 2>/dev/null || true
docker network rm emacs-gendl-network 2>/dev/null || true

# Recreate from scratch
docker network create emacs-gendl-network
# Then restart containers with --network flag
```

## Integration with Claude Code

This environment is designed to work seamlessly with Claude Code:

1. **MCP Service Access**: Claude Code can use MCP services directly without HTTP calls
2. **File System Access**: Both containers mount `/home/dcooper8/projects` for shared file access
3. **Real-time Monitoring**: Use `docker exec` to connect to Emacs and watch Claude's activities
4. **Development Feedback Loop**: Edit files, test with Claude, see results in real-time

### Example Claude Code Workflow
```python
# Claude makes changes via MCP services
result = mcp__gendl__gendl__lisp_eval(code='(ql:quickload :my-project)')

# You can see the results in your SLIME session
# And use Update! links in Gendl web interface for live reloading
```

## Important Notes

### Security Considerations
- MCP services allow arbitrary Lisp evaluation
- Only use in trusted, containerized environments
- Do not expose services to untrusted networks

### Data Types
- All results are returned as strings (using `format "%s"`)
- Complex Lisp data structures maintain their textual representation
- Boolean values: `t` for true, `nil` for false

### Error Handling
- Syntax errors and runtime errors are caught and returned in result
- MCP services provide consistent error reporting

## Related Documentation

- **Gendl Development Guide**: `dot-files/emacs.d/etc/gendl/CLAUDE.md`
- **Main Gendl Guide**: `/projects/CLAUDE.md` (top-level project documentation)

## Version History

- **Initial Setup**: Basic container configuration
- **Network Integration**: Added Docker network for container communication
- **SLIME Integration**: Enabled seamless Emacs-to-Gendl SLIME connections
- **HTTP API Setup**: Both containers expose HTTP APIs for external tool integration
- **MCP Migration**: Transitioned from raw HTTP to MCP services for better integration


## AI Agent Development Lessons Learned

### String Escaping and Code Generation Best Practices

**Problem**: When AI agents generate Emacs Lisp code, over-escaping quotes can cause parsing errors.

**Incorrect (over-escaped):**
```elisp
(insert \"    [OK] example\\n\")
```

**Correct (clean Lisp):**
```elisp
(insert "    [OK] example\n")
```

**Key Lesson**: When evaluating Lisp directly via MCP, write actual Lisp code, not escaped strings. Only escape when writing code to files as string literals.

### Temporary Emacs Buffers for Code Sharing

**Best Practice**: Use temporary Emacs buffers to share code artifacts with users instead of external files.

```elisp
(with-current-buffer (get-buffer-create "*code-additions*")
  (erase-buffer)
  (insert ";; Clean code here")
  (emacs-lisp-mode)
  (goto-char (point-min))
  (switch-to-buffer "*code-additions*"))
```

**Benefits**:
- Integrated with user's workflow
- No file system clutter
- Syntax highlighting available
- Easy copy/paste workflow

### General Error Handling for Lisp Evaluation

**Robust Pattern for MCP Operations**:
```elisp
(defun safe-lisp-operation ()
  "Example of proper error handling in Lisp evaluation"
  (condition-case err
      (let ((result (potentially-failing-function)))
        (if result
            (format "Success: %s" result)
          "Operation completed but returned nil"))
    (error (format "Error: %s" (error-message-string err)))))
```

**Key Points**:
- Always wrap potentially failing operations in `condition-case`
- Provide meaningful error messages
- Handle both errors and nil results appropriately
- Use `error-message-string` for clean error reporting

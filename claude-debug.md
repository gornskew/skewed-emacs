# Claude Debug Session Summary - Emacs Backend Crash Investigation

## Problem Statement

The Emacs lisp-eval HTTP endpoint in the skewed-emacs Docker container crashes when file operations conflict with files open in the host Emacs. Instead of returning graceful error messages, the entire Emacs daemon process crashes with "socket hang up" errors.

## Root Cause Identified

**File locking conflicts cause Emacs daemon crashes** when:
1. A file is open in host Emacs 
2. Container Emacs tries to edit the same file via `find-file` + buffer operations + `save-buffer`
3. The conflict triggers a low-level Emacs process crash, not just a catchable error

## Key Findings

### Crash Reproduction Method
```elisp
;; This reliably crashes the container when docker-compose.yml is open in host Emacs
(progn
  (find-file "/projects/skewed-emacs/docker-compose.yml")
  (with-current-buffer "docker-compose.yml"
    (goto-char (point-min))
    (search-forward "emacs:")
    (end-of-line)
    (insert " # test comment")
    (save-buffer)))
```

### What Works vs What Crashes
â **Safe operations**: Read-only file access, `with-temp-buffer`, directory listings
â **Crash triggers**: `find-file` + buffer editing + `save-buffer` on locked files

### Technical Investigation Results
- **Error handling at eval level insufficient** - crashes occur below Elisp error handling
- **HTTP endpoint level protection needed** - entire servlet must be wrapped
- **Multiple protection layers required** - `condition-case-unless-debug`, `with-demoted-errors`, `unwind-protect`

## Solutions Attempted

### 1. Enhanced Safe Evaluation (Partially Successful)
Created `emacs-lisply-safe-eval` function with comprehensive error handling, but crashes still occurred at deeper levels.

### 2. Alternative File Operations (Working)
String-based file manipulation via Gendl backend proved successful:
```lisp
(let ((content (with-temp-buffer (insert-file-contents file) (buffer-string))))
  (setq fixed-content (substitute-string content old new))
  (with-temp-file file (insert fixed-content)))
```

## File Structure Context

### Key Files Modified
- `/projects/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/source/endpoints.el` - HTTP endpoint definitions
- `/projects/skewed-emacs/docker-compose.yml` - Fixed `.claude` mount path (`/home/node/.claude`)

### Current Status
- **Docker build**: Successfully rebuilding with syntax-corrected endpoints.el
- **Mount fix**: Changed `/home/emacs-user/.claude` â `/home/node/.claude` for proper Claude terminal access
- **Basic endpoints**: Working normally (ping, simple eval)
- **Crash protection**: Still needed at servlet level

## Next Steps Required

### Immediate Priority
1. **Implement servlet-level protection** in endpoints.el:
   ```elisp
   (defservlet* lisply/lisp-eval application/json ()
     (condition-case-unless-debug err
         (unwind-protect
             ;; existing servlet logic here
             )
       (error 
         ;; return graceful error JSON
         )))
   ```

2. **Test crash protection** with file conflict scenario

3. **Make changes permanent** in Docker build once proven stable

### Research-Based Solution Strategy
Based on Emacs documentation research:
- Use `condition-case-unless-debug` (allows debugging when needed)
- Combine with `with-demoted-errors` for message conversion
- Apply `unwind-protect` for cleanup guarantees
- Wrap entire servlet, not just eval operation

## Test Methodology

### Crash Test Setup
1. Open `/projects/skewed-emacs/docker-compose.yml` in host Emacs
2. Attempt file editing via container Emacs
3. **Expected result**: Graceful error message instead of daemon crash

### Verification Commands
```bash
# Check container status
docker ps | grep emacs

# Test basic functionality  
curl -s http://localhost:7081/lisply/ping-lisp

# Test error handling
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(+ 1 2 3)"}'
```

## Important Context

### Environment Details
- **MCP Architecture**: Claude Desktop â WSL script â Docker containers
- **Network**: `emacs-gendl-network` for inter-container communication
- **Ports**: Emacs (7081â7080), Gendl (9081â9080), Node (various)
- **Error Pattern**: "socket hang up" followed by "connect ECONNREFUSED"

### Container Restart Behavior
- **MCP-managed containers**: Require Claude Desktop restart to rebuild
- **docker-compose method**: Alternative for manual container management
- **Syntax errors**: Break container builds, require careful file editing

### File Editing Lessons
- **Avoid direct buffer operations** on potentially locked files
- **Use string manipulation** for safer file modifications
- **Test via isolated functions** before modifying permanent files

## Commands Reference

### Restart Sequence
```bash
# Full restart
pkill -f "Claude Desktop"
# Restart Claude Desktop application

# Alternative: docker-compose method
cd /projects/skewed-emacs
docker compose down && docker compose up -d
```

### Debug Commands
```elisp
;; Test safe evaluation
(emacs-lisply-safe-eval "(+ 1 2 3)")

;; Check backend status
(emacs-lisply-server-status)

;; Reload endpoints
(load "/projects/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/source/endpoints.el")
```

## Research Citations

Key insights from web search on Emacs error handling:
- **File locking**: Emacs creates `.#filename` symlinks for file locks
- **Error handling hierarchy**: `condition-case-unless-debug` > `with-demoted-errors` > `condition-case`
- **Daemon stability**: Known issue with concurrent file access in daemon mode
- **Protection patterns**: Multi-layer error handling recommended for robust services

## Success Metrics

**Goal**: Replace Emacs daemon crashes with graceful error responses like:
```json
{"success": false, "error": "File is locked by another process", "result": "", "stdout": ""}
```

**Current State**: Crashes produce MCP errors instead of application-level error handling.

**Completion Criteria**: 
1. File conflict scenarios return JSON error responses
2. Emacs daemon remains stable and responsive
3. Protection integrated into permanent Docker build

---

*Session Date: 2025-06-26*  
*Status: Ready for servlet-level crash protection implementation*
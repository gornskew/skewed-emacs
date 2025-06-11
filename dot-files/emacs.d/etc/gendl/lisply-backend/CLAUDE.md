# Lisply Backend Access Guide

## About This Documentation

This documentation covers access to the Gendl lisply-backend service across different environments. The core Lisp workflows documented here work universally - whether you're using direct HTTP access, MCP-enabled environments, or other interfaces to the backend.

## Related Documentation
- **Main Gendl Guide**: See `../../CLAUDE.md` for complete Gendl development workflow
- **Emacs Lisply Backend**: See `skewed-emacs` repository: `dot-files/emacs.d/sideloaded/lisply-backend/CLAUDE.md` for Emacs HTTP API
- Both Gendl (port 9081) and Emacs (port 7080) lisply services can run simultaneously

**For MCP-capable environments** (like Claude Desktop): Consider using the **lisply-mcp** package, which provides structured tool integration through the Model Context Protocol.

**For non-MCP environments** (like Claude Code): Use the direct HTTP access methods documented here.

## Core Lisp Workflows

*These Lisp expressions work in any environment that can evaluate Lisp against the backend*

### Bootstrap Development Environment

```lisp
;; Load Quicklisp package manager
(load-quicklisp)

;; Add your project directories (adjust path as needed)
(pushnew "~/projects/gendl/demos/" ql:*local-project-directories* :test #'equalp)

;; Enable development features for web applications
(setq gwl:*developing?* t)
```

### Load Demo Systems

```lisp
;; Load example systems (adjust names as needed)
(ql:quickload :wire-world)
(ql:quickload :bus)
```

### Working with Gendl Applications

**Note:** Package names, system names, and initialization function names vary by application. Common patterns:

```lisp
;; Systems are typically loaded with:
(ql:quickload :your-system-name)

;; Initialization functions may follow patterns like:
;; (your-package:initialize!)  - if such a function exists
;; (your-package:start)
;; (your-package:setup)

;; Check the source code or documentation for your specific system
```

### Development Workflow with Update! Links

When `gwl:*developing?*` is enabled, Gendl web applications include **Update!** and **SetSelf** development links:

```lisp
;; Enable development mode
(setq gwl:*developing?* t)

;; After making source changes, use Update! link on web page
;; OR if self is set, use:
(the update!)
```

**Iterative Development Process:**
1. Make changes to source files (`.lisp`, `.gendl`) 
2. Click **Update!** link on web page OR use `(the update!)` in REPL
3. Refresh page to see changes immediately
4. Use **SetSelf** to enable REPL commands like `(the update!)` for faster iteration

## Environment-Specific Usage

### MCP Environment (Claude Desktop, etc.)

**Advantages in MCP environments:**
- Execute Lisp expressions directly through the `lisp_eval` tool
- No need to construct JSON payloads or handle escaping
- Access to additional tools like `http_request` for testing endpoints
- Structured error handling and debugging support

**Usage:**
Simply use the `lisp_eval` tool with any of the Lisp expressions shown above. For example:
- Tool: `lisp_eval`
- Code: `(load-quicklisp)`

**Testing web applications:**
Use the `http_request` tool to test endpoints:
- Tool: `http_request` 
- Path: `/your-app-endpoint`
- Method: `GET`

### Direct HTTP Access (Claude Code, curl, etc.)

There is a Common Lisp service running on localhost that handles eval requests.

#### Testing the Service

```bash
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(+ 1 2 3)"}'
```

Expected response format:
```json
{"success":true,"result":"6","stdout":""}
```

**Note:** Do not include the `Content-Type: application/json` header - the service works without it and including it may cause issues with some tools.

**Important for Claude Code:** When using the Bash tool to call the service, the JSON response will be treated as an "error" by default. To properly capture the response, use output redirection:

```bash
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(+ 1 2 3)"}' 2>&1 | cat
```

#### Bootstrap Workflow via HTTP

1. **Load Quicklisp:**
   ```bash
   curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(load-quicklisp)"}' 2>&1 | cat
   ```

2. **Add local project directory:**
   ```bash
   curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(pushnew \"~/projects/gendl/demos/\" ql:*local-project-directories* :test #'\''equalp)"}' 2>&1 | cat
   ```

3. **Load demo systems:**
   ```bash
   curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(ql:quickload :wire-world)"}' 2>&1 | cat
   curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(ql:quickload :bus)"}' 2>&1 | cat
   ```

4. **Enable development mode:**
   ```bash
   curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(setq gwl:*developing?* t)"}' 2>&1 | cat
   ```

#### Testing Web Applications via HTTP

Use curl to test any page served by your Gendl application:
```bash
curl -s http://localhost:9081/your-app-endpoint | head -20
```

**Development Notes:**
- When testing static assets with `curl -I` (HEAD requests), responses may appear as errors even when assets are serving correctly
- Use `curl -s` for content requests to properly test page functionality
- Static assets may include: CSS, JavaScript, images, and other web resources
- The specific endpoints and structure depend on your particular Gendl application

## Universal Development Patterns

### Package and System Management

```lisp
;; Check current package
(package-name *package*)

;; Switch to a specific package
(in-package :gdl-user)

;; List loaded systems
(asdf:already-loaded-systems)

;; Check if a system is available
(asdf:find-system :your-system-name nil)
```

### Common Gendl Operations

```lisp
;; Create a basic object
(make-object 'box :width 10 :height 5 :length 3)

;; Create an object instance for interactive development
(make-self 'your-package:assembly)

;; Access object properties
(the width)  ; when object is current context
(the-object object-name width)  ; for specific object

;; Development and debugging
(describe-object *my-object*)
(apropos "some-symbol")
```

### Working with Source Files and Compilation

```lisp
;; Compile and load a modified source file
(load (compile-file "~/projects/gendl/demos/your-demo/source/assembly.lisp"))

;; After modifying source, create new instance to see changes
(make-self 'your-package:assembly)
```

### 3D Output Generation

```lisp
;; Generate X3D output for web visualization
(with-format (x3d *standard-output*) (write-the cad-output-tree))

;; Generate other formats (if available)
(with-format (vrml *standard-output*) (write-the cad-output-tree))
```

### Gendl Object Definition Patterns

```lisp
;; Basic object with individual instances
(define-object assembly (base-object)
  :objects
  ((my-box :type 'box
           :width 10
           :height 5
           :center (translate (the center) :right 5))
   
   ;; Object with computed parameters
   (my-sphere :type 'sphere
              :radius 3
              :display-controls (list :color :red)
              :center (translate (the center) :left 10))))

;; Using sequences for multiple objects
(define-object crowded-scene (base-object)
  :objects
  ((fence-posts :type 'cone
                :sequence (:size 8)
                :radius-1 0.5
                :length 2
                :center (translate (the center) 
                                   :right (* (the-child index) 3)
                                   :up (half (the-child length))))
   
   ;; Circular pattern with trigonometry
   (rock-circle :type 'sphere
                :sequence (:size 6)
                :radius (nth (the-child index) '(0.8 1.2 0.6 1.0 0.9 0.7))
                :display-controls (list :color (nth (the-child index)
                                                    '(:grey :brown :tan)))
                :center (translate (the center)
                                   :right (* (cos (* (the-child index) 
                                                     (/ (* 2 pi) 6))) 5)
                                   :rear (* (sin (* (the-child index) 
                                                    (/ (* 2 pi) 6))) 5)))))
```

### Error Handling and Debugging

```lisp
;; Check for errors in a safe way
(ignore-errors (your-potentially-failing-operation))

;; Get stack trace (implementation specific)
#+sbcl (sb-debug:print-backtrace)
#+ccl (ccl:print-call-history)

;; Return to top level from debugger
:abort  ; or :a in most implementations
```

## Note on Initialization Functions

**Important:** Not all Gendl systems have initialization functions, and those that do may not follow a standard naming convention. Common patterns include:

- `(package-name:initialize!)`
- `(package-name:start)`
- `(package-name:setup)`
- Or custom function names specific to the application

Check the source code or documentation of your specific system to determine the correct initialization approach. Many systems work immediately after loading with `ql:quickload` without requiring additional initialization.

## Service Information

The Gendl backend typically runs with these default endpoints:
- **HTTP**: `http://localhost:9081/`
- **Lisp Evaluation**: `http://localhost:9081/lisply/lisp-eval`
- **Ping**: `http://localhost:9081/lisply/ping-lisp`

### Tested Working Examples (Claude Code)
```bash
# Test connection
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(+ 1 2 3)"}' 2>&1 | cat

# Setup development environment
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(load-quicklisp)"}' 2>&1 | cat
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d "{\"code\": \"(pushnew \\\"/path/to/your/projects/\\\" ql:*local-project-directories* :test #'equalp)\"}" 2>&1 | cat
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(setq gwl:*developing?* t)"}' 2>&1 | cat

# Load systems
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(ql:quickload :modern-site)"}' 2>&1 | cat
```

For MCP environments, these details are handled automatically by the lisply-mcp middleware.
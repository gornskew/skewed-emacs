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
- **Mount**: `~/projects` → `/projects`

### Gendl Container (`gendl-ccl`)  
- **Base**: `genworks/gendl:devo-ccl`
- **Network Name**: `gendl-ccl` (accessible as `gendl-ccl:4200` and `gendl-ccl:9080` from other containers)
- **Host Ports**:
  - `4201` → `4200` (Swank/SLIME)
  - `9081` → `9080` (HTTP API - deprecated in favor of MCP)
- **MCP Service**: Available via `mcp__gendl__*` functions
- **Mount**: `~/projects` → `/home/gendl-user/projects`

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

## MCP API Usage

For detailed MCP API documentation, examples, and best practices, see:
**`dot-files/emacs.d/sideloaded/lisply-backend/CLAUDE.md`**

### Quick MCP API Reference
```python
# Basic evaluation
mcp__skewed_emacs__skewed_emacs__lisp_eval(code="(+ 1 2 3)")

# File editing (see lisply-backend docs for detailed patterns)
mcp__skewed_emacs__skewed_emacs__lisp_eval(code='(find-file "/path/to/file.lisp")')

# Test connectivity
mcp__skewed_emacs__skewed_emacs__ping_lisp()
```

**⚠️ CRITICAL WARNING**: When using MCP file editing, you share the **global current buffer** with the user. Always use `with-current-buffer` patterns to avoid conflicts. See lisply-backend documentation for safe practices.

## File Editing via MCP

**For comprehensive file editing documentation, see:**
**`dot-files/emacs.d/sideloaded/lisply-backend/CLAUDE.md`**

That documentation includes:
- Detailed MCP API patterns
- Paredit mode instructions for Lisp editing
- Safe buffer operations vs string manipulation
- Interactive prompt avoidance
- Error recovery patterns
- **Shared buffer footgun warnings**

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
   - Ensure `~/projects` exists on host
   - Check permissions: `ls -la ~/projects`

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
2. **File System Access**: Both containers mount `~/projects` for shared file access
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


## Development Lessons Learned

**For detailed lessons learned including:**
- **Shared current buffer footgun and solutions**
- String escaping and code generation best practices
- Interactive prompt handling
- Successful MCP file editing workflows
- Paredit mode workflows
- Error recovery patterns

**See: `dot-files/emacs.d/sideloaded/lisply-backend/CLAUDE.md`**

### Key Takeaway: Shared Buffer State
When using MCP file editing, remember that you **share the global current buffer** with the user. This was discovered during viewport menu development when buffer switching conflicts occurred. Always use explicit buffer targeting with `with-current-buffer` patterns.

### Quick Reference: Safe Patterns
```elisp
;; BAD: Relies on global current buffer
(search-forward "target")

;; GOOD: Explicit buffer targeting
(with-current-buffer "specific-file.lisp"
  (search-forward "target"))
```


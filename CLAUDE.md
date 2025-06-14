# Skewed Emacs + Gendl Docker Development Environment

This file provides guidance to Claude Code (claude.ai/code) when working with the integrated Skewed Emacs and Gendl development environment.

## Overview

This setup provides a complete Lisp development environment with:
- **Skewed Emacs Container**: Custom Emacs configuration with HTTP API on port 7081
- **Gendl Container**: 3D CAD/modeling system with Common Lisp REPL and HTTP API on port 9081
- **Network Integration**: Containers communicate via Docker network for SLIME connections
- **Dual HTTP APIs**: Both containers expose lisply-backend APIs for external tool integration

## â ï¸  TEMPORARY WORKAROUND - PATH/Environment Corruption Issue

**STATUS**: Active workaround in place, root cause unresolved
**DELETE THIS SECTION** once the underlying cause is fixed

### Current Issue

The container startup process corrupts the environment, causing:
1. **PATH Duplication**: `/usr/bin` appears multiple times in PATH
2. **Missing SHELL**: `SHELL` environment variable not set
3. **Native Compilation Failures**: `as` assembler not found during .eln generation
4. **Inconsistent `shell-file-name`**: Defaults to `/bin/sh` instead of `/bin/bash`

### Root Cause Analysis

**Architecture Issue**: The container startup script (`docker/startup.sh`) uses:
```bash
emacs --daemon --no-init-file --load /tmp/emacs-daemon-startup.el
```

This means:
- `init.el` is **never loaded** in the daemon (bypassed with `--no-init-file`)
- Environment fixes in `init.el` are **ineffective**
- Custom daemon config in `/tmp/emacs-daemon-startup.el` handles environment
- **True root cause**: Unknown upstream source of environment corruption

### Current Workarounds (Applied)

**1. Startup Script Fix** (`docker/startup.sh`):
```elisp
;; ENVIRONMENT FIX: Always ensure proper container environment
(let ((path-corrupted (or (not (getenv "PATH"))
                          (string-match-p "not found" (or (getenv "PATH") ""))))
  ;; Fix PATH if corrupted + Always fix SHELL
  (setenv "SHELL" "/bin/bash")
  (setq shell-file-name "/bin/bash")
  ;; ... process-environment updates ...)
```

**2. Init.el Defensive Fix** (`dot-files/emacs.d/init.el`):
```elisp
;; Aggressive native compilation disabling
(setq native-comp-jit-compilation nil
      package-native-compile nil
      ;; ... environment PATH/SHELL fixes ...)
```

### Verification Commands

Check if workarounds are active:
```bash
# Test current environment
docker exec skewed-emacs emacsclient --eval '(list (getenv "SHELL") shell-file-name (getenv "PATH"))'

# Test native compilation settings
docker exec skewed-emacs emacsclient --eval 'native-comp-jit-compilation'

# Test assembler accessibility
docker exec skewed-emacs emacsclient --eval '(shell-command-to-string "which as")'
```

### Current State (After Restart)

- â **Environment**: `SHELL=/bin/bash`, `shell-file-name=/bin/bash`
- â **PATH**: Contains `/usr/bin` (with duplicates - harmless)
- â **Native Compilation**: Properly disabled (`nil`/`-1`)
- â **Toolchain**: `as` and `gcc` accessible
- â **Package Installation**: Magit installs without compilation errors
- â **Root Cause**: Still unknown - workarounds mask the issue

### TODO: Find True Root Cause

Investigation needed:
1. **Docker Layer Analysis**: Check if base image `debian:testing-slim` has issues
2. **Build Process**: Examine if `apt-get install` or user setup corrupts environment
3. **Container Runtime**: Check if Docker/runtime environment affects startup
4. **Emacs Bootstrap**: Investigate if `bootstrap-emacs.sh` side effects persist

**Expected Fix**: Once root cause found, remove both workarounds and restart should work cleanly.

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

# Verify both services are running
curl -s http://localhost:7081/lisply/ping-lisp  # Should return "pong"
curl -s http://localhost:9081/lisply/ping-lisp  # Should return "pong"
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
  - `7081` → `7080` (HTTP API)
- **HTTP API**: `http://localhost:7081/lisply/lisp-eval`
- **Mount**: `/home/dcooper8/projects` → `/projects`

### Gendl Container (`gendl-ccl`)  
- **Base**: `genworks/gendl:devo-ccl`
- **Network Name**: `gendl-ccl` (accessible as `gendl-ccl:4200` and `gendl-ccl:9080` from other containers)
- **Host Ports**:
  - `4201` → `4200` (Swank/SLIME)
  - `9081` → `9080` (HTTP API)
- **HTTP API**: `http://localhost:9081/lisply/lisp-eval`
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

### 2. HTTP API Development  
```bash
# Test Gendl HTTP API from host
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(+ 1 2 3)"}' 2>&1 | cat

# Test Emacs HTTP API from host
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(+ 1 2 3)"}' 2>&1 | cat

# Test inter-container communication
docker exec skewed-emacs curl -s http://gendl-ccl:9080/lisply/ping-lisp
```

### 3. Monitoring Claude Code Activity
With both HTTP APIs running, you can monitor what Claude Code or other agents are doing:

```bash
# Watch Emacs activities
docker logs -f skewed-emacs

# Watch Gendl activities  
docker logs -f gendl-ccl

# Or from within Emacs, watch the *Messages* buffer for API calls
```

## Network Architecture

```
Host Machine
├── Port 7081 → skewed-emacs:7080 (Emacs HTTP API)
├── Port 9081 → gendl-ccl:9080 (Gendl HTTP API)  
└── Port 4201 → gendl-ccl:4200 (Gendl Swank)

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
```bash
# Test HTTP APIs
curl -s http://localhost:7081/lisply/ping-lisp
curl -s http://localhost:9081/lisply/ping-lisp

# Test inter-container communication
docker exec skewed-emacs curl -s http://gendl-ccl:9080/lisply/ping-lisp
docker exec skewed-emacs telnet gendl-ccl 4200 <<< "quit"
```

## Troubleshooting

### Common Issues

1. **Containers not communicating**:
   - Ensure both containers are on the same network: `docker network ls`
   - Check container names: `docker ps --format "table {{.Names}}\t{{.Networks}}"`

2. **SLIME connection fails**:
   - Verify Swank is running: `docker exec gendl-ccl netstat -an | grep 4200`
   - Check network connectivity: `docker exec skewed-emacs telnet gendl-ccl 4200`

3. **HTTP APIs not responding**:
   - Check container logs: `docker logs <container-name>`
   - Verify port forwarding: `docker port <container-name>`

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

1. **HTTP API Access**: Claude Code can make HTTP calls to both Emacs and Gendl APIs
2. **File System Access**: Both containers mount `/home/dcooper8/projects` for shared file access
3. **Real-time Monitoring**: Use `docker exec` to connect to Emacs and watch Claude's activities
4. **Development Feedback Loop**: Edit files, test with Claude, see results in real-time

### Example Claude Code Workflow
```bash
# Claude makes changes via HTTP API
curl -X POST http://localhost:9081/lisply/lisp-eval -d '{"code": "(ql:quickload :my-project)"}'

# You can see the results in your SLIME session
# And use Update! links in Gendl web interface for live reloading
```

## Related Documentation

- **Gendl Development Guide**: `dot-files/emacs.d/etc/gendl/CLAUDE.md`
- **Gendl Lisply Backend**: `dot-files/emacs.d/etc/gendl/lisply-backend/CLAUDE.md`  
- **Emacs Lisply Backend**: `dot-files/emacs.d/sideloaded/lisply-backend/CLAUDE.md`

## Version History

- **Initial Setup**: Basic container configuration
- **Network Integration**: Added Docker network for container communication
- **SLIME Integration**: Enabled seamless Emacs-to-Gendl SLIME connections
- **Dual API Setup**: Both containers expose HTTP APIs for external tool integration
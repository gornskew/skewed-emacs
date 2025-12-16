# Services Discovery Migration

## Overview

Migrated dashboard service discovery from `docker ps` (requires docker.sock) to `services.json` single source of truth.

## Changes Made

### 1. Created `services-discovery.el`

**Location:** `dot-files/emacs.d/etc/services-discovery.el`

**Purpose:** Parse `services.json` and provide service information to Emacs

**Key Functions:**
- `skewed-get-services()` - Returns all services
- `skewed-get-lisply-backends()` - Returns emacs-lisp and common-lisp services
- `skewed-get-swank-services()` - Returns services with SWANK ports
- `skewed-reload-services()` - Force reload from services.json

### 2. Updated `dashboard-additions.el`

**Changes:**
- Added `(require 'services-discovery)` at top
- Replaced `discover-network-lisply-backends()` to use `skewed-get-lisply-backends()`
- Replaced `discover-swank-services()` to use `skewed-get-swank-services()`
- Removed `discover-docker-services()` (no longer needed)
- Updated fallbacks to use container network names instead of localhost

### 3. Services.json Structure

**Location:** `/projects/skewed-emacs/services.json`

**Services Defined:**
- `skewed-emacs` - Emacs Lisp backend (port 7080)
- `gendl-ccl` - CCL Common Lisp with SWANK (ports 9080, 4200)
- `gendl-sbcl` - SBCL Common Lisp with SWANK (ports 9090, 4210)
- `lisply-mcp` - Node.js middleware with SSH

### 4. Benefits

**Before:**
- Required docker.sock mount
- Used `docker ps` command
- Hardcoded fallbacks to localhost
- Security risk with docker socket access

**After:**
- No docker.sock required ✓
- Pure container-to-container networking ✓
- Services defined in single source of truth ✓
- Secure by default ✓

## Testing

```elisp
;; Test services discovery
(skewed-get-lisply-backends)
;; => ((:name "skewed-emacs" :port 7080 ...) 
;;     (:name "gendl-ccl" :port 9080 ...) 
;;     (:name "gendl-sbcl" :port 9090 ...))

(skewed-get-swank-services)
;; => ((:name "gendl-ccl" :port 4200 ...) 
;;     (:name "gendl-sbcl" :port 4210 ...))

;; Test dashboard
(dashboard-refresh-buffer)
```

## Container Build

Files are automatically included in Docker build via:
```dockerfile
COPY . /home/emacs-user/skewed-emacs/
```

After rebuild, dashboard will load from built-in config at:
- `/home/emacs-user/.emacs.d/etc/services-discovery.el`
- `/home/emacs-user/skewed-emacs/services.json`

## Next Steps

1. ✓ Services discovery migrated to services.json
2. ✓ Docker.sock dependency removed
3. → Rebuild containers to bake in new configuration
4. → Test dashboard on fresh container start
5. → Proceed with gotty integration

## Files Modified

- `dot-files/emacs.d/etc/services-discovery.el` (NEW)
- `dot-files/emacs.d/etc/dashboard-additions.el` (MODIFIED)
- `services.json` (EXISTING - now used by Emacs)

Date: 2025-12-16

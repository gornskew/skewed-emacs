#!/bin/bash
# Startup script - Start Emacs daemon with HTTP backend, then exec bash

set -e

# Create daemon configuration
cat > /tmp/emacs-daemon-startup.el << 'EOF'
;; Clean daemon startup - loads lisply backend
(setq debug-on-error t)

;; Disable native compilation
(setq native-comp-speed -1)
(setq package-native-compile nil)
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation nil)

;; Ensure git is in PATH
(setenv "PATH" (concat "/usr/bin:" (getenv "PATH")))

(message "=== Starting Clean Emacs Daemon ===")

;; Initialize package system
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Load simple-httpd
(require 'simple-httpd)

;; Set httpd-host to 0.0.0.0 for Docker port mapping
(setq httpd-host "0.0.0.0")

;; Load lisply backend components
(let ((backend-dir "/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/source/"))
  (load-file (concat backend-dir "http-setup.el"))
  (load-file (concat backend-dir "endpoints.el"))
  (load-file (concat backend-dir "backend.el")))

;; Start the HTTP server
(emacs-lisply-start-server)
(message "✓ Lisply HTTP server started on port 7080")

;; Start default Emacs server (Unix socket)
(server-start)
(message "✓ Emacs server started (Unix socket)")

(message "✓ Clean Emacs daemon ready!")
(message "   - HTTP API: port 7080")
(message "   - Projects mounted at: /projects")
(message "   - emacsclient available via: emacsclient")
EOF

# Start Emacs daemon in background
echo "Starting Emacs daemon with HTTP server..."
emacs --daemon --no-init-file --load /tmp/emacs-daemon-startup.el > /tmp/emacs-daemon.log 2>&1 &
EMACS_PID=$!

# Wait for daemon to start
echo "Waiting for daemon to start..."
for i in {1..30}; do
    if emacsclient --eval "t" > /dev/null 2>&1; then
        echo "✓ Emacs daemon ready"
        break
    elif [ $i -eq 30 ]; then
        echo "✗ Daemon failed to start"
        cat /tmp/emacs-daemon.log
        exit 1
    else
        sleep 1
    fi
done

# Show status and start REPL
echo ""
echo "=== Skewed Emacs Container Ready ==="
echo "✓ Emacs daemon running (PID: $EMACS_PID)"
echo "✓ HTTP API available on port 7080"
echo "✓ Projects directory: /projects"
echo ""
echo "Usage:"
echo "  # Test HTTP API"
echo "  curl http://localhost:7080/lisply/ping-lisp"
echo ""
echo "  # Use emacsclient"
echo "  emacsclient --eval '(+ 1 2 3)'"
echo ""
echo "  # View daemon logs"
echo "  cat /tmp/emacs-daemon.log"
echo ""

# Start the Emacs REPL as the main process
exec /home/emacs-user/emacs-repl.sh

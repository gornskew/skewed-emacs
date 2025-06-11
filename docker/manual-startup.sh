#!/bin/bash
# Manual startup script - just get Emacs daemon running first

set -e

echo "=== Manual Emacs Daemon Startup ==="

# Create minimal daemon startup - NO HTTP server yet
cat > /tmp/minimal-daemon.el << 'EOF'
;; Minimal daemon startup - just get basic Emacs running
(setq debug-on-error t)

;; Disable native compilation completely
(setq native-comp-speed -1)
(setq package-native-compile nil)
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation nil)

(message "Starting minimal Emacs daemon...")

;; Initialize package system
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Start TCP server for emacsclient
(setq server-use-tcp t)
(setq server-port 9999)
(setq server-host "0.0.0.0")
(server-start)

(message "Minimal daemon ready - TCP server on port 9999")
EOF

echo "Starting minimal Emacs daemon..."
emacs --daemon --no-init-file --load /tmp/minimal-daemon.el &

# Wait for daemon
echo "Waiting for daemon..."
for i in {1..10}; do
    if emacsclient --eval "t" > /dev/null 2>&1; then
        echo "â Emacs daemon ready after $i seconds"
        break
    elif [ $i -eq 10 ]; then
        echo "â Daemon failed to start"
        exit 1
    else
        echo "Waiting... ($i/10)"
        sleep 1
    fi
done

echo ""
echo "â Emacs daemon running"
echo "â TCP server on port 9999"
echo ""
echo "Test with: emacsclient --eval '(+ 1 2 3)'"
echo "Or connect interactively: emacsclient -t"
echo ""
echo "To load HTTP components manually:"
echo "  emacsclient --eval '(require \"simple-httpd\")' "
echo "  emacsclient --eval '(load-file \"/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/source/http-setup.el\")' "
echo ""

# Keep container alive
echo "Container will stay alive. Use docker exec to connect."
while true; do
    if ! emacsclient --eval "t" > /dev/null 2>&1; then
        echo "Daemon died, exiting..."
        exit 1
    fi
    sleep 60
done

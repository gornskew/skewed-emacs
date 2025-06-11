#!/bin/bash
# Consolidated startup script - Clean Emacs Lisp REPL with HTTP backend

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

;; Start TCP server for emacsclient (internal use only)
(setq server-use-tcp t)
(setq server-port 9999)
(setq server-host "127.0.0.1")
(setq server-auth-dir "/home/emacs-user/.emacs.d/server")
(make-directory server-auth-dir t)
(set-file-modes server-auth-dir #o700)
(server-start)

(message "✓ Clean Emacs daemon ready!")
(message "   - HTTP API: port 7080")
(message "   - Projects mounted at: /projects")
EOF

# Start Emacs daemon in background
echo "Starting Emacs daemon..."
emacs --daemon --no-init-file --load /tmp/emacs-daemon-startup.el > /tmp/emacs-daemon.log 2>&1 &
EMACS_PID=$!
echo "Emacs daemon PID: $EMACS_PID"

# Wait for daemon to start
echo "Waiting for daemon to start..."
for i in {1..30}; do
    echo "Daemon check $i/30..."
    if emacsclient --server-file=/home/emacs-user/.emacs.d/server/127.0.0.1:9999 --eval "t" > /dev/null 2>&1; then
        echo "✓ Daemon responding after $i seconds"
        break
    elif [ $i -eq 30 ]; then
        echo "✗ Daemon failed to start after 30 seconds"
        echo "=== Daemon startup log ==="
        cat /tmp/emacs-daemon.log
        exit 1
    else
        sleep 1
    fi
done

# Check if running interactively
if [ -t 0 ]; then
    echo "=== Emacs Lisp REPL ==="
    echo "Projects directory: /projects"
    echo "HTTP API: port 7080"
    echo ""
    echo "Enter Lisp expressions:"
    echo "Example: (+ 1 2 3)"
    echo "Type 'quit' to exit."
    echo ""
    
    # Interactive REPL loop
    while IFS= read -r line; do
        if [[ -z "$line" ]]; then
            continue
        fi
        
        case "$line" in
            "(quit)" | "(exit)" | "exit" | "quit")
                echo "Goodbye!"
                break
                ;;
            *)
                result=$(emacsclient --server-file=/home/emacs-user/.emacs.d/server/127.0.0.1:9999 --eval "$line" 2>/dev/null)
                echo "$result"
                ;;
        esac
    done
else
    # Daemon mode - keep container alive
    echo "Daemon mode - HTTP API available on port 7080"
    while true; do
        if ! emacsclient --server-file=/home/emacs-user/.emacs.d/server/127.0.0.1:9999 --eval "t" > /dev/null 2>&1; then
            echo "✗ Daemon stopped"
            exit 1
        fi
        sleep 60
    done
fi
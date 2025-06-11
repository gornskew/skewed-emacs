#!/bin/bash
# startup.sh - Start Emacs daemon and present REPL on stdio (following Gendl pattern)

set -e

echo "Starting Emacs daemon with lisply backend..."

# Create minimal startup script for Emacs daemon
cat > /tmp/emacs-daemon-startup.el << 'EOF'
;; Minimal daemon startup configuration - NO init.el loading
(setq debug-on-error nil)

;; Completely disable native compilation
(setq native-comp-speed -1)
(setq package-native-compile nil)
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation nil)

;; Ensure git is in PATH
(setenv "PATH" (concat "/usr/bin:" (getenv "PATH")))

;; Initialize package system minimally
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Load simple-httpd
(message "Loading simple-httpd...")
(condition-case err
    (progn
      (require 'simple-httpd)
      (message "â simple-httpd loaded"))
  (error 
    (message "Installing simple-httpd...")
    (package-refresh-contents)
    (package-install 'simple-httpd)
    (require 'simple-httpd)))

;; Load lisply backend
(message "Loading lisply backend...")
(condition-case err
    (progn
      (load-file "/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/source/http-setup.el")
      (load-file "/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/source/endpoints.el")
      (load-file "/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/source/backend.el")
      (emacs-lisply-start-server)
      (message "â Lisply backend started on port 7080"))
  (error 
    (message "â Lisply backend failed: %s" err)))

;; Start TCP server for emacsclient
(setq server-use-tcp t)
(setq server-port 9999)
(setq server-host "0.0.0.0")
(condition-case err
    (progn
      (server-start)
      (message "â Emacs server started on port 9999"))
  (error
    (message "â ï¸  Server start issue: %s" err)))

(message "ð Emacs daemon ready!")
EOF

# Start Emacs daemon in background
echo "Starting Emacs daemon..."
emacs --daemon --no-init-file --load /tmp/emacs-daemon-startup.el &

# Wait for daemon to start
sleep 5

# Verify daemon is running
if ! emacsclient --eval "t" > /dev/null 2>&1; then
    echo "â Emacs daemon failed to start"
    exit 1
fi

echo "â Emacs daemon started successfully"
echo "â Lisply HTTP API available on port 7080"
echo "â Emacs TCP server available on port 9999"
echo ""

# Check if running interactively (has stdin)
if [ -t 0 ]; then
    echo "Emacs Lisp REPL ready. Enter expressions to evaluate:"
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
            "(ping)")
                echo "pong"
                continue
                ;;
            *)
                # Evaluate using emacsclient
                result=$(emacsclient --eval "$line" 2>&1)
                echo "$result"
                ;;
        esac
    done
    echo "REPL session ended."
else
    echo "Running in daemon mode. Services are available via HTTP and TCP."
    echo "For interactive REPL, run: docker exec -it <container> /bin/bash"
    echo "Then use: emacsclient --eval '<expression>'"
    echo ""
    
    # Keep container alive by monitoring the daemon
    while true; do
        if ! emacsclient --eval "t" > /dev/null 2>&1; then
            echo "â Emacs daemon stopped, exiting..."
            exit 1
        fi
        sleep 60
        echo "$(date): Services running (HTTP: 7080, TCP: 9999)"
    done
fi

#!/bin/bash
# startup.sh - Fixed baseline startup with proper HTTP server binding

set -e

echo "=== Starting Emacs daemon with lisply backend ==="

# Create startup script with the fixes we discovered
cat > /tmp/emacs-daemon-startup.el << 'EOF'
;; Fixed daemon startup configuration - loads lisply backend with proper binding
(setq debug-on-error t)

;; Completely disable native compilation
(setq native-comp-speed -1)
(setq package-native-compile nil)
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation nil)

;; Ensure git is in PATH
(setenv "PATH" (concat "/usr/bin:" (getenv "PATH")))

(message "=== Fixed Emacs daemon startup initiated ===")

;; Initialize package system
(message "Initializing package system...")
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
    (message "â simple-httpd failed: %s" err)
    (message "Installing simple-httpd...")
    (package-refresh-contents)
    (package-install 'simple-httpd)
    (require 'simple-httpd)))

;; CRITICAL FIX: Set httpd-host to 0.0.0.0 for Docker port mapping
(setq httpd-host "0.0.0.0")
(message "â Set httpd-host to 0.0.0.0 for Docker compatibility")

;; Load lisply backend components
(message "Loading lisply backend components...")
(let ((backend-dir "/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/source/"))
  (condition-case err
      (progn
        (message "Loading http-setup.el...")
        (load-file (concat backend-dir "http-setup.el"))
        (message "Loading endpoints.el...")
        (load-file (concat backend-dir "endpoints.el"))
        (message "Loading backend.el...")
        (load-file (concat backend-dir "backend.el"))
        (message "â All lisply backend files loaded"))
    (error 
      (message "â Lisply backend loading failed: %s" err)
      (kill-emacs 1))))

;; Start the HTTP server
(message "Starting lisply HTTP server...")
(condition-case err
    (progn
      (emacs-lisply-start-server)
      (message "â Lisply backend started on port 7080 (binding to 0.0.0.0)")
      
      ;; Verify server functions exist
      (if (and (fboundp 'emacs-lisply-start-server)
               (fboundp 'emacs-lisply-stop-server))
          (message "â Server functions verified")
        (progn
          (message "â Server functions not available")
          (kill-emacs 1))))
  (error 
    (message "â Lisply server start failed: %s" err)
    (kill-emacs 1)))

;; Start TCP server for emacsclient
(message "Starting Emacs TCP server...")
(setq server-use-tcp t)
(setq server-port 9999)
(setq server-host "0.0.0.0")
(condition-case err
    (progn
      (server-start)
      (message "â Emacs server started on port 9999"))
  (error
    (message "â ï¸ Server start issue (non-fatal): %s" err)))

(message "ð Emacs daemon ready!")
(message "   - HTTP API: port 7080 (bound to 0.0.0.0)")
(message "   - TCP server: port 9999")

;; Test the HTTP server internally
(message "Testing HTTP server internally...")
(condition-case err
    (progn
      ;; Verify the ping function works
      (if (fboundp 'emacs-lisply-ping)
          (progn
            (message "â Ping function available")
            (message "Internal ping result: %s" (emacs-lisply-ping)))
        (message "â ï¸ Ping function not found")))
  (error
    (message "â ï¸ Internal ping test failed: %s" err)))

EOF

# Start Emacs daemon with improved logging
echo "Starting Emacs daemon..."
emacs --daemon --no-init-file --load /tmp/emacs-daemon-startup.el > /tmp/emacs-daemon.log 2>&1 &
EMACS_PID=$!

# Wait for daemon to start with better feedback
echo "Waiting for daemon startup..."
for i in {1..30}; do
    if emacsclient --eval "t" > /dev/null 2>&1; then
        echo "â Emacs daemon responding after ${i} seconds"
        break
    elif [ $i -eq 30 ]; then
        echo "â Emacs daemon failed to respond after 30 seconds"
        echo "=== Daemon startup log ==="
        cat /tmp/emacs-daemon.log
        exit 1
    else
        echo "Waiting... (${i}/30)"
        sleep 1
    fi
done

echo ""
echo "=== Daemon startup log ==="
cat /tmp/emacs-daemon.log

echo ""
echo "â Emacs daemon started successfully"
echo "â Lisply HTTP API available on port 7080 (bound to 0.0.0.0)"
echo "â Emacs TCP server available on port 9999"

# Test the HTTP server from inside the container
echo ""
echo "=== Internal HTTP server test ==="
if command -v curl >/dev/null 2>&1; then
    echo "Testing ping endpoint internally..."
    if curl -f -s http://localhost:7080/lisply/ping-lisp; then
        echo ""
        echo "â Internal ping test successful"
    else
        echo ""
        echo "â Internal ping test failed"
    fi
    
    echo "Testing eval endpoint internally..."
    EVAL_RESULT=$(curl -s -H "Content-Type: application/json" -d '{"code": "(+ 1 2 3)"}' http://localhost:7080/lisply/lisp-eval)
    if echo "$EVAL_RESULT" | grep -q '"success":true'; then
        echo "â Internal eval test successful: $EVAL_RESULT"
    else
        echo "â Internal eval test failed: $EVAL_RESULT"
    fi
else
    echo "curl not available for internal testing"
fi

# Check if running interactively (has stdin)
if [ -t 0 ]; then
    echo ""
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
    echo ""
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
        
        # Log health check with HTTP test
        echo "$(date): Services running (HTTP: 7080, TCP: 9999)"
        
        # Test HTTP endpoint periodically
        if command -v curl >/dev/null 2>&1; then
            if curl -f -s http://localhost:7080/lisply/ping-lisp > /dev/null; then
                echo "$(date): HTTP endpoint responding"
            else
                echo "$(date): â ï¸ HTTP endpoint not responding"
            fi
        fi
        
        sleep 60
    done
fi
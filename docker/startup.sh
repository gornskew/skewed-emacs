#!/bin/bash
# Unified startup.sh - Used for both build time (--batch) and runtime
# Contains all PATH/SHELL fixes in one place

set -e

# Check for batch mode (build time)
BATCH_MODE=false
if [ "$1" = "--batch" ]; then
    BATCH_MODE=true
    echo "=== Build-Time Package Installation ==="
    echo "Running emacs once to install packages via init.el..."
else
    echo "=== Skewed Emacs Container Startup ==="
fi

# Runtime configuration (ignored in batch mode)
HTTP_PORT=${HTTP_PORT:-7080}
START_HTTP=${START_HTTP:-true}
START_SWANK=${START_SWANK:-false}
SWANK_PORT=${SWANK_PORT:-4200}

if [ "$BATCH_MODE" = "false" ]; then
    echo "HTTP_PORT: $HTTP_PORT"
    echo "START_HTTP: $START_HTTP"  
    echo "START_SWANK: $START_SWANK"
    echo "SWANK_PORT: $SWANK_PORT"
fi

# Create emacs startup script with PATH/SHELL fixes
# This script works for both batch and daemon modes
cat > /tmp/emacs-startup.el << EOF
;; Unified emacs startup - handles both build and runtime
(setq debug-on-error t)

;; ENVIRONMENT FIX: Single location for PATH/SHELL corruption fixes
;; This ensures clean environment for both build and runtime
(let ((path-corrupted (or (not (getenv "PATH"))
                          (string-match-p "not found" (or (getenv "PATH") "")))))
  
  ;; Fix PATH if corrupted
  (when path-corrupted
    (setenv "PATH" "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")
    (setq exec-path '("/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/bin")))
  
  ;; Always ensure SHELL is properly set
  (setenv "SHELL" "/bin/bash")
  (setq shell-file-name "/bin/bash")
  
  ;; Update process-environment
  (setq process-environment
        (cons (concat "PATH=" (getenv "PATH"))
              (cons "SHELL=/bin/bash"
                    (seq-remove (lambda (env)
                                  (or (string-prefix-p "PATH=" env)
                                      (string-prefix-p "SHELL=" env)))
                                process-environment))))
  
  ;; Ensure /usr/bin is in exec-path
  (unless (member "/usr/bin" exec-path)
    (add-to-list 'exec-path "/usr/bin"))
  (setenv "PATH" (concat "/usr/bin:" (getenv "PATH"))))

(if (getenv "EMACS_BATCH_MODE")
    (message "=== Build Time: Loading init.el for package installation ===")
  (message "=== Runtime: Loading init.el with pre-installed packages ==="))

(message "Environment status:")
(message "  SHELL: %s" (getenv "SHELL"))
(message "  Git available: %s" (executable-find "git"))
(message "  init.el exists: %s" (file-exists-p "~/.emacs.d/init.el"))

;; Load init.el naturally - works for both build and runtime
(condition-case err
    (progn
      (load-file "~/.emacs.d/init.el")
      (if (getenv "EMACS_BATCH_MODE")
          (message "â Build: init.el loaded, packages installed")
        (message "â Runtime: init.el loaded with pre-installed packages")))
  (error 
    (message "Error loading init.el: %s" err)
    (if (getenv "EMACS_BATCH_MODE")
        (progn
          (message "Build failed: init.el could not load")
          (kill-emacs 1))
      (message "Runtime warning: init.el had issues, continuing..."))))

;; Batch mode: just exit after loading init.el
(when (getenv "EMACS_BATCH_MODE")
  (message "â Package installation complete - exiting")
  (kill-emacs 0))

;; Runtime mode: start services
(when (string= "${START_HTTP}" "true")
  (when (fboundp 'emacs-lisply-start-server)
    (setq emacs-lisply-port ${HTTP_PORT})
    (emacs-lisply-start-server)
    (message "â Lisply HTTP server started on port ${HTTP_PORT}")))

;; Start default Emacs server (Unix socket) for emacsclient
(server-start)
(message "â Emacs server started (Unix socket)")

(message "â Emacs daemon ready with full IDE configuration!")
(message "   - HTTP API: port ${HTTP_PORT}")
(message "   - Terminal IDE: emacsclient -t")
(message "   - Projects: /projects")
EOF

if [ "$BATCH_MODE" = "true" ]; then
    # Build-time execution: install packages and exit
    echo "Installing packages defined in ~/.emacs.d/init.el..."
    echo "This may take several minutes..."
    
    # Set environment flag for Emacs to detect batch mode
    export EMACS_BATCH_MODE=true
    
    # Run emacs in batch mode with timeout
    TERM=dumb timeout 600 emacs --batch --load /tmp/emacs-startup.el || {
        exit_code=$?
        if [ $exit_code -eq 124 ]; then
            echo "â Package installation timed out after 10 minutes"
            exit 1
        elif [ $exit_code -ne 0 ]; then
            echo "â Package installation failed with exit code: $exit_code"
            exit 1
        fi
    }
    
    echo "â Build-time package installation completed"
    
else
    # Runtime execution: start daemon for interactive use
    echo "Starting Emacs daemon (packages pre-installed, should be fast)..."
    
    # Ensure batch mode flag is not set
    unset EMACS_BATCH_MODE
    
    # Start daemon in background
    emacs --daemon --load /tmp/emacs-startup.el > /tmp/emacs-daemon.log 2>&1 &
    EMACS_PID=$!
    
    # Wait for daemon to start
    echo "Waiting for daemon to start..."
    for i in {1..30}; do
        if emacsclient --eval "t" > /dev/null 2>&1; then
            echo "â Emacs daemon ready"
            break
        elif [ $i -eq 30 ]; then
            echo "â Daemon failed to start"
            echo "Daemon log:"
            cat /tmp/emacs-daemon.log
            exit 1
        else
            sleep 1
        fi
    done
    
    # Show runtime status
    echo ""
    echo "=== Skewed Emacs Container Ready ==="
    echo "â Emacs daemon running (PID: $EMACS_PID)"
    echo "â Full init.el configuration active"
    echo "â HTTP API available on port $HTTP_PORT"
    echo "â All packages pre-installed during build"
    echo ""
    echo "Usage:"
    echo "  # Test HTTP API"
    echo "  curl http://localhost:$HTTP_PORT/lisply/ping-lisp"
    echo ""
    echo "  # Full terminal IDE"
    echo "  emacsclient -t"
    echo ""
    echo "  # Quick evaluation"
    echo "  emacsclient --eval '(+ 1 2 3)'"
    echo ""
    
    # Start the REPL as the main process (if it exists)
    if [ -f /home/emacs-user/emacs-repl.sh ]; then
        exec /home/emacs-user/emacs-repl.sh
    else
        # Fallback: keep container running
        echo "Keeping container alive..."
        tail -f /tmp/emacs-daemon.log
    fi
fi

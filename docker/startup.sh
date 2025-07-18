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


echo "Starting Emacs daemon (packages pre-installed, should be fast)..."
    # Start daemon in background
SHELL=/bin/bash TERM=${TERM} COLORTERM=${COLORTERM} emacs --daemon --load /home/emacs-user/.emacs.d/init.el > /tmp/emacs-daemon.log 2>&1 &
EMACS_PID=$!
    
# Wait for daemon to start
echo "Waiting for daemon to start..."
for i in {1..30}; do
    if emacsclient --eval "t" > /dev/null 2>&1; then
        echo "Emacs daemon ready"
        break
    elif [ $i -eq 30 ]; then
        echo "Daemon failed to start"
        echo "Daemon log:"
        cat /tmp/emacs-daemon.log
        exit 1
    else
        sleep 1
    fi
done

# Show runtime status
echo ""
echo ""
echo "Usage:"
echo "  # Test HTTP API"
echo "  curl http://localhost:$HTTP_PORT/lisply/ping-lisp"
echo ""
echo "  # Full terminal IDE"
echo "  source dot-files/bash_profile; eskew"
echo ""
echo "M-x eat, then claudly for claud code."
echo " "
echo " M-x slime-connect for Gendl"
echo " "

exec /home/emacs-user/emacs-repl.sh
    


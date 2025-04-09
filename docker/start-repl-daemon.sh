#!/bin/bash
# Start an Emacs daemon for REPL functionality

# Get the directory of this script
SCRIPT_DIR="$(dirname "$0")"

# Start Emacs as a daemon if not already running
if ! emacsclient -e '(+ 1 1)' &>/dev/null; then
  echo "Starting Emacs daemon..."
  emacs --daemon=repl-daemon
  # Wait for daemon to be ready
  sleep 1
fi

# Load our REPL code into the daemon
emacsclient -s repl-daemon -e "(progn
  (load-file \"$SCRIPT_DIR/elisp-repl-daemon.el\")
  (message \"REPL daemon ready\"))"

echo "REPL daemon loaded and ready"
echo "Use ./repl-client.sh to connect and use the REPL"
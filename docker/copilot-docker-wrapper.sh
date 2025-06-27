#!/bin/bash
# Improved wrapper script for Copilot language server via Docker
# This script maintains a persistent stdio connection

# Enable strict error handling
set -euo pipefail

# Ensure proper signal forwarding
trap 'kill -TERM $PID 2>/dev/null || true; wait $PID 2>/dev/null || true' TERM INT EXIT

# Function to check if container is running
check_container() {
    if ! docker ps --format '{{.Names}}' | grep -q '^lisply-mcp$'; then
        echo "Error: Container 'lisply-mcp' is not running" >&2
        echo "Please start the container first" >&2
        exit 1
    fi
}

# Check container status
check_container

# Method 1: Direct docker exec with proper signal handling and unbuffered I/O
exec docker exec -i lisply-mcp sh -c '
    # Ensure unbuffered I/O
    export NODE_ENV=production
    export HOME=/home/node
    cd /home/node
    
    # Use stdbuf to disable buffering if available, otherwise rely on node
    if command -v stdbuf >/dev/null 2>&1; then
        exec stdbuf -i0 -o0 -e0 npx @github/copilot-language-server --stdio
    else
        exec npx @github/copilot-language-server --stdio
    fi
' &

# Store the background process PID
PID=$!

# Wait for the process to complete
wait $PID
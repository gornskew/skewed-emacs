#!/bin/bash
# Singleton wrapper for Copilot language server via Docker
# Ensures only one copilot process runs at a time

# Check if container is running
if ! docker ps --format '{{.Names}}' | grep -q '^lisply-mcp$'; then
    echo "Error: Container 'lisply-mcp' is not running" >&2
    exit 1
fi

# Kill any existing copilot processes in the container
echo "Cleaning up any existing copilot processes..." >&2
docker exec lisply-mcp sh -c 'pkill -f copilot-language-server || true' >/dev/null 2>&1

# Wait a moment for cleanup
sleep 1

# Start the copilot server with resource limits and proper signal handling
exec docker exec -i lisply-mcp sh -c '
    cd /home/node
    
    # Set resource limits and environment
    export NODE_ENV=production
    export NODE_OPTIONS="--max-old-space-size=256"
    ulimit -v 524288  # Limit virtual memory to 512MB
    
    # Execute with timeout to prevent runaway processes
    exec timeout 1800 npx @github/copilot-language-server --stdio
'
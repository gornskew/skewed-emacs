#!/bin/bash
# Helper script to run Claude Code in the Skewed Emacs container

CONTAINER_NAME="${CONTAINER_NAME:-skewed-emacs}"

# Check if container is running
if ! docker ps --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
    echo "Error: Container ${CONTAINER_NAME} is not running"
    echo "Start it with: ./run --network -d"
    exit 1
fi

# Check if claude-code is available in container
if ! docker exec ${CONTAINER_NAME} which claude-code > /dev/null 2>&1; then
    echo "Error: claude-code not found in container"
    echo "Rebuild the container with: ./build"
    exit 1
fi

# Check if user is already logged in
if docker exec ${CONTAINER_NAME} claude-code auth status > /dev/null 2>&1; then
    echo "✓ Claude Code is already authenticated"
else
    echo "Claude Code authentication required..."
    echo "Running interactive login in container..."
    docker exec -it ${CONTAINER_NAME} claude-code auth login
fi

# Run claude-code with all arguments passed through
echo "Running: claude-code $@"
docker exec -it ${CONTAINER_NAME} claude-code "$@"
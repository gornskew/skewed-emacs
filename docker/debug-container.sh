#!/bin/bash
# Quick debug script for running container

set -e

echo "=== Container Debug Script ==="

# Get the latest container ID for skewed-emacs
CONTAINER_ID=$(docker ps -a --filter ancestor=skewed-emacs:dev --format "{{.ID}}" | head -1)

if [ -z "$CONTAINER_ID" ]; then
    echo "No skewed-emacs container found"
    exit 1
fi

echo "Found container: $CONTAINER_ID"
echo ""

echo "=== Container Status ==="
docker ps --filter id=$CONTAINER_ID

echo ""
echo "=== Port Mappings ==="
docker port $CONTAINER_ID

echo ""
echo "=== Container Logs (last 50 lines) ==="
docker logs --tail 50 $CONTAINER_ID

echo ""
echo "=== Process List Inside Container ==="
docker exec $CONTAINER_ID ps aux

echo ""
echo "=== Network Status Inside Container ==="
docker exec $CONTAINER_ID netstat -tlnp

echo ""
echo "=== Test Internal HTTP ==="
docker exec $CONTAINER_ID curl -v http://localhost:7080/lisply/ping-lisp || echo "Internal HTTP test failed"

echo ""
echo "=== Test External HTTP ==="
EXTERNAL_PORT=$(docker port $CONTAINER_ID 7080/tcp | cut -d: -f2)
if [ -n "$EXTERNAL_PORT" ]; then
    echo "Testing external port: $EXTERNAL_PORT"
    curl -v http://localhost:$EXTERNAL_PORT/lisply/ping-lisp || echo "External HTTP test failed"
else
    echo "No external port mapping found"
fi

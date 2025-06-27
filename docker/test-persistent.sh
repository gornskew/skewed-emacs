#!/bin/bash
# Test script to verify persistent connection

echo "Testing persistent connection to copilot-language-server..." >&2

# Try the most basic approach with signal forwarding
docker exec -i --sig-proxy=true lisply-mcp   sh -c 'cd /home/node && exec npx @github/copilot-language-server --stdio'

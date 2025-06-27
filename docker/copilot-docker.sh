#!/bin/bash
# Enhanced wrapper script for Copilot language server via Docker


# Use unbuffered I/O and ensure proper signal handling
exec docker exec -it lisply-mcp sh -c '
  cd /home/node
  exec npx @github/copilot-language-server --stdio
'

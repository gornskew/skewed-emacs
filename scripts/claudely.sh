#!/bin/bash

function claudly (dir)
{
    file=${1:-"."};
    
    # Update Claude Code to latest version
    echo "Checking for Claude Code updates..."
    docker exec lisply-mcp bash -c 'cd ~/.claude/local && npm update @anthropic-ai/claude-code' >/dev/null 2>&1
    
    # First, merge the MCP configs inside the lisply-mcp container
    echo "Merging MCP configurations..."
    docker exec lisply-mcp node /app/scripts/merge-mcp-config.js >/dev/null 2>&1
    
    # Run Claude with the merged config
    echo "Starting Claude Code with MCP integration..."
    docker exec -it --detach-keys "ctrl-^" lisply-mcp  /home/node/.claude/local/claude --mcp-config /tmp/merged-mcp-config.json
}


claudly $



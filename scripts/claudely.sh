#!/bin/bash

function claudly (dir)
{
    file=${1:-"."};
    
    # Update Claude Code to latest version
    echo "Checking for Claude Code updates..."
    ssh lisply-mcp 'cd ~/.claude/local && npm update @anthropic-ai/claude-code' >/dev/null 2>&1
    
    # First, merge the MCP configs inside the lisply-mcp container
    echo "Merging MCP configurations..."
    ssh lisply-mcp 'node /app/scripts/merge-mcp-config.js' >/dev/null 2>&1
    
    # Run Claude with the merged config
    echo "Starting Claude Code with MCP integration..."
    ssh -t lisply-mcp  'cd /projects && /home/node/.claude/local/claude --mcp-config /tmp/merged-mcp-config.json'
}


claudly $



# SSH-based aliases for accessing lisply-mcp container
# These replace the docker exec based versions for better security
#
# SSH keys and config are baked into the skewed-emacs Docker image.
# Just use 'ssh lisply-mcp' - the config handles everything.

function claudly 
{
    # Update Claude Code to latest version
    echo "Checking for Claude Code updates..."
    ssh lisply-mcp 'cd ~/.claude/local && npm update @anthropic-ai/claude-code' >/dev/null 2>&1
    
    # Merge the MCP configs
    echo "Merging MCP configurations..."
    ssh lisply-mcp 'node /app/scripts/merge-mcp-config.js' >/dev/null 2>&1
    
    # Run Claude with the merged config
    echo "Starting Claude Code with MCP integration..."
    ssh -t lisply-mcp '/home/node/.claude/local/claude --mcp-config /tmp/merged-mcp-config.json'
}


function geminly 
{
    # Merge the MCP configs
    echo "Merging MCP configurations..."
    ssh lisply-mcp 'node /app/scripts/merge-mcp-config.js' >/dev/null 2>&1
    ssh lisply-mcp 'cp /tmp/merged-mcp-config.json /home/node/.gemini/settings.json' >/dev/null 2>&1
    
    # Run Gemini
    echo "Starting Gemini CLI with MCP integration..."
    ssh -t lisply-mcp '/home/node/.gemini/local/gemini'
}


function codexly
{
    # Run Codex
    echo "Starting OpenAI Codex CLI..."
    ssh -t lisply-mcp '/home/node/.codex/local/codex'
}


# Generic SSH shell into lisply-mcp
function lisply-shell
{
    echo "Opening shell in lisply-mcp container..."
    ssh -t lisply-mcp
}

# Test SSH connection to lisply-mcp
function lisply-test
{
    echo "Testing SSH connection to lisply-mcp..."
    if ssh lisply-mcp 'echo Connection successful!'; then
        echo "SSH connection to lisply-mcp is working."
    else
        echo "SSH connection failed. Check that:"
        echo "  1. lisply-mcp container is running"
        echo "  2. skewed-emacs has started (exports SSH key on startup)"
        echo "  3. Both containers are on the same network"
    fi
}

# SSH-based aliases for accessing lisply-mcp container
# These replace the docker exec based versions for better security
# Source this file in your .bash_profile or add these functions directly

# SSH connection settings for lisply-mcp
LISPLY_MCP_HOST="lisply-mcp"  # Container hostname on the docker network
LISPLY_MCP_USER="node"
LISPLY_MCP_SSH_KEY="$HOME/.ssh/lisply-mcp"

# Common SSH options
_lisply_ssh_opts() {
    echo "-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR"
}

function claudly 
{
    file="${1:-.}"
    
    # Update Claude Code to latest version (via SSH)
    echo "Checking for Claude Code updates..."
    ssh $(_lisply_ssh_opts) -i "$LISPLY_MCP_SSH_KEY" \
        "${LISPLY_MCP_USER}@${LISPLY_MCP_HOST}" \
        'cd ~/.claude/local && npm update @anthropic-ai/claude-code' >/dev/null 2>&1
    
    # Merge the MCP configs inside the lisply-mcp container
    echo "Merging MCP configurations..."
    ssh $(_lisply_ssh_opts) -i "$LISPLY_MCP_SSH_KEY" \
        "${LISPLY_MCP_USER}@${LISPLY_MCP_HOST}" \
        'node /app/scripts/merge-mcp-config.js' >/dev/null 2>&1
    
    # Run Claude with the merged config (interactive SSH session)
    echo "Starting Claude Code with MCP integration..."
    ssh $(_lisply_ssh_opts) -t -i "$LISPLY_MCP_SSH_KEY" \
        "${LISPLY_MCP_USER}@${LISPLY_MCP_HOST}" \
        '/home/node/.claude/local/claude --mcp-config /tmp/merged-mcp-config.json'
}


function geminly 
{
    file="${1:-.}"
    
    # Merge the MCP configs inside the lisply-mcp container
    echo "Merging MCP configurations..."
    ssh $(_lisply_ssh_opts) -i "$LISPLY_MCP_SSH_KEY" \
        "${LISPLY_MCP_USER}@${LISPLY_MCP_HOST}" \
        'node /app/scripts/merge-mcp-config.js' >/dev/null 2>&1
    
    ssh $(_lisply_ssh_opts) -i "$LISPLY_MCP_SSH_KEY" \
        "${LISPLY_MCP_USER}@${LISPLY_MCP_HOST}" \
        'cp /tmp/merged-mcp-config.json /home/node/.gemini/settings.json' >/dev/null 2>&1
    
    # Run Gemini with the merged config (interactive SSH session)
    echo "Starting Gemini CLI with MCP integration..."
    ssh $(_lisply_ssh_opts) -t -i "$LISPLY_MCP_SSH_KEY" \
        "${LISPLY_MCP_USER}@${LISPLY_MCP_HOST}" \
        '/home/node/.gemini/local/gemini'
}


function codexly
{
    file="${1:-.}"
    
    # Run Codex (interactive SSH session)
    echo "Starting OpenAI Codex CLI..."
    ssh $(_lisply_ssh_opts) -t -i "$LISPLY_MCP_SSH_KEY" \
        "${LISPLY_MCP_USER}@${LISPLY_MCP_HOST}" \
        '/home/node/.codex/local/codex'
}


# Generic SSH shell into lisply-mcp
function lisply-shell
{
    echo "Opening shell in lisply-mcp container..."
    ssh $(_lisply_ssh_opts) -t -i "$LISPLY_MCP_SSH_KEY" \
        "${LISPLY_MCP_USER}@${LISPLY_MCP_HOST}"
}

# Test SSH connection to lisply-mcp
function lisply-test
{
    echo "Testing SSH connection to lisply-mcp..."
    if ssh $(_lisply_ssh_opts) -i "$LISPLY_MCP_SSH_KEY" \
        "${LISPLY_MCP_USER}@${LISPLY_MCP_HOST}" 'echo Connection successful!'; then
        echo "SSH connection to lisply-mcp is working."
    else
        echo "SSH connection failed. Check that:"
        echo "  1. lisply-mcp container is running"
        echo "  2. SSH keys are set up (run setup-ssh-keys.sh)"
        echo "  3. docker-compose.yml has the correct volume mounts"
    fi
}

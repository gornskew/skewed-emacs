# Claude Desktop Integration (Windows)

If you want to use Claude Desktop with the Skewed Emacs MCP servers,
this guide walks you through the setup. This assumes you have already
completed the basic [Containerized Runnings](../README.md#containerized-runnings-recommended)
setup.

## Prerequisites

- Windows with [WSL2](https://docs.microsoft.com/en-us/windows/wsl/install) installed
- [Docker Desktop](https://www.docker.com/products/docker-desktop/) running with WSL2 backend
- [Claude Desktop](https://claude.ai/download) installed
- skewed-emacs cloned and stack started at least once (see main README)

## Setup

1. **Start the stack** (if not already running):

   ```bash
   cd ~/projects/skewed-emacs
   ./compose-dev up
   ```

   Wait for the `[SUCCESS] MCP configurations ready` message.

2. **Copy the generated config to Claude Desktop**:

   ```bash
   cp mcp/claude_desktop_config.json /mnt/c/Users/YOUR_USERNAME/AppData/Roaming/Claude/claude_desktop_config.json
   ```

   Replace `YOUR_USERNAME` with your Windows username.

   Alternatively, from Windows Explorer:
   - Source: `\\wsl$\Ubuntu\home\YOUR_WSL_USER\projects\skewed-emacs\mcp\claude_desktop_config.json`
   - Destination: `%APPDATA%\Claude\claude_desktop_config.json`

3. **Restart Claude Desktop** — you should see three MCP servers connect:
   - `skewed-emacs` — Emacs Lisp evaluation
   - `gendl-sbcl` — Common Lisp (SBCL) with Gendl
   - `gendl-ccl` — Common Lisp (CCL) with Gendl

## Daily Usage

The Docker stack must be running for Claude Desktop to use the MCP servers:

```bash
cd ~/projects/skewed-emacs
./compose-dev up -d   # -d for daemon mode (no interactive shell)
```

To stop the stack:

```bash
cd ~/projects/skewed-emacs
./compose-dev down
```

## What You Can Do

With these MCP servers, Claude Desktop can:

- Evaluate Emacs Lisp code and interact with the Emacs environment
- Evaluate Common Lisp code in SBCL or CCL
- Work with the Gendl geometry kernel for CAD/knowledge-based engineering
- Access documentation and run HTTP requests against the backend services

## Merging with Existing MCP Configuration

If you already have other MCP servers configured in Claude Desktop, you will
need to manually merge the `mcpServers` entries from the generated
`claude_desktop_config.json` into your existing configuration file.

## Cloning to a Different Location

The generated `claude_desktop_config.json` contains the absolute path to
your skewed-emacs clone. If you clone to somewhere other than
`~/projects/skewed-emacs`, the config will automatically use the correct
path — it is determined at `./compose-dev up` time based on where you
run the command.

## Troubleshooting

**MCP servers not connecting:**
- Ensure the Docker stack is running (`docker ps` should show the containers)
- Check that the paths in `claude_desktop_config.json` match your WSL clone location
- Restart Claude Desktop after copying the config

**"emacsclient not ready" warning on first start:**
- This is normal — the Emacs daemon takes a few seconds to initialize
- Run `./compose-dev up` again and the MCP config will generate successfully

**Config still has `${SKEWED_CLONE_PATH}` placeholder:**
- The substitution happens at merge time; re-run `./compose-dev up`
- If it persists, check that the stack is fully up before the merge runs

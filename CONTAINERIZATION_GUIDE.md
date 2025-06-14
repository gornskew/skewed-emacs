# Quick Start Commands for Emacs Lisply Backend

## 1. Build the Container
```bash
cd /projects/skewed-emacs/docker
chmod +x build
./build
```

## 2. Test the Container Locally
```bash
# Test basic functionality
docker run --rm -p 7080:7080 genworks/skewed-emacs:devo

# In another terminal, test the API
curl http://localhost:7080/lisply/ping-lisp
curl -X POST http://localhost:7080/lisply/lisp-eval \
     -H "Content-Type: application/json" \
     -d '{"code": "(+ 1 2 3)"}'
```

## 3. Push to Docker Hub
```bash
# Make sure you're logged in to Docker Hub
docker login

# Push the image
docker push genworks/skewed-emacs:devo
```

## 4. Update Your MCP Configuration
Replace your current emacs MCP config with:

```json
{
    "mcpServers": {
        "emacs": {
            "command": "wsl",
            "args": [
                "node",
                "/home/dcooper8/projects/lisply-mcp/scripts/mcp-wrapper.js",
                "--mount", "/home/dcooper8/projects:/projects",
                "--image-base-name", "genworks/skewed-emacs",
                "--http-host-port", "7080",
                "--server-name", "emacs"
            ]
        }
    }
}
```

## 5. Test with Claude Desktop
After updating the config and restarting Claude Desktop, the system should:
- Automatically pull `genworks/skewed-emacs:devo` 
- Start a container with environment variables
- Wait for the HTTP server to be available
- Provide `emacs__lisp_eval` and `emacs__ping_lisp` tools

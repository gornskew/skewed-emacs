# Docker Compose for Skewed Emacs + Gendl Development Environment

This Docker Compose setup provides a standardized way to manage both
the Skewed Emacs and Gendl lisply-backend containers,
Dr. Frankenstein'd together from the existing hodgepodge of run
scripts.

## Quick Start

```bash
# Initialize environment and start services
./compose-dev up

# Start only specific services
./compose-dev up emacs     # Just Emacs
./compose-dev up gendl     # Just Gendl

# Connect to services
./compose-dev emacs        # Interactive Emacs session
./compose-dev shell gendl  # Shell in Gendl container

# Test APIs
./compose-dev test         # Test both HTTP APIs

# View logs
./compose-dev logs         # All services
./compose-dev logs gendl   # Just Gendl

# Stop everything
./compose-dev down
```

## Sample Run-through

2. Use eskew to jump into an emacsclient session
```bash
# Use the eskew function (from bash_profile) to connect to Emacs
source dot-files/bash_profile  # if not already sourced
eskew
```

3. Use Slime/Swank from emacs to connect your emacs to the Gendl session:
```emacs
;; Connect to Gendl (Common Lisp-based) swank service:
M-x slime-connect RET gendl RET 4200 RET
;; Test with a simple object
(theo (make-object 'box :width 10 :height 5 :length 3) volume)
```

4. Make an `*eat*` terminal in emacs

```emacs
M-x eat
```

5.  Run Claude Code in it:
```bash
claudly
```


### Container Management

```bash
# Service management
./compose-dev up              # Start all services
./compose-dev down            # Stop all services  
./compose-dev restart gendl   # Restart specific service
./compose-dev logs emacs      # View service logs
./compose-dev status          # Show container status
```



## Files

### `docker-compose.yml`
The main compose configuration that defines both services with:
- Port mappings compatible with existing run scripts
- Environment variables matching original configurations  
- Health checks for service reliability
- Shared network for inter-container communication
- Volume mounts for project directories

### `generate-env.sh`
Template for environment customization. Edit this as neededbefore running `./compose-dev`:
```bash
cp .env.example .env
```


### Network Setup (default ports shown)

```
Docker Host Machine
├── Port 7081 → skewed-emacs:7080 (Emacs HTTP API)
├── Port 9081 → gendl:9080 (Gendl HTTP API)
└── Port 4201 → gendl:4200 (Gendl SWANK for SLIME)
```

## 🔗 Alternative Approach: MCP Client Integration

If you prefer to integrate with MCP clients (like Claude Desktop)
without managing containers at the command-line, use the **Second
Way** approach:

1. Clone the [lisply-mcp
   repository](https://github.com/gornskew/lisply-mcp) instead
2. Configure your MCP client with the `mcp-exec` script as described in the [README](https://github.com/gornskew/lisply-mcp/README.md)
3. Containers will be started automatically when needed



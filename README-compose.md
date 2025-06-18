# Docker Compose for Skewed Emacs + Gendl Development Environment

This Docker Compose setup provides a standardized way to manage both the Skewed Emacs and Gendl lisply-backend containers, Dr. Frankenstein'd together from the existing hodgepodge of run scripts.

## Quick Start

```bash
# Initialize environment and start both services
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

## Files Created

### `docker-compose.yml`
The main compose configuration that defines both services with:
- Port mappings compatible with existing run scripts
- Environment variables matching original configurations  
- Health checks for service reliability
- Shared network for inter-container communication
- Volume mounts for project directories

### `.env.example`
Template for environment customization. Copy to `.env` and modify as needed:
```bash
cp .env.example .env
```

### `compose-dev` (Executable)
Management script with commands for service lifecycle, interactive access, testing and monitoring.

## Architecture

### Emacs Service: `localhost:7081` â `container:7080`
### Gendl Service: `localhost:9081` â `container:9080` (HTTP), `localhost:4201` â `container:4200` (SWANK)
### Network: `emacs-gendl-network` for inter-container communication

## Usage

```bash
# Start everything
./compose-dev up

# Test APIs  
curl -X POST http://localhost:7081/lisply/lisp-eval -d '{"code": "(+ 1 2 3)"}'
curl -X POST http://localhost:9081/lisply/lisp-eval -d '{"code": "(+ 1 2 3)"}'

# SLIME from host: M-x slime-connect RET localhost RET 4201 RET
# SLIME from container: M-x slime-connect RET gendl-backend RET 4200 RET
```

See the full README artifacts above for complete documentation.
# Docker Compose Setup

## Quick Start

1. **Generate your environment file:**
   ```bash
   ./generate-env.sh
   ```
   This creates a `.env` file with your local paths and user settings.

2. **Start the services:**
   ```bash
   docker compose up
   ```

3. **Connect to Emacs:**
   ```bash
   docker compose exec skewed-emacs emacsclient -t
   ```

## Environment Variables

The `generate-env.sh` script automatically detects and sets:

- `HOST_HOME` - Your home directory path
- `PROJECTS_DIR` - Path to your projects directory  
- `USER_CONFIG_DIR` - Path to your .config directory
- `HOST_USER_UID` - Your user ID for proper file permissions
- `DOCKER_GROUP_ID` - Docker group ID for Docker socket access

## Manual Configuration

If you need to customize settings, edit the `.env` file after running `generate-env.sh`:

```bash
# Example .env customizations
PROJECTS_DIR=/custom/projects/path
USER_CONFIG_DIR=/custom/config/path
EMACS_IMAGE_TAG=latest
```

## Troubleshooting

**Mount Issues:**
- Ensure you've run `./generate-env.sh` 
- Check that paths in `.env` exist and are accessible
- Verify user permissions on mounted directories

**Container Communication:**
- All services use the `skewed-emacs-network` bridge network
- Emacs can connect to Gendl via hostname `gendl-backend:4200`
- MCP services communicate through the lisply-mcp container

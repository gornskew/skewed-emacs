#!/bin/bash
# Generate .env file for docker-compose
# Run this script to create your local .env file

set -e

echo "Generating .env file for docker-compose..."

# Detect current user's home directory
if [ -z "$USER_HOME" ]; then
    USER_HOME="$HOME"
fi

# Create .env file
cat > .env << EOF
# Docker Compose Environment Variables
# Generated on $(date)

# User paths
HOST_HOME=$USER_HOME
PROJECTS_DIR=$USER_HOME/projects
USER_CONFIG_DIR=$USER_HOME/.config

# User/Group IDs
HOST_USER_UID=$(id -u)
DOCKER_GROUP_ID=$(getent group docker | cut -d: -f3 || echo 999)

# Container names (optional overrides)
EMACS_CONTAINER_NAME=skewed-emacs
GENDL_CONTAINER_NAME=gendl
NODE_CONTAINER_NAME=lisply-mcp

# Network name
DOCKER_NETWORK_NAME=skewed-emacs-network

# Image tags
EMACS_IMAGE_TAG=devo
GENDL_IMAGE_TAG=devo-ccl
NODE_IMAGE_TAG=devo

# Port configurations
EMACS_HTTP_HOST_PORT=7081
EMACS_HTTP_PORT=7080
GENDL_HTTP_HOST_PORT=9081  
GENDL_HTTP_PORT=9080
GENDL_SWANK_HOST_PORT=4201
GENDL_SWANK_PORT=4200
NODE_DEV_HOST_PORT=3001
NODE_DEV_PORT=3000
NODE_API_HOST_PORT=8081
NODE_API_PORT=8080
NODE_ALT_HOST_PORT=9001
NODE_ALT_PORT=9000
EOF

echo "Created .env file with the following settings:"
echo "HOME: $USER_HOME"
echo "PROJECTS: $USER_HOME/projects"
echo "CONFIG: $USER_HOME/.config"
echo "UID: $(id -u)"
echo "Docker GID: $(getent group docker | cut -d: -f3 || echo 999)"
echo ""
echo "You can now run: docker compose up"

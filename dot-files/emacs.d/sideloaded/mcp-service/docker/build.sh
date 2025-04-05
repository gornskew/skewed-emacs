#!/bin/bash

# build.sh - Build Emacs MCP Docker container
# 
# Copyright (C) 2025 Genworks
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

set -e

# Default values
TAG="latest"
IMAGE_NAME="emacs-mcp"

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    -t|--tag)
      TAG="$2"
      shift 2
      ;;
    -n|--name)
      IMAGE_NAME="$2"
      shift 2
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

# Full image name with tag
FULL_IMAGE_NAME="${IMAGE_NAME}:${TAG}"

echo "Building Docker image: ${FULL_IMAGE_NAME}"

# Get script directory and project root directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Build the Docker image from the project root, specifying the Dockerfile in docker/
cd "${PROJECT_ROOT}"

# Check if we're using a private repository
ECHO_WARNING=""
SSH_KEY_ARG=""

# If the user has an SSH key, use it for private repository cloning
SSH_KEY_FILE="${HOME}/.ssh/id_rsa"
if [ -f "${SSH_KEY_FILE}" ]; then
  # Add the SSH key as a build argument
  SSH_KEY_ARG="--build-arg SSH_PRIVATE_KEY=\"$(cat ${SSH_KEY_FILE})\""
  echo "Using SSH key for repository access."
else
  ECHO_WARNING="echo \"Warning: No SSH key found at ${SSH_KEY_FILE}. Will build without cloning the config repository.\";"
fi

# Warn if no SSH key is found
eval "${ECHO_WARNING}"

# Build with or without SSH key
eval "docker build ${SSH_KEY_ARG} -t \"${FULL_IMAGE_NAME}\" -f \"${SCRIPT_DIR}/Dockerfile\" ."

echo "Successfully built ${FULL_IMAGE_NAME}"

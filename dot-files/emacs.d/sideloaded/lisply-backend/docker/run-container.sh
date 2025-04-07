#!/bin/bash

# run-container.sh - Run Emacs Lisply Backend Docker container
# 
# Copyright (C) 2025 Genworks
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

set -e

# Default values
IMAGE_NAME="emacs-lisply:latest"
PORT=7081
MOUNT_DIR=""

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    -i|--image)
      IMAGE_NAME="$2"
      shift 2
      ;;
    -p|--port)
      PORT="$2"
      shift 2
      ;;
    -m|--mount)
      MOUNT_DIR="$2"
      shift 2
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

# Create mount argument if mount directory is provided
MOUNT_ARG=""
if [ -n "${MOUNT_DIR}" ]; then
  MOUNT_ARG="-v ${MOUNT_DIR}:/projects"
fi

# Run the container
echo "Starting Emacs Lisply Backend container..."
docker run -d \
  --name emacs-lisply-server \
  -p ${PORT}:7080 \
  ${MOUNT_ARG} \
  ${IMAGE_NAME}

echo "Container started. Emacs Lisply Backend is available at http://localhost:${PORT}/lisply"
echo "Use 'docker logs emacs-lisply-server' to check the logs."
echo "Use 'docker stop emacs-lisply-server' to stop the container."

#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/.."

# Always rebuild the base image for now to make sure we get a clean install
echo "Building base Emacs 30.1 image (this may take a while)..."
docker build -t skewed-emacs-base:latest -f docker/emacs-base.dockerfile .

# If build failed, exit
if [ $? -ne 0 ]; then
  echo "Failed to build base image. Exiting."
  exit 1
fi

# Build the skewed-emacs image
echo "Building skewed-emacs image..."
docker build -t skewed-emacs:latest -f docker/Dockerfile .

echo "Build complete!"
echo "Run the container with: ./docker/run-container.sh"
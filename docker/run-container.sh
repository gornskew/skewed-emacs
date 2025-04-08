#!/bin/bash
set -e

# Run skewed-emacs container with port mapping
docker run -it --rm \
  -p 7080:7080 \
  -p 2222:22 \
  --name skewed-emacs-container \
  skewed-emacs:latest

echo "Container stopped"
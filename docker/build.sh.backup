#!/bin/bash
# Enhanced build script with runtime build step for Skewed Emacs

# Change to the directory of this script, regardless of where it's called from
cd "$(dirname "$0")"
SCRIPT_DIR="$(pwd)"

set -e  # Exit on first error

echo "=== Starting build for Skewed Emacs ==="
echo "=== Script running from: $SCRIPT_DIR ==="

# Step 1: Build the base image with everything except the problematic native compilation
echo "=== Building skewed-emacs-base image ==="
docker build -t skewed-emacs-base -f skewed-emacs-base.dockerfile ..

# Step 2: Run the container with a command that keeps it alive
echo "=== Starting container in detached mode ==="
CONTAINER_ID=$(docker run -d skewed-emacs-base tail -f /dev/null)
echo "Container ID: $CONTAINER_ID"

# Step 3: Execute the native compilation within the running container
echo "=== Running native compilation in container ==="
echo "    This may take a few minutes depending on your system..."
docker exec $CONTAINER_ID /bin/bash -c "cd /home/emacs-user && sudo -u emacs-user emacs --batch --load /home/emacs-user/.emacs.d/init.el && echo 'Native compilation completed successfully'"

# Check if successful
if [ $? -ne 0 ]; then
    echo "WARNING: Native compilation exited with non-zero status, but continuing anyway..."
    # Optionally view logs to debug
    docker logs $CONTAINER_ID
fi

# Create a temporary directory for extraction
TEMP_DIR=$(mktemp -d)
echo "=== Extracting compiled files to $TEMP_DIR ==="

# Identify the real directory behind the symlink
echo "=== Checking symlink structure for .emacs.d ==="
docker exec $CONTAINER_ID ls -la /home/emacs-user/.emacs.d

# Extract compiled files from the actual directory (not the symlink)
echo "=== Extracting files from the real directory that .emacs.d points to ==="
docker cp $CONTAINER_ID:/home/emacs-user/skewed-emacs/dot-files/emacs.d $TEMP_DIR/

# List the contents to verify
ls -la $TEMP_DIR

# Clean up the container
echo "=== Cleaning up container ==="
docker rm -f $CONTAINER_ID

echo "=== Build completed successfully ==="
# Step 3: Build the final image with pre-compiled files
echo "=== Building final image with pre-compiled files ==="
cat > $TEMP_DIR/Dockerfile << EOF
# Final stage Dockerfile with pre-compiled files
FROM skewed-emacs-base:latest

# Add any runtime-only dependencies
USER root
RUN apt-get update && apt-get install -y \\
    strace \\
    lsof \\
    procps \\
    net-tools \\
    && rm -rf /var/lib/apt/lists/*

# Setup the .emacs.d directory
# First, ensure the target directory exists and is not a symlink
RUN rm -rf /home/emacs-user/skewed-emacs/dot-files/emacs.d/

# Copy the pre-compiled files
COPY ./emacs.d/ /home/emacs-user/skewed-emacs/dot-files/emacs.d/

# No need to create symlink - already done in base image
RUN chown -R emacs-user:emacs-user /home/emacs-user/skewed-emacs/dot-files/emacs.d

# Switch back to emacs-user for consistency with base image
USER emacs-user
EOF

# Verify the directory structure before building
echo "=== Verifying directory structure before building ==="
echo "Contents of $TEMP_DIR:"
ls -la $TEMP_DIR
echo "Contents of $TEMP_DIR/emacs.d (if it exists):"
if [ -d "$TEMP_DIR/emacs.d" ]; then
    ls -la $TEMP_DIR/emacs.d
else
    echo "Warning: $TEMP_DIR/emacs.d directory not found"
fi

# Build the final image with the compiled files
echo "=== Building final image from $TEMP_DIR ==="
cd $TEMP_DIR && docker build -t skewed-emacs . || {
    echo "ERROR: Build failed. See above for details."
    echo "Directory contents for debugging:"
    ls -la
    exit 1
}

# Clean up
echo "=== Cleaning up temporary files ==="
rm -rf $TEMP_DIR

# If we got here, all builds succeeded
echo "=== Build successful! ==="
echo "You can run the container with: docker run -it -p 7080:7080 -p 22:22 skewed-emacs"

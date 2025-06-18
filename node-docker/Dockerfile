# Node.js Development Container with Claude Code
# Optimized for development with minimal bloat
FROM node:20-slim

ARG DEBIAN_FRONTEND=noninteractive

# Install essential tools and dependencies
RUN apt-get -y update && apt-get -y upgrade && apt-get -y autoremove && \
    apt-get -y install \
    curl \
    wget \
    git \
    sudo \
    ca-certificates \
    openssh-client \
    && rm -rf /var/cache/apt/* /var/lib/apt/lists/* && apt-get clean

# Install Claude Code globally
RUN npm install -g @anthropic-ai/claude-code

# Use existing node user (already has UID 1000 and is in node group)
# Just ensure it has bash shell
RUN usermod -s /bin/bash node

# Create project directory structure (will be mounted from host)
RUN mkdir -p /projects && \
    chown node:node /projects

# Switch to node user for all subsequent operations
USER node
WORKDIR /home/node

# Set environment variables for runtime
ENV TERM=xterm-256color
ENV NODE_ENV=development

# Expose common Node.js development ports
EXPOSE 3000 8080 9000

# Default command - keep container running
CMD ["tail", "-f", "/dev/null"]
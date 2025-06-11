# Simplified Dockerfile for Skewed Emacs following Gendl Docker pattern
FROM debian:testing-slim

ARG DEBIAN_FRONTEND=noninteractive

# Install runtime dependencies (similar to Gendl approach)
RUN apt-get -y update && apt-get -y upgrade && apt-get -y autoremove && \
    apt-get -y install \
    # Emacs and basic tools
    emacs-nox \
    # Core utilities
    curl \
    wget \
    git \
    sudo \
    # Network tools
    iputils-ping \
    telnet \
    netcat-openbsd \
    # Development tools for native compilation
    build-essential \
    binutils \
    binutils-dev \
    gcc \
    libc6-dev \
    # Cleanup
    && rm -rf /var/cache/apt/* && apt-get clean

# Create emacs-user similar to gendl-user pattern
RUN groupadd -g 1000 emacs-user && \
    useradd -m -s /bin/bash -u 1000 -g emacs-user emacs-user && \
    ln -s /home /Users

# Create project directory structure
RUN mkdir -p /home/emacs-user/projects && \
    chown emacs-user:emacs-user /home/emacs-user/projects && \
    ln -s /home/emacs-user/projects /projects

# Copy skewed-emacs configuration 
COPY --chown=emacs-user:emacs-user . /home/emacs-user/skewed-emacs/

# Switch to emacs-user
USER emacs-user
WORKDIR /home/emacs-user

# Run the setup script to install configuration
RUN cd skewed-emacs && ./setup

# Verify the lisply backend files are present
RUN ls -la /home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/source/

# Bootstrap Emacs packages during build time (not at runtime)
# This ensures all packages from init.el are downloaded and compiled into the image

# Copy bootstrap script
COPY --chown=emacs-user:emacs-user docker/bootstrap-emacs.sh /tmp/bootstrap-emacs.sh
RUN chmod +x /tmp/bootstrap-emacs.sh

# Run the bootstrap script to install packages
RUN /tmp/bootstrap-emacs.sh

# Verify packages were installed by checking for key directories
RUN ls -la ~/.emacs.d/elpa/ 2>/dev/null && echo \"Ã¢ ELPA packages installed\" || echo \"Ã¢Â¹Ã¯Â¸  No ELPA directory found\"

# Create verification script to avoid shell escaping issues
COPY --chown=emacs-user:emacs-user docker/verify-lisply.sh /tmp/verify-lisply.sh
RUN chmod +x /tmp/verify-lisply.sh && /tmp/verify-lisply.sh

# Copy startup script and make it executable
COPY --chown=emacs-user:emacs-user docker/startup.sh /home/emacs-user/startup.sh
RUN chmod +x /home/emacs-user/startup.sh

# Set environment variables
ENV TERM=xterm-256color

# Expose ports for both Lisply HTTP API and Emacs server
EXPOSE 7080 9999

# Use the startup script to avoid shell escaping issues
CMD ["/home/emacs-user/startup.sh"]

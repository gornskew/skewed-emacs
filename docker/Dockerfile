# Fixed Dockerfile for Skewed Emacs - Working baseline
FROM debian:testing-slim

ARG DEBIAN_FRONTEND=noninteractive

# Install runtime dependencies
RUN apt-get -y update && apt-get -y upgrade && apt-get -y autoremove && \
    apt-get -y install \
    # Emacs and basic tools
    emacs-nox \
    # Core utilities
    curl \
    wget \
    git \
    sudo \
    # Network tools for diagnostics
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
RUN ls -la /home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/source/ && \
    echo "â Lisply backend files verified"

# Bootstrap Emacs packages during build time
# This ensures simple-httpd is available at runtime
COPY --chown=emacs-user:emacs-user docker/bootstrap-emacs.sh /tmp/bootstrap-emacs.sh
RUN chmod +x /tmp/bootstrap-emacs.sh && /tmp/bootstrap-emacs.sh

# Verify packages were installed
RUN ls -la ~/.emacs.d/elpa/ 2>/dev/null && echo "â ELPA packages installed" || echo "â ï¸ No ELPA directory found"

# Copy the FIXED startup script 
COPY --chown=emacs-user:emacs-user docker/startup.sh /home/emacs-user/startup.sh
RUN chmod +x /home/emacs-user/startup.sh

# Set environment variables
ENV TERM=xterm-256color

# Expose ports for both Lisply HTTP API and Emacs server
EXPOSE 7080 9999

# Use the fixed startup script
CMD ["/home/emacs-user/startup.sh"]

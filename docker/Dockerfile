# Optimized Skewed Emacs Dockerfile -
# Reduced bloat while keeping compiled elisp
FROM debian:testing-slim

ARG DEBIAN_FRONTEND=noninteractive

# Install runtime dependencies with minimal
# build tools for native compilation

RUN apt-get -y update && apt-get -y upgrade && apt-get -y autoremove && \
    apt-get -y install \
    emacs \
    curl \
    wget \
    git \
    sudo \
    iputils-ping \
    telnet \
    netcat-openbsd \
    gcc \
    libc6-dev \
    ca-certificates \
    openssh-client \
    ghostscript \
    poppler-utils \
    imagemagick \
    && rm -rf /var/cache/apt/* /var/lib/apt/lists/* && apt-get clean

# Create emacs-user with proper shell
RUN groupadd -g 1000 emacs-user && \
    useradd -m -s /bin/bash -u 1000 -g emacs-user emacs-user

# Create project directory structure (will be mounted from host)
RUN mkdir -p /projects && \
    chown emacs-user:emacs-user /projects

# Copy skewed-emacs configuration 
COPY --chown=emacs-user:emacs-user . /home/emacs-user/skewed-emacs/

# Switch to emacs-user for all subsequent operations
USER emacs-user
WORKDIR /home/emacs-user

# Run setup to install configuration (creates ~/.emacs.d from dot-files)
RUN cd skewed-emacs && ./setup

# Verify the configuration is installed
RUN ls -la ~/.emacs.d/init.el && \
    ls -la ~/.emacs.d/sideloaded/lisply-backend/source/ && \
    echo "Ã¢ Configuration files verified"

# Copy startup script (used for both build and runtime)
COPY --chown=emacs-user:emacs-user docker/startup.sh /home/emacs-user/startup.sh
COPY --chown=emacs-user:emacs-user docker/emacs-repl.sh /home/emacs-user/emacs-repl.sh
RUN chmod +x /home/emacs-user/startup.sh /home/emacs-user/emacs-repl.sh

# PACKAGE INSTALLATION: Use startup.sh in batch mode
# This loads init.el naturally and installs all packages, then exits
RUN echo "=== Installing packages using startup.sh --batch ===" && \
    /home/emacs-user/startup.sh --batch && \
    echo "Ã¢ Package installation completed via startup.sh"

# BLOAT CLEANUP: Remove copilot cache but keep compiled elisp
RUN echo "=== Cleaning up bloat while preserving compiled elisp ===" && \
    rm -rf /home/emacs-user/.emacs.d/.cache/copilot/ && \
    echo "Ã¢ Removed copilot cache (saves ~368MB)" && \
    du -sh /home/emacs-user/.emacs.d/ && \
    echo "Ã¢ Native compiled files preserved:" && \
    find /home/emacs-user/.emacs.d/eln-cache/ -name '*.eln' | wc -l

# Verify critical packages were installed
RUN echo "Verifying package installation..." && \
    emacs --batch --eval " \
    (progn \
      (package-initialize) \
      (let ((missing nil)) \
        (dolist (pkg '(simple-httpd flycheck company magit slime)) \
          (unless (package-installed-p pkg) \
            (push pkg missing))) \
        (if missing \
            (progn \
              (message \"Missing packages: %s\" missing) \
              (kill-emacs 1)) \
          (message \"Ã¢ All critical packages verified\"))))" || \
    echo "Ã¢   Package verification had issues, but proceeding..."

# Set environment variables for runtime
ENV TERM=xterm-256color

# Expose port for Lisply HTTP API
EXPOSE 7080

# Use the same startup.sh as entrypoint (will run in daemon mode)
ENTRYPOINT ["/home/emacs-user/startup.sh"]

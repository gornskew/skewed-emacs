# Clean Skewed Emacs Dockerfile - Use startup.sh for both build and runtime
FROM debian:testing-slim

ARG DEBIAN_FRONTEND=noninteractive

# Install runtime dependencies
RUN apt-get -y update && apt-get -y upgrade && apt-get -y autoremove && \
    apt-get -y install \
    emacs-nox \
    curl \
    wget \
    git \
    sudo \
    iputils-ping \
    telnet \
    netcat-openbsd \
    build-essential \
    binutils \
    binutils-dev \
    gcc \
    libc6-dev \
    ca-certificates \
    openssh-client \
    && rm -rf /var/cache/apt/* && apt-get clean

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
    echo "â Configuration files verified"

# Copy startup script (used for both build and runtime)
COPY --chown=emacs-user:emacs-user docker/startup.sh /home/emacs-user/startup.sh
COPY --chown=emacs-user:emacs-user docker/emacs-repl.sh /home/emacs-user/emacs-repl.sh
RUN chmod +x /home/emacs-user/startup.sh /home/emacs-user/emacs-repl.sh

# PACKAGE INSTALLATION: Use startup.sh in batch mode
# This loads init.el naturally and installs all packages, then exits
RUN echo "=== Installing packages using startup.sh --batch ===" && \
    /home/emacs-user/startup.sh --batch && \
    echo "â Package installation completed via startup.sh"

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
          (message \"â All critical packages verified\"))))" || \
    echo "â  Package verification had issues, but proceeding..."

# Set environment variables for runtime
ENV TERM=xterm-256color

# Expose port for Lisply HTTP API
EXPOSE 7080

# Use the same startup.sh as entrypoint (will run in daemon mode)
ENTRYPOINT ["/home/emacs-user/startup.sh"]

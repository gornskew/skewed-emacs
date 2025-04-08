# Dockerfile for Skewed Emacs with Lisply Backend
# Uses the emacs-base image which already has Emacs 30.1 built and installed

# Start with the pre-built Emacs base image
FROM skewed-emacs-base:latest

# Install additional runtime dependencies 
# (base image already has libncurses6, libgnutls30, libxml2, libjansson4, libsqlite3-0, libgpm2, libgccjit0, git)
# Adding build-essential which includes binutils (for 'as') and gcc needed for native compilation
RUN apt-get update && apt-get install -y \
    nodejs \
    npm \
    openssh-server \
    tmux \
    curl \
    procps \
    sudo \
    coreutils \
    build-essential \
    binutils \
    git \
    && rm -rf /var/lib/apt/lists/*

# Configure SSH
RUN mkdir /var/run/sshd && \
    echo 'PermitUserEnvironment yes' >> /etc/ssh/sshd_config && \
    echo 'ClientAliveInterval 30' >> /etc/ssh/sshd_config && \
    echo 'ClientAliveCountMax 10' >> /etc/ssh/sshd_config

# Create emacs-user with UID 1000
RUN useradd -m -s /bin/bash -u 1000 emacs-user && \
    usermod -aG sudo emacs-user && \
    mkdir -p /home/emacs-user/.ssh && \
    touch /home/emacs-user/.ssh/authorized_keys && \
    chown -R emacs-user:emacs-user /home/emacs-user/.ssh && \
    chmod 700 /home/emacs-user/.ssh && \
    chmod 600 /home/emacs-user/.ssh/authorized_keys && \
    echo "emacs-user ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

# Emacs is already installed in the base image - PATH is set in the base image

# Create the necessary directories
WORKDIR /home/emacs-user

# Copy the skewed-emacs repository content
COPY --chown=emacs-user:emacs-user . /skewed-emacs/

# Switch to emacs-user for setup
USER emacs-user
ENV HOME="/home/emacs-user"

# Run the setup script to install the configuration
RUN cd /skewed-emacs && ./setup

RUN cd /home/emacs-user && emacs --batch --load /home/emacs-user/.emacs.d/init.el

# Set environment variable for terminal
ENV TERM=xterm-256color


# Create entrypoint script
USER root
RUN echo '#!/bin/bash\n\
set -e\n\
\n\
# Start SSH daemon\n\
service ssh start\n\
\n\
echo "Skewed Emacs with Lisply backend is starting..."\n\
echo "- Lisply HTTP server will be available on port 7080"\n\
echo "- SSH server available on port 22"\n\
echo "- Connect with: ssh emacs-user@<container-ip>"\n\
echo "- Interactive Emacs Lisp REPL will appear in this terminal"\n\
\n\
# Ensure binaries are available in PATH\n\
export PATH=/usr/bin:/usr/local/bin:$PATH\n\
\n\
# Start a basic bash shell for debugging\n\
echo \"Starting interactive bash session for debugging\"\n\
echo \"See ~/README.debug.md for debugging instructions\"\n\
echo \"--------------------------------------------\"\n\
echo \"Run 'emacs' manually to test initialization\"\n\
echo \"--------------------------------------------\"\n\
exec sudo -u emacs-user bash -l\n\
' > /entrypoint.sh && chmod +x /entrypoint.sh

USER emacs-user
# Entry point
ENTRYPOINT ["/entrypoint.sh"]
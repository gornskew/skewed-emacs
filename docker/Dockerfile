# Skewed Emacs Dockerfile -
# Using Debian testing for Emacs 30.1 support.
FROM debian:testing-slim

ARG DEBIAN_FRONTEND=noninteractive

# Install runtime dependencies including GUI Emacs
RUN apt-get -y update && apt-get -y install --no-install-recommends \
    emacs-gtk \
    ncurses-term \
    curl \
    wget \
    git \
    ca-certificates \
    openssh-client \
    ghostscript \
    poppler-utils \
    imagemagick \
    libpng-dev \
    libpoppler-glib8 \
    unzip \
    fontconfig && \
    # Install Nerd Fonts with debugging
    mkdir -p /usr/share/fonts/truetype/nerd-fonts && \
    curl -fsSL https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/NerdFontsSymbolsOnly.zip -o /tmp/NerdFontsSymbolsOnly.zip && \
    ls -l /tmp/NerdFontsSymbolsOnly.zip && \
    unzip -v /tmp/NerdFontsSymbolsOnly.zip && \
    unzip /tmp/NerdFontsSymbolsOnly.zip -d /usr/share/fonts/truetype/nerd-fonts && \
    rm /tmp/NerdFontsSymbolsOnly.zip && \
    ls -l /usr/share/fonts/truetype/nerd-fonts && \
    fc-cache -fv && \
    fc-list | grep "Symbols Nerd Font" || echo "ERROR: Nerd Fonts not found in fc-list" && \
    # Install Docker CLI
    curl -fsSL https://download.docker.com/linux/static/stable/x86_64/docker-24.0.7.tgz | tar xz --strip-components=1 -C /usr/local/bin docker/docker && \
    chmod +x /usr/local/bin/docker && \
    # Create user and directories
    groupadd -g 1000 emacs-user && \
    useradd -m -s /bin/bash -u 1000 -g emacs-user emacs-user && \
    mkdir -p /projects && chown emacs-user:emacs-user /projects && \
    ln -s /home/emacs-user/skewed-emacs/docker/copilot-language-server /usr/local/bin/copilot-language-server && \
    ln -s /projects /home/emacs-user/projects && \
    # Clean up
    rm -rf /var/cache/apt/* /var/lib/apt/lists/* && apt-get clean

# Copy skewed-emacs configuration
COPY --chown=emacs-user:emacs-user . /home/emacs-user/skewed-emacs/

# Switch to emacs-user
USER emacs-user
WORKDIR /home/emacs-user

# Run setup and initialize Emacs
RUN cd skewed-emacs && ./setup && \
    ls -la ~/.emacs.d/init.el && \
    ls -la ~/.emacs.d/sideloaded/lisply-backend/source/ && \
    echo "Configuration files verified" && \
    # Initialize Emacs and install nerd-icons fonts
    START_HTTP=false SKEWED_EMACS_DOCKER_BUILD=true emacs --batch --load "~/.emacs.d/init.el" && \
    START_HTTP=false SKEWED_EMACS_DOCKER_BUILD=true emacs --batch --load "~/.emacs.d/init.el" --eval "(recompile-all-packages)" && \
    START_HTTP=false SKEWED_EMACS_DOCKER_BUILD=true emacs --batch --load "~/.emacs.d/init.el" --eval "(when (package-installed-p 'nerd-icons) (nerd-icons-install-fonts t))" && \
    fc-cache -fv && \
    fc-list | grep "Symbols Nerd Font" || echo "ERROR: Nerd Fonts not found in fc-list after nerd-icons" && \
    START_HTTP=true SKEWED_EMACS_DOCKER_BUILD=true emacs --daemon --load "~/.emacs.d/init.el" && \
    sleep 8 && emacsclient --eval "(load \"~/skewed-emacs/docker/wait-for-async.el\")" && \
    echo "Emacs initialization complete"

# Update .bashrc
RUN echo 'export DOT_BASHRC_LOADED=1' >> /home/emacs-user/.bashrc && \
    echo 'export DOT_PROFILE_LOADED=1' >> /home/emacs-user/.bashrc && \
    echo 'if [ -z "$DOT_BASH_PROFILE_LOADED" ]; then . "$HOME/.bash_profile"; export DOT_BASH_PROFILE_LOADED=1; fi' >> /home/emacs-user/.bashrc

# Copy startup scripts
COPY --chown=emacs-user:emacs-user docker/startup.sh /home/emacs-user/startup.sh
COPY --chown=emacs-user:emacs-user docker/emacs-repl.sh /home/emacs-user/emacs-repl.sh
RUN chmod +x /home/emacs-user/startup.sh /home/emacs-user/emacs-repl.sh

# Set environment variables
ENV TERM=xterm-256color
ENV COLORTERM=truecolor
ENV SKEWED_EMACS_CONTAINER=true
EXPOSE 7080

ENTRYPOINT ["/home/emacs-user/startup.sh"]
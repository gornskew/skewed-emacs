# Optimized Skewed Emacs Dockerfile -
# Using Debian base with GUI Emacs support
FROM debian:testing-slim

ARG DEBIAN_FRONTEND=noninteractive

# Install runtime dependencies including GUI Emacs
# Note: emacs package includes X11 support for graphical emacsclient
RUN apt-get -y update && apt-get -y upgrade && apt-get -y autoremove && \
    apt-get -y install \
    emacs \
    ncurses-term \
    curl \
    wget \
    git \
    gcc \
    libc6-dev \
    ca-certificates \
    openssh-client \
    ghostscript \
    poppler-utils \
    imagemagick \
    automake \
    libpng-dev \
    libpoppler-dev \
    libpoppler-glib-dev \
    libz-dev \
    make \
    pkg-config \
    dpkg-dev \
    fonts-dejavu \
    && rm -rf /var/cache/apt/* /var/lib/apt/lists/* && apt-get clean \
    && curl -fsSL \
    https://download.docker.com/linux/static/stable/x86_64/docker-24.0.7.tgz \
    | tar xz --strip-components=1 -C /usr/local/bin \
    docker/docker \
    && chmod +x /usr/local/bin/docker \
    && groupadd -g 1000 emacs-user  \
    && useradd -m -s /bin/bash -u 1000 -g emacs-user emacs-user \
    && mkdir -p /projects \
    && chown emacs-user:emacs-user /projects \
    && ln -s \
    /home/emacs-user/skewed-emacs/docker/copilot-language-server \
    /usr/local/bin/copilot-language-server && \
    fc-cache -fv && \ 
    echo "deb-src http://deb.debian.org/debian testing main" >> /etc/apt/sources.list && \
    echo "deb-src http://deb.debian.org/debian-security testing-security main" >> /etc/apt/sources.list && \
    apt-get update && \
    mkdir -p /usr/src/emacs && \
    cd /usr/src/emacs && \
    apt-get source emacs && \
    chown -R emacs-user:emacs-user /usr/src/emacs && \
    rm -rf /var/cache/apt/* /var/lib/apt/lists/* && apt-get clean

# Copy skewed-emacs configuration 
COPY --chown=emacs-user:emacs-user . \
    /home/emacs-user/skewed-emacs/

# Switch to emacs-user for all subsequent operations
USER emacs-user
WORKDIR /home/emacs-user

# Run setup to install configuration (links ~/.emacs.d to ~/skewed-emacs/dot-files/emacs.d/)
RUN cd skewed-emacs && ./setup \
    && ls -la ~/.emacs.d/init.el \
    && ls -la ~/.emacs.d/sideloaded/lisply-backend/source/ \
    && echo "Configuration files verified"

# Copy startup script (used at runtime as entry point)
COPY --chown=emacs-user:emacs-user docker/startup.sh /home/emacs-user/startup.sh
COPY --chown=emacs-user:emacs-user docker/emacs-repl.sh \
      /home/emacs-user/emacs-repl.sh
RUN chmod +x /home/emacs-user/startup.sh /home/emacs-user/emacs-repl.sh


RUN START_HTTP=false emacs --batch --load "~/.emacs.d/init.el" \
    && \
    START_HTTP=false emacs --batch --load "~/.emacs.d/init.el" --eval "(recompile-all-packages)" \
    && \
    START_HTTP=true emacs --daemon --load "~/.emacs.d/init.el" \
    && sleep 8 && emacsclient --eval "(load \"~/skewed-emacs/docker/wait-for-async.el\")"
#
# FLAG -- 60 just for proof of concept.
# Instead of sleep 60, wait for async jobs to be done. 
#



RUN cat << EOF >> /home/emacs-user/.bashrc

export DOT_BASHRC_LOADED=1
export DOT_PROFILE_LOADED=1

if [ -z "$DOT_BASH_PROFILE_LOADED" ]; then
  . "$HOME/.bash_profile"
  export DOT_BASH_PROFILE_LOADED=1
fi

EOF


# Set environment variables for runtime
ENV TERM=xterm-256color
ENV COLORTERM=truecolor
ENV SKEWED_EMACS_CONTAINER=true
# Expose port for Lisply HTTP API
EXPOSE 7080

ENTRYPOINT ["/home/emacs-user/startup.sh"]
# CMD ["/bin/bash"]


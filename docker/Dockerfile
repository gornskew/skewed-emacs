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
    /usr/local/bin/copilot-language-server

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

# Copy startup script (used for both build and runtime)
COPY --chown=emacs-user:emacs-user docker/startup.sh /home/emacs-user/startup.sh
COPY --chown=emacs-user:emacs-user docker/emacs-repl.sh \
      /home/emacs-user/emacs-repl.sh
RUN chmod +x /home/emacs-user/startup.sh /home/emacs-user/emacs-repl.sh


# Install packages and precompile .eln files
RUN echo "=== Installing packages and precompiling .eln files ===" \
    && /home/emacs-user/startup.sh --batch \
    && echo "=== Precompiling all .el files in ~/.emacs.d ===" \
    && emacs --batch \
       -q \
       --eval "(setq native-comp-jit-compilation nil)" \
       --eval "(setq native-comp-deferred-compilation nil)" \
       --eval "(setq native-comp-async-jobs-number 5)" \
       --eval "(require 'package)" \
       --eval "(add-to-list 'package-archives '(\"gnu\" . \"https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/\"))" \
       --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/\"))" \
       --eval "(package-initialize)" \
       --eval "(native-compile-async (list (locate-user-emacs-file \"elpa\") \"~/.emacs.d/sideloaded\" \"~/.emacs.d/etc\") t)" \
    && echo "=== Native compilation completed ==="

RUN cat << EOF >> /home/emacs-user/.bashrc

export DOT_BASHRC_LOADED=1
export DOT_PROFILE_LOADED=1

if [ -z "$DOT_BASH_PROFILE_LOADED" ]; then
  . "$HOME/.bash_profile"
  export DOT_BASH_PROFILE_LOADED=1
fi

echo 
echo "Please use \"claudly\" to launch Claude Code with MCP support."
echo 
EOF

# Set environment variables for runtime
ENV TERM=xterm-256color
ENV COLORTERM=truecolor

# Expose port for Lisply HTTP API
EXPOSE 7080

# Use the same startup.sh as entrypoint (will run in daemon mode)
ENTRYPOINT ["/home/emacs-user/startup.sh"]
#ENTRYPOINT ["/bin/bash"]

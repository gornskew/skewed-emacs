FROM silex/emacs:28.2-alpine@sha256:b7e60f8debb85dc49fca5ebc241a4cd97a0d1d1cd7fdceae9c8de87d76b28e97

# Add metadata labels
LABEL maintainer="Genworks" \
      description="Emacs Lisply Backend for Model Context Protocol" \
      version="1.0" \
      org.opencontainers.image.source="https://github.com/gendl/emacs-lisply-backend"

# Install necessary packages with specific versions
RUN apk add --no-cache \
    git=2.36.5-r0 \
    curl=8.0.1-r0 \
    wget=1.21.3-r0 \
    openssh-client=9.0_p1-r2 \
    bash=5.1.16-r2 \
    tzdata=2022a-r0 \
    coreutils=9.1-r0 \
    findutils=4.9.0-r0 \
    && rm -rf /var/cache/apk/*

# Create a non-root user with specific UID/GID for better security
RUN addgroup -g 1000 emacs && \
    adduser -D -h /home/emacs -s /bin/bash -G emacs -u 1000 emacs

# Set up SSH for Git (only used during build)
RUN mkdir -p /home/emacs/.ssh && \
    chmod 700 /home/emacs/.ssh

# Accept SSH key as build argument
ARG SSH_PRIVATE_KEY

# Add the SSH key if provided
RUN if [ -n "$SSH_PRIVATE_KEY" ]; then \
    echo "$SSH_PRIVATE_KEY" > /home/emacs/.ssh/id_rsa && \
    chmod 600 /home/emacs/.ssh/id_rsa && \
    echo "Host gitlab.genworks.com\n\tStrictHostKeyChecking no\n\tIdentityFile /home/emacs/.ssh/id_rsa\n" > /home/emacs/.ssh/config && \
    chmod 600 /home/emacs/.ssh/config && \
    chown -R emacs:emacs /home/emacs/.ssh; \
    fi

# Switch to user
USER emacs
WORKDIR /home/emacs

# Copy the skewed-emacs repository into the container
COPY --chown=emacs:emacs . /home/emacs/skewed-emacs

# Run the setup script to configure dotfiles
RUN cd /home/emacs/skewed-emacs && \
    chmod +x ./setup && \
    ./setup && \
    echo "Skewed Emacs configuration installed successfully."

# Copy Lisply files
RUN mkdir -p /home/emacs/.emacs.d/lisp
COPY --chown=emacs:emacs source/lisply-http-setup.el /home/emacs/.emacs.d/lisp/
COPY --chown=emacs:emacs source/lisply-endpoints.el /home/emacs/.emacs.d/lisp/
COPY --chown=emacs:emacs source/emacs-lisply-backend.el /home/emacs/.emacs.d/lisp/

# Add Lisply backend configuration to init.el
RUN echo '\n;; Lisply Backend configuration\n(add-to-list '\''load-path "~/.emacs.d/lisp")\n(require '\''simple-httpd)\n(require '\''emacs-lisply-backend)\n(emacs-lisply-start)' >> /home/emacs/.emacs.d/init.el

# Launch Emacs once to bootstrap package installation through init.el
RUN emacs --batch --eval "(require 'package)" \
          --eval "(package-initialize)" \
          --eval "(load-file \"~/.emacs.d/init.el\")" \
          --eval "(message \"Package installation complete\")"

# Clean up SSH keys after build (security best practice)
USER root
RUN rm -rf /home/emacs/.ssh
USER emacs

# Expose the HTTP port
EXPOSE 7080

# Start Emacs in daemon mode
CMD ["emacs", "--daemon"]

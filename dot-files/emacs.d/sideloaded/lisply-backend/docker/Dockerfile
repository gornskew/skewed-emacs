FROM silex/emacs:28.2-alpine

# Install necessary packages
RUN apk add --no-cache \
    git \
    curl \
    wget \
    openssh-client \
    bash \
    tzdata \
    && rm -rf /var/cache/apk/*

# Create a non-root user
RUN adduser -D -h /home/emacs emacs

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

# Clone config repository and set up environment (during build only)
RUN if [ -n "$SSH_PRIVATE_KEY" ]; then \
    git clone git@gitlab.genworks.com/dnd/config.git /home/emacs/.config && \
    chmod +x /home/emacs/.config/setup && \
    /home/emacs/.config/setup; \
    fi

# Copy Lisply files
RUN mkdir -p /home/emacs/.emacs.d/lisp
COPY --chown=emacs:emacs source/lisply-http-setup.el /home/emacs/.emacs.d/lisp/
COPY --chown=emacs:emacs source/lisply-endpoints.el /home/emacs/.emacs.d/lisp/
COPY --chown=emacs:emacs source/emacs-lisply-backend.el /home/emacs/.emacs.d/lisp/

# Copy the package installation script
COPY --chown=emacs:emacs docker/install-packages.el /home/emacs/.emacs.d/lisp/
RUN emacs --batch -l /home/emacs/.emacs.d/lisp/install-packages.el

# Add Lisply backend configuration to init.el
RUN echo '\n;; Lisply Backend configuration\n(add-to-list '\''load-path "~/.emacs.d/lisp")\n(require '\''simple-httpd)\n(require '\''emacs-lisply-backend)\n(emacs-lisply-start)' >> /home/emacs/.emacs.d/init.el

# Clean up SSH keys after build (security best practice)
USER root
RUN rm -rf /home/emacs/.ssh
USER emacs

# Expose the HTTP port
EXPOSE 7080

# Start Emacs in daemon mode
CMD ["emacs", "--daemon"]

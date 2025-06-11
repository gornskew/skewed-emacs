# Base Dockerfile for Emacs 30.1
# This builds Emacs from source and creates a reusable base image

# Build stage
FROM debian:latest AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    autoconf \
    automake \
    texinfo \
    git \
    libtool \
    libncurses-dev \
    libgnutls28-dev \
    libxml2-dev \
    libjansson-dev \
    libsqlite3-dev \
    libgpm-dev \
    libgccjit-12-dev \
    gcc-12 \
    g++-12 \
    wget \
    ca-certificates \
    zlib1g-dev \
    libz-dev \
    pkg-config \
    gnutls-bin \
    gnutls-dev \
    libgnutls30 \
    && rm -rf /var/lib/apt/lists/*

# Set the working directory
WORKDIR /tmp

# Clone the Emacs repository
RUN git clone --depth 1 --branch emacs-30.1 https://git.savannah.gnu.org/git/emacs.git

# Build Emacs with native compilation but without GUI
WORKDIR /tmp/emacs
RUN ./autogen.sh && \
    ./configure --with-native-compilation=aot \
                --with-json \
                --with-modules \
                --with-gnutls=ifavailable \
                --without-x \
                --without-sound \
                --without-xpm \
                --without-jpeg \
                --without-tiff \
                --without-gif \
                --without-png \
                --without-rsvg \
                --without-imagemagick \
                --with-zlib \
    && make -j$(nproc) \
    && make install prefix=/usr/local

# Runtime stage
FROM debian:latest

# Install runtime dependencies including assembler for native compilation
RUN apt-get update && apt-get install -y \
    libncurses6 \
    libgnutls30 \
    libxml2 \
    libjansson4 \
    libsqlite3-0 \
    libgpm2 \
    libgccjit0 \
    binutils \
    build-essential \
    git sudo \
    && rm -rf /var/lib/apt/lists/*

# Copy Emacs from the builder stage - use usr/local to preserve all files
COPY --from=builder /usr/local /usr/local


# Create emacs-user with UID 1000
RUN useradd -m -s /bin/bash -u 1000 emacs-user && \
    usermod -aG sudo emacs-user

# Copy the skewed-emacs repository content with our user configuration
COPY --chown=emacs-user:emacs-user . /home/emacs-user/skewed-emacs/

USER emacs-user
ENV HOME="/home/emacs-user"
WORKDIR /home/emacs-user

# Run the setup script to install the configuration
RUN cd skewed-emacs && ./setup


# Set environment variable for terminal
ENV TERM=xterm-256color


CMD /bin/bash
# Base Dockerfile for Emacs 30.1
# This builds Emacs from source and creates a reusable base image

# Build stage
FROM ubuntu:latest AS builder

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
FROM ubuntu:latest

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
    gcc-12 \
    git \
    && rm -rf /var/lib/apt/lists/*

# Copy Emacs from the builder stage - use usr/local to preserve all files
COPY --from=builder /usr/local /usr/local
ENV PATH="/usr/local/bin:${PATH}"
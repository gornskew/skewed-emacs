# Skewed Emacs — Additive Multi-Stage Build with Clean Layering
# Build tree:
#   common-runtime-base (ttyd, Node.js, lisply-mcp) - shared foundation
#   ├── runtime-base (+ emacs-nox)        → Tag: lite
#   │   └── runtime-aituis                → Tags: -aituis, -tui
#   └── runtime-gui-base (+ emacs-gtk + fonts)
#       └── runtime-gui                   → Tag: -gui
#           └── runtime-gui-aituis        → Tags: -gui-aituis, full, latest

FROM debian:stable-slim AS builder

ARG DEBIAN_FRONTEND=noninteractive
ARG NODE_MAJOR=23
ARG GIT_BRANCH=master

SHELL ["/bin/bash", "-lc"]

# ============================================================
# Builder: Build ALL artifacts (GUI + AI CLIs)
# ============================================================

# Install ALL build dependencies (both GUI and non-GUI)
RUN set -euxo pipefail; \
   apt-get update; \
   apt-get install -y --no-install-recommends \
     emacs-gtk \
     ncurses-term \
     curl wget git ca-certificates openssh-client \
     ghostscript poppler-utils \
     unzip gnupg \
     fontconfig \
     fonts-noto fonts-noto-color-emoji fonts-noto-mono \
     fonts-font-awesome fonts-symbola fonts-powerline \
     automake autoconf make gcc g++ pkg-config \
     libpng-dev \
     libpoppler-glib-dev libpoppler-private-dev \
     cmake libtool \
     libvterm-dev \
   ; \
   rm -rf /var/lib/apt/lists/*

# Install Nerd Fonts (for GUI builds)
RUN set -euxo pipefail; \
   mkdir -p /usr/share/fonts/truetype/nerd-fonts; \
   curl -fsSL https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/NerdFontsSymbolsOnly.zip \
     -o /tmp/NerdFontsSymbolsOnly.zip; \
   unzip -q /tmp/NerdFontsSymbolsOnly.zip -d /usr/share/fonts/truetype/nerd-fonts; \
   rm -f /tmp/NerdFontsSymbolsOnly.zip; \
   fc-cache -fv

# Create user
RUN set -euxo pipefail; \
   groupadd -g 1000 emacs-user; \
   useradd -m -s /bin/bash -u 1000 -g emacs-user emacs-user; \
   mkdir -p /projects; \
   chown emacs-user:emacs-user /projects; \
   ln -s /projects /home/emacs-user/projects

# Copy repo
COPY --chown=emacs-user:emacs-user . /home/emacs-user/skewed-emacs/

USER emacs-user
WORKDIR /home/emacs-user

# Setup skewed-emacs config
RUN set -euxo pipefail; \
   cd skewed-emacs; \
   SKEWED_EMACS_DOCKER_BUILD=true ./setup

# Initial package load
RUN set -euxo pipefail; \
   START_HTTP=false SKEWED_EMACS_DOCKER_BUILD=true emacs --batch \
     --load "~/.emacs.d/init.el"

# Build vterm (needed for all variants)
RUN set -euxo pipefail; \
   START_HTTP=false SKEWED_EMACS_DOCKER_BUILD=true emacs --batch \
     --load "~/.emacs.d/init.el" \
     --eval "(vterm-module-compile)"

# Build pdf-tools (for GUI variants)
RUN set -euxo pipefail; \
   PDFDIR="$(ls -d /home/emacs-user/.emacs.d/elpa/pdf-tools-* 2>/dev/null | head -n1)"; \
   if [ -n "$PDFDIR" ]; then \
     cd "$PDFDIR"; \
     ./build/server/autobuild -i "$PDFDIR"; \
     cd /home/emacs-user; \
     START_HTTP=false SKEWED_EMACS_DOCKER_BUILD=true emacs --batch \
       --load "~/.emacs.d/init.el" \
       --eval "(require 'pdf-tools)"; \
   else \
     echo "pdf-tools not found, skipping build"; \
   fi

# Recompile packages
RUN set -euxo pipefail; \
   START_HTTP=false SKEWED_EMACS_DOCKER_BUILD=true emacs --batch \
     --load "~/.emacs.d/init.el" \
     --eval "(recompile-all-packages)"

# Install nerd-icons fonts (for GUI variants)
RUN set -euxo pipefail; \
   START_HTTP=false SKEWED_EMACS_DOCKER_BUILD=true emacs --batch \
     --load "~/.emacs.d/init.el" \
     --eval "(when (package-installed-p 'nerd-icons) (nerd-icons-install-fonts t))"; \
   fc-cache -fv

USER root

# Install Node.js (needed for all variants - lisply-mcp)
RUN set -euxo pipefail; \
   mkdir -p /etc/apt/keyrings; \
   curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key \
     | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg; \
   echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_${NODE_MAJOR}.x nodistro main" \
     > /etc/apt/sources.list.d/nodesource.list; \
   apt-get update; \
   apt-get install -y --no-install-recommends nodejs; \
   rm -rf /var/lib/apt/lists/*

RUN set -euxo pipefail; \
   ln -sf /home/emacs-user/skewed-emacs/docker/copilot-language-server /usr/local/bin/copilot-language-server

USER emacs-user

# Install lisply-mcp (needed for all variants)
RUN set -euxo pipefail; \
   git clone --depth 1 --branch "${GIT_BRANCH}" https://github.com/gornskew/lisply-mcp.git; \
   cd lisply-mcp/scripts; npm ci --omit=dev; \
   cd ../scripts; npm ci --omit=dev; \
   npm cache clean --force || true

ENV PATH="/home/emacs-user/.local/bin:${PATH}"

# Install AI TUI CLIs (for -aituis variants)
RUN set -euxo pipefail; \
   mkdir -p ~/.claude/local ~/.local/bin; \
   npm install --omit=dev @anthropic-ai/claude-code --prefix ~/.claude/local; \
   ln -sf ~/.claude/local/node_modules/.bin/claude ~/.local/bin/claude; \
   npm cache clean --force || true

RUN set -euxo pipefail; \
   mkdir -p ~/.codex/local; \
   npm install --omit=dev @openai/codex --prefix ~/.codex/local; \
   ln -sf ~/.codex/local/node_modules/.bin/codex ~/.local/bin/codex; \
   npm cache clean --force || true

RUN set -euxo pipefail; \
   mkdir -p ~/.gemini/local; \
   npm install --omit=dev @google/gemini-cli --prefix ~/.gemini/local; \
   ln -sf ~/.gemini/local/node_modules/.bin/gemini ~/.local/bin/gemini; \
   npm cache clean --force || true

# Start daemon to settle async init
RUN set -euxo pipefail; \
   START_HTTP=true SKEWED_EMACS_DOCKER_BUILD=true emacs --daemon --load "~/.emacs.d/init.el"; \
   sleep 8; \
   emacsclient --eval "(load \"~/skewed-emacs/docker/wait-for-async.el\")"

# Cleanup builder caches
RUN set -euxo pipefail; \
   rm -rf ~/.cache /tmp/*; \
   rm -rf ~/.emacs.d/elpa/*/.git 2>/dev/null || true


# ============================================================
# Common Runtime Base: Shared foundation (ttyd, Node.js, lisply-mcp)
# This layer is inherited by BOTH GUI and non-GUI branches
# ============================================================
FROM debian:stable-slim AS common-runtime-base

ARG DEBIAN_FRONTEND=noninteractive
ARG NODE_MAJOR=23

SHELL ["/bin/bash", "-lc"]

# Install minimal common runtime deps (NO Emacs yet)
RUN set -euxo pipefail; \
   apt-get update; \
   apt-get install -y --no-install-recommends \
     ncurses-term \
     curl wget git ca-certificates openssh-client \
     libvterm-dev \
     gnupg \
   ; \
   rm -rf /var/lib/apt/lists/*

# Install Node.js (needed for lisply-mcp in all variants)
RUN set -euxo pipefail; \
   mkdir -p /etc/apt/keyrings; \
   curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key \
     | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg; \
   echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_${NODE_MAJOR}.x nodistro main" \
     > /etc/apt/sources.list.d/nodesource.list; \
   apt-get update; \
   apt-get install -y --no-install-recommends nodejs; \
   rm -rf /var/lib/apt/lists/*

# Install ttyd (web terminal - needed for all variants)
RUN set -euxo pipefail; \
   wget -O /usr/local/bin/ttyd https://github.com/tsl0922/ttyd/releases/download/1.7.7/ttyd.x86_64; \
   chmod +x /usr/local/bin/ttyd; \
   ln -sf /usr/local/bin/ttyd /usr/local/bin/webterm

# Create user
RUN set -euxo pipefail; \
   groupadd -g 1000 emacs-user; \
   useradd -m -s /bin/bash -u 1000 -g emacs-user emacs-user; \
   mkdir -p /projects; \
   chown emacs-user:emacs-user /projects; \
   ln -s /projects /home/emacs-user/projects

# Copy common artifacts from builder (Emacs config, lisply-mcp, vterm)
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/skewed-emacs /home/emacs-user/skewed-emacs
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/lisply-mcp /home/emacs-user/lisply-mcp
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/.emacs.d /home/emacs-user/.emacs.d
COPY --from=builder /usr/local/bin/copilot-language-server /usr/local/bin/copilot-language-server

USER emacs-user
WORKDIR /home/emacs-user

ENV TERM=xterm-256color
ENV COLORTERM=truecolor
ENV SKEWED_EMACS_CONTAINER=true
ENV PATH="/home/emacs-user/.local/bin:${PATH}"

RUN set -euxo pipefail; \
   /home/emacs-user/skewed-emacs/setup
   

# ============================================================
# Runtime Base: Common base + emacs-nox (TUI only)
# Tagged as: {branch}-lite
# ============================================================
FROM common-runtime-base AS runtime-base

USER root

# Install emacs-nox (terminal-only Emacs)
RUN set -euxo pipefail; \
   apt-get update; \
   apt-get install -y --no-install-recommends emacs-nox; \
   rm -rf /var/lib/apt/lists/*

USER emacs-user

# Regenerate configs
RUN set -euxo pipefail; \
   cd ~/skewed-emacs; \
   SKEWED_WRITE_SKEWED_SEARCH_CONFIG=true emacs --batch -l generate-configs.el -f skewed-generate-all-configs

ARG SKEWED_BUILD_SKEWED_INDEX=false
RUN set -euxo pipefail; \
   if [ "${SKEWED_BUILD_SKEWED_INDEX}" = "true" ]; then \
     emacs --batch \
       --eval "(add-to-list 'load-path \"/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/source\")" \
       --eval "(setq emacs-lisply-skewed-search-config-path \"/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/skewed-search-config.json\")" \
       --eval "(setq emacs-lisply-skewed-search-index-path \"/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/skewed-search-index.json\")" \
       -l lisply-skewed-search.el \
       --eval "(emacs-lisply-skewed-search-build-index)"; \
   fi


COPY --chown=emacs-user:emacs-user mcp/entrypoint.sh /usr/local/bin/entrypoint.sh
COPY --chown=emacs-user:emacs-user docker/emacs-repl.sh /home/emacs-user/emacs-repl.sh
RUN chmod +x /usr/local/bin/entrypoint.sh /home/emacs-user/emacs-repl.sh

RUN set -euxo pipefail; \
    /home/emacs-user/skewed-emacs/setup


ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]


# ============================================================
# Runtime AI TUIs: Base + AI CLIs
# Tagged as: {branch}-aituis, {branch}-tui
# ============================================================
FROM runtime-base AS runtime-aituis

# Copy AI CLI installations from builder
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/.claude /home/emacs-user/.claude
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/.codex /home/emacs-user/.codex
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/.gemini /home/emacs-user/.gemini
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/.local/bin /home/emacs-user/.local/bin


# ============================================================
# Runtime GUI Base: Common base + emacs-gtk + fonts
# This is the GUI equivalent of runtime-base
# ============================================================
FROM common-runtime-base AS runtime-gui-base

USER root

# Install GUI-specific packages (emacs-gtk, fonts, PDF support)
RUN set -euxo pipefail; \
   apt-get update; \
   apt-get install -y --no-install-recommends \
     emacs-gtk \
     ghostscript poppler-utils libpoppler-glib8 \
     fontconfig \
     fonts-noto fonts-noto-color-emoji fonts-noto-mono \
     fonts-font-awesome fonts-symbola fonts-powerline \
   ; \
   rm -rf /var/lib/apt/lists/*

# Copy GUI-specific artifacts (Nerd Fonts)
COPY --from=builder /usr/share/fonts/truetype/nerd-fonts /usr/share/fonts/truetype/nerd-fonts
RUN fc-cache -fv

USER emacs-user

# Regenerate configs
RUN set -euxo pipefail; \
   cd ~/skewed-emacs; \
   SKEWED_WRITE_SKEWED_SEARCH_CONFIG=true emacs --batch -l generate-configs.el -f skewed-generate-all-configs

ARG SKEWED_BUILD_SKEWED_INDEX=false
RUN set -euxo pipefail; \
   if [ "${SKEWED_BUILD_SKEWED_INDEX}" = "true" ]; then \
     emacs --batch \
       --eval "(add-to-list 'load-path \"/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/source\")" \
       --eval "(setq emacs-lisply-skewed-search-config-path \"/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/skewed-search-config.json\")" \
       --eval "(setq emacs-lisply-skewed-search-index-path \"/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/skewed-search-index.json\")" \
       -l lisply-skewed-search.el \
       --eval "(emacs-lisply-skewed-search-build-index)"; \
   fi


COPY --chown=emacs-user:emacs-user mcp/entrypoint.sh /usr/local/bin/entrypoint.sh
COPY --chown=emacs-user:emacs-user docker/emacs-repl.sh /home/emacs-user/emacs-repl.sh
RUN chmod +x /usr/local/bin/entrypoint.sh /home/emacs-user/emacs-repl.sh

RUN set -euxo pipefail; \
    /home/emacs-user/skewed-emacs/setup

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]


# ============================================================
# Runtime GUI: Alias for runtime-gui-base
# Tagged as: {branch}-gui
# ============================================================
FROM runtime-gui-base AS runtime-gui


# ============================================================
# Runtime GUI + AI TUIs: Everything
# Tagged as: {branch}-gui-aituis, {branch}-full, {branch}, latest
# ============================================================
FROM runtime-gui-base AS runtime-gui-aituis

# Copy AI CLI installations from builder
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/.claude /home/emacs-user/.claude
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/.codex /home/emacs-user/.codex
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/.gemini /home/emacs-user/.gemini
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/.local/bin /home/emacs-user/.local/bin

# Skewed Emacs — multi-stage, optimized for fast web-terminal iteration
# - builder: heavy Emacs setup, pdf-tools, vterm, package compilation
# - runtime-base: minimal runtime with Node.js and core artifacts
# - runtime-webterm-experiment: fast-rebuilding layer with Gotty/Wetty/ttyd

FROM debian:stable-slim AS builder

ARG DEBIAN_FRONTEND=noninteractive
ARG NODE_MAJOR=23
ARG GIT_BRANCH=master
ARG CODEX_VERSION=0.73.0
ARG INSTALL_GEMINI_VIA_NPM=true

SHELL ["/bin/bash", "-lc"]

# ---- base deps (builder) ----
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

# ---- Nerd Fonts (builder) ----
RUN set -euxo pipefail; \
   mkdir -p /usr/share/fonts/truetype/nerd-fonts; \
   curl -fsSL https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/NerdFontsSymbolsOnly.zip \
     -o /tmp/NerdFontsSymbolsOnly.zip; \
   unzip -q /tmp/NerdFontsSymbolsOnly.zip -d /usr/share/fonts/truetype/nerd-fonts; \
   rm -f /tmp/NerdFontsSymbolsOnly.zip; \
   fc-cache -fv

# ---- user + dirs ----
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

# ---- skewed-emacs setup + pdf-tools/vterm build ----
RUN set -euxo pipefail; \
   cd skewed-emacs; \
   SKEWED_EMACS_DOCKER_BUILD=true ./setup

RUN set -euxo pipefail; \
   START_HTTP=false SKEWED_EMACS_DOCKER_BUILD=true emacs --batch \
     --load "~/.emacs.d/init.el"

RUN set -euxo pipefail; \
   START_HTTP=false SKEWED_EMACS_DOCKER_BUILD=true emacs --batch \
     --load "~/.emacs.d/init.el" \
     --eval "(vterm-module-compile)"

RUN set -euxo pipefail; \
   PDFDIR="$(ls -d /home/emacs-user/.emacs.d/elpa/pdf-tools-* | head -n1)"; \
   cd "$PDFDIR"; \
   ./build/server/autobuild -i "$PDFDIR"

RUN set -euxo pipefail; \
   START_HTTP=false SKEWED_EMACS_DOCKER_BUILD=true emacs --batch \
     --load "~/.emacs.d/init.el" \
     --eval "(require 'pdf-tools)"

RUN set -euxo pipefail; \
   START_HTTP=false SKEWED_EMACS_DOCKER_BUILD=true emacs --batch \
     --load "~/.emacs.d/init.el" \
     --eval "(recompile-all-packages)"; \
   START_HTTP=false SKEWED_EMACS_DOCKER_BUILD=true emacs --batch \
     --load "~/.emacs.d/init.el" \
     --eval "(when (package-installed-p 'nerd-icons) (nerd-icons-install-fonts t))"; \
   fc-cache -fv

USER root
# ---- Node.js (builder) ----
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
# ---- lisply-mcp + AI CLIs ----
RUN set -euxo pipefail; \
   git clone --depth 1 --branch "${GIT_BRANCH}" https://github.com/gornskew/lisply-mcp.git; \
   cd lisply-mcp/scripts; npm ci --omit=dev; \
   cd ../scripts-v2; npm ci --omit=dev; npm cache clean --force || true

ENV PATH="/home/emacs-user/.local/bin:${PATH}"

RUN set -euxo pipefail; \
   mkdir -p ~/.claude/local ~/.local/bin; \
   npm install --omit=dev @anthropic-ai/claude-code --prefix ~/.claude/local; \
   ln -sf ~/.claude/local/node_modules/.bin/claude ~/.local/bin/claude; npm cache clean --force || true

RUN set -euxo pipefail; \
   mkdir -p ~/.codex/local ~/.local/bin; \
   npm install --omit=dev @openai/codex --prefix ~/.codex/local; \
   ln -sf ~/.codex/local/node_modules/.bin/codex ~/.local/bin/codex; npm cache clean --force || true

RUN set -euxo pipefail; \
   if [[ "${INSTALL_GEMINI_VIA_NPM}" == "true" ]]; then \
     mkdir -p ~/.gemini/local; \
     npm install --omit=dev @google/gemini-cli --prefix ~/.gemini/local; \
     ln -sf ~/.gemini/local/node_modules/.bin/gemini ~/.local/bin/gemini; npm cache clean --force || true; \
   fi

# Start daemon to settle async init
RUN set -euxo pipefail; \
   START_HTTP=true SKEWED_EMACS_DOCKER_BUILD=true emacs --daemon --load "~/.emacs.d/init.el"; \
   sleep 8; \
   emacsclient --eval "(load \"~/skewed-emacs/docker/wait-for-async.el\")"

# Cleanup
RUN set -euxo pipefail; \
   rm -rf ~/.cache /tmp/*; \
   rm -rf ~/.emacs.d/elpa/*/.git 2>/dev/null || true


# ============================================================
# Runtime base (shared, minimal)
# ============================================================
FROM debian:stable-slim AS runtime-base

ARG DEBIAN_FRONTEND=noninteractive
ARG NODE_MAJOR=23

SHELL ["/bin/bash", "-lc"]

RUN set -euxo pipefail; \
   apt-get update; \
   apt-get install -y --no-install-recommends \
     emacs-gtk ncurses-term curl wget git ca-certificates openssh-client \
     ghostscript poppler-utils libpoppler-glib8 unzip gnupg fontconfig \
     fonts-noto fonts-noto-color-emoji fonts-noto-mono \
     fonts-font-awesome fonts-symbola fonts-powerline \
     libvterm-dev \
   ; \
   rm -rf /var/lib/apt/lists/*

# Node.js (runtime)
RUN set -euxo pipefail; \
   mkdir -p /etc/apt/keyrings; \
   curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key \
     | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg; \
   echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_${NODE_MAJOR}.x nodistro main" \
     > /etc/apt/sources.list.d/nodesource.list; \
   apt-get update; \
   apt-get install -y --no-install-recommends nodejs; \
   rm -rf /var/lib/apt/lists/*

# User setup
RUN set -euxo pipefail; \
   groupadd -g 1000 emacs-user; \
   useradd -m -s /bin/bash -u 1000 -g emacs-user emacs-user; \
   mkdir -p /projects; \
   chown emacs-user:emacs-user /projects; \
   ln -s /projects /home/emacs-user/projects

# Copy core artifacts
COPY --from=builder /usr/share/fonts/truetype/nerd-fonts /usr/share/fonts/truetype/nerd-fonts
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/lisply-mcp /home/emacs-user/lisply-mcp
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/.local /home/emacs-user/.local
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/.gemini /home/emacs-user/.gemini
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/.claude /home/emacs-user/.claude
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/.codex /home/emacs-user/.codex

RUN set -euxo pipefail; \
   ln -sf /home/emacs-user/skewed-emacs/docker/copilot-language-server /usr/local/bin/copilot-language-server; \
   fc-cache -fv


COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/skewed-emacs /home/emacs-user/skewed-emacs
COPY --chown=emacs-user:emacs-user mcp/entrypoint.sh /usr/local/bin/entrypoint.sh
COPY --chown=emacs-user:emacs-user docker/emacs-repl.sh /home/emacs-user/emacs-repl.sh
RUN chmod +x /usr/local/bin/entrypoint.sh /home/emacs-user/emacs-repl.sh

USER emacs-user
WORKDIR /home/emacs-user

ENV TERM=xterm-256color
ENV COLORTERM=truecolor
ENV SKEWED_EMACS_CONTAINER=true
ENV PATH="/home/emacs-user/.local/bin:${PATH}"

RUN set -euxo pipefail; \
   ./skewed-emacs/setup


# ============================================================
# Web terminal experiment layer (fast to rebuild)
# ============================================================
FROM runtime-base AS runtime-webterm-experiment

USER root

RUN set -euxo pipefail; \
   apt-get update; \
   apt-get install -y --no-install-recommends golang-go; \
   \
   # Gotty latest (active fork, good xterm.js alternative)
   go install github.com/sorenisanerd/gotty@latest; \
   mv /root/go/bin/gotty /usr/local/bin/gotty-soren; \
   chmod +x /usr/local/bin/gotty-soren; \
   \
   # ttyd (BEST for emoji/double-width support, hterm frontend)
   wget -O /usr/local/bin/ttyd https://github.com/tsl0922/ttyd/releases/download/1.7.7/ttyd.x86_64; \
   chmod +x /usr/local/bin/ttyd; \
   \
   # Cleanup
   apt-get purge -y --auto-remove golang-go; \
   apt-get clean; \
   rm -rf /var/lib/apt/lists/* /root/go

# Default to ttyd (override with WEBTERM env)
RUN ln -sf /usr/local/bin/ttyd /usr/local/bin/webterm


FROM runtime-webterm-experiment

USER emacs-user

COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/skewed-emacs /home/emacs-user/skewed-emacs
WORKDIR /home/emacs-user/skewed-emacs
COPY --chown=emacs-user:emacs-user mcp/entrypoint.sh /usr/local/bin/entrypoint.sh
COPY --chown=emacs-user:emacs-user docker/emacs-repl.sh /home/emacs-user/emacs-repl.sh
WORKDIR /home/emacs-user
RUN chmod +x /usr/local/bin/entrypoint.sh /home/emacs-user/emacs-repl.sh

ENV TERM=xterm-256color
ENV COLORTERM=truecolor
ENV SKEWED_EMACS_CONTAINER=true
ENV PATH="/home/emacs-user/.local/bin:${PATH}"

RUN set -euxo pipefail; \
   ./skewed-emacs/setup

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

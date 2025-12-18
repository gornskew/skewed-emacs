# Skewed Emacs — multi-stage, trimmed
# - builder stage: installs build deps, runs Emacs package installs, builds pdf-tools epdfinfo, installs CLIs
# - runtime stage: only runtime deps + copies built artifacts from builder

FROM debian:stable-slim AS builder

ARG DEBIAN_FRONTEND=noninteractive
ARG NODE_MAJOR=23
ARG GIT_BRANCH=master
# Pin Codex version explicitly for reproducible builds (update as desired)
ARG CODEX_VERSION=0.73.0
# If you later confirm Gemini has stable release asset names, you can flip this
ARG INSTALL_GEMINI_VIA_NPM=true

SHELL ["/bin/bash", "-lc"]

# ---- base deps (builder) ----
# NOTE: includes build toolchain + poppler dev headers for pdf-tools epdfinfo build
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
    # pdf-tools build deps
    automake autoconf make gcc g++ pkg-config \
    libpng-dev \
    libpoppler-glib-dev libpoppler-private-dev \
  ; \
  rm -rf /var/lib/apt/lists/*

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

# Copy skewed-emacs repo into image (builder)
COPY --chown=emacs-user:emacs-user . /home/emacs-user/skewed-emacs/

# keep your symlink behavior
RUN set -euxo pipefail; \
  ln -sf /home/emacs-user/skewed-emacs/docker/copilot-language-server /usr/local/bin/copilot-language-server

USER emacs-user
WORKDIR /home/emacs-user

# ---- lisply-mcp (builder) ----
# Keep it local and runnable from skewed-emacs
RUN set -euxo pipefail; \
  git clone --depth 1 --branch "${GIT_BRANCH}" https://github.com/gornskew/lisply-mcp.git; \
  cd lisply-mcp/scripts; \
  npm ci --omit=dev; \
  npm cache clean --force || true

# ---- AI CLIs ----

ENV PATH="/home/emacs-user/.local/bin:${PATH}"

# ---- Claude Code via npm (avoids native installer side-effects) ----
RUN set -euxo pipefail; \
  mkdir -p ~/.claude/local ~/.local/bin; \
  npm install --omit=dev @anthropic-ai/claude-code --prefix ~/.claude/local; \
  ln -sf ~/.claude/local/node_modules/.bin/claude ~/.local/bin/claude; \
  npm cache clean --force || true; \
  ~/.local/bin/claude --version || true


# OpenAI Codex: install via npm (no GitHub release tarball)
RUN set -euxo pipefail; \
  mkdir -p ~/.codex/local ~/.local/bin; \
  npm install --omit=dev @openai/codex --prefix ~/.codex/local; \
  ln -sf ~/.codex/local/node_modules/.bin/codex ~/.local/bin/codex; \
  npm cache clean --force || true; \
  ~/.local/bin/codex --version || true



# Gemini CLI: keep npm for now (until we pin stable release asset names)
RUN set -euxo pipefail; \
  if [[ "${INSTALL_GEMINI_VIA_NPM}" == "true" ]]; then \
    mkdir -p ~/.gemini/local; \
    npm install --omit=dev @google/gemini-cli --prefix ~/.gemini/local; \
    ln -sf ~/.gemini/local/node_modules/.bin/gemini ~/.local/bin/gemini; \
    npm cache clean --force || true; \
  fi; \
  command -v gemini || true; \
  gemini --version || true

# ---- SSH keys (optional; keeping your behavior) ----
RUN set -euxo pipefail; \
  mkdir -p ~/.ssh; chmod 700 ~/.ssh; \
  ssh-keygen -t ed25519 -f ~/.ssh/id_ed25519 -N "" -C "emacs-user@skewed-emacs"

# ---- skewed-emacs setup + pdf-tools build ----
RUN set -euxo pipefail; \
  cd skewed-emacs; \
  SKEWED_EMACS_DOCKER_BUILD=true ./setup

# Ensure pdf-tools installed + build epdfinfo
RUN set -euxo pipefail; \
  START_HTTP=false SKEWED_EMACS_DOCKER_BUILD=true emacs --batch \
    --load "~/.emacs.d/init.el" \
    --eval "(require 'package)" \
    --eval "(package-initialize)" \
    --eval "(unless (package-installed-p 'pdf-tools) (package-refresh-contents) (package-install 'pdf-tools))"

RUN set -euxo pipefail; \
  PDFDIR="$(ls -d /home/emacs-user/.emacs.d/elpa/pdf-tools-* | head -n1)"; \
  cd "$PDFDIR"; \
  ./build/server/autobuild -i "$PDFDIR"

RUN set -euxo pipefail; \
  START_HTTP=false SKEWED_EMACS_DOCKER_BUILD=true emacs --batch \
    --load "~/.emacs.d/init.el" \
    --eval "(require 'pdf-tools)"

# Recompile packages + nerd-icons fonts
RUN set -euxo pipefail; \
  START_HTTP=false SKEWED_EMACS_DOCKER_BUILD=true emacs --batch \
    --load "~/.emacs.d/init.el" \
    --eval "(recompile-all-packages)"; \
  START_HTTP=false SKEWED_EMACS_DOCKER_BUILD=true emacs --batch \
    --load "~/.emacs.d/init.el" \
    --eval "(when (package-installed-p 'nerd-icons) (nerd-icons-install-fonts t))"; \
  fc-cache -fv

# Start daemon once to let your async init settle (keeping your behavior)
RUN set -euxo pipefail; \
  START_HTTP=true SKEWED_EMACS_DOCKER_BUILD=true emacs --daemon --load "~/.emacs.d/init.el"; \
  sleep 8; \
  emacsclient --eval "(load \"~/skewed-emacs/docker/wait-for-async.el\")"

# Cleanup (builder) — trims caches before copy to runtime
RUN set -euxo pipefail; \
  rm -rf ~/.cache /tmp/*; \
  rm -rf ~/.emacs.d/elpa/*/.git 2>/dev/null || true


# ============================================================
# Runtime image
# ============================================================
FROM debian:stable-slim AS runtime

ARG DEBIAN_FRONTEND=noninteractive
ARG NODE_MAJOR=23

SHELL ["/bin/bash", "-lc"]

# Runtime deps only (NO compiler toolchain, NO poppler dev headers, NO imagemagick, NO docker CLI)
RUN set -euxo pipefail; \
  apt-get update; \
  apt-get install -y --no-install-recommends \
    emacs-gtk \
    ncurses-term \
    curl wget git ca-certificates openssh-client \
    ghostscript poppler-utils \
    libpoppler-glib8 \
    unzip gnupg \
    fontconfig \
    fonts-noto fonts-noto-color-emoji fonts-noto-mono \
    fonts-font-awesome fonts-symbola fonts-powerline \
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

# user + dirs
RUN set -euxo pipefail; \
  groupadd -g 1000 emacs-user; \
  useradd -m -s /bin/bash -u 1000 -g emacs-user emacs-user; \
  mkdir -p /projects; \
  chown emacs-user:emacs-user /projects; \
  ln -s /projects /home/emacs-user/projects

# Copy fonts + caches from builder (nerd fonts + nerd-icons installed fonts)
COPY --from=builder /usr/share/fonts/truetype/nerd-fonts /usr/share/fonts/truetype/nerd-fonts

# Copy user content from builder
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/skewed-emacs /home/emacs-user/skewed-emacs
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/.emacs.d /home/emacs-user/.emacs.d
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/lisply-mcp /home/emacs-user/lisply-mcp
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/.ssh /home/emacs-user/.ssh
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/.local /home/emacs-user/.local
COPY --from=builder --chown=emacs-user:emacs-user /home/emacs-user/.gemini /home/emacs-user/.gemini

# keep your symlink
RUN set -euxo pipefail; \
  ln -sf /home/emacs-user/skewed-emacs/docker/copilot-language-server /usr/local/bin/copilot-language-server; \
  fc-cache -fv

# Startup scripts
COPY --chown=emacs-user:emacs-user docker/startup.sh /home/emacs-user/startup.sh
COPY --chown=emacs-user:emacs-user docker/emacs-repl.sh /home/emacs-user/emacs-repl.sh
RUN chmod +x /home/emacs-user/startup.sh /home/emacs-user/emacs-repl.sh

USER emacs-user
WORKDIR /home/emacs-user

ENV TERM=xterm-256color
ENV COLORTERM=truecolor
ENV SKEWED_EMACS_CONTAINER=true
ENV PATH="/home/emacs-user/.local/bin:${PATH}"

EXPOSE 7080
ENTRYPOINT ["/home/emacs-user/startup.sh"]

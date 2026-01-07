#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
CONTAINER_ROOT="/home/emacs-user/skewed-emacs"
CONTAINER_INDEX_PATH="~/.emacs.d/sideloaded/lisply-backend/skewed-search-index.sexp"
LOAD_PATH="${CONTAINER_ROOT}/dot-files/emacs.d/sideloaded/lisply-backend/source"
PROJECTS_DIR="${PROJECTS_DIR:-/projects}"

IMAGE=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --image=*)
      IMAGE="${1#*=}"
      shift
      ;;
    --image)
      IMAGE="${2:-}"
      shift 2
      ;;
    *)
      echo "Unknown option: $1" >&2
      exit 1
      ;;
  esac
done

if [ -z "${IMAGE}" ]; then
  echo "Missing required --image argument." >&2
  exit 1
fi

if ! command -v docker >/dev/null 2>&1; then
  echo "Missing required executable: docker" >&2
  exit 1
fi

if ! docker image inspect "${IMAGE}" >/dev/null 2>&1; then
  echo "Bootstrap image not found: ${IMAGE}" >&2
  exit 1
fi

echo "Building pre-extracted skewed_search index using ${IMAGE}..."
start_ts="$(date +%s)"

docker run --rm \
  --entrypoint bash \
  -v "${PROJECT_ROOT}:${CONTAINER_ROOT}" \
  -v "${PROJECTS_DIR}:/projects" \
  "${IMAGE}" \
  -lc "set -euo pipefail; emacs --batch --eval \"(add-to-list 'load-path \\\"${LOAD_PATH}\\\")\" --eval \"(setq emacs-lisply-skewed-search-services-path \\\"${CONTAINER_ROOT}/services.sexp\\\")\" --eval \"(setq emacs-lisply-skewed-search-index-path \\\"${CONTAINER_INDEX_PATH}\\\")\" -l lisply-skewed-search-build.el --eval \"(emacs-lisply-skewed-search-build-index)\"; if [ -f \"${HOME}/.emacs.d/sideloaded/lisply-backend/skewed-search-index.sexp\" ]; then ls -lh \"${HOME}/.emacs.d/sideloaded/lisply-backend/skewed-search-index.sexp\"; else echo \"Index not found at ${HOME}/.emacs.d/sideloaded/lisply-backend/skewed-search-index.sexp\" >&2; exit 1; fi"

end_ts="$(date +%s)"
elapsed="$((end_ts - start_ts))"

echo "Index built inside container at: ${CONTAINER_INDEX_PATH}"

echo "Elapsed seconds: ${elapsed}"

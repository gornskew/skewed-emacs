#!/bin/bash
# Run a pure Elisp REPL using stdio

# Run Emacs in batch mode with our Elisp stdio REPL
exec emacs --batch -Q --load "$(dirname "$0")/elisp-stdio-repl.el"
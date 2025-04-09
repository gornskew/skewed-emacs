#!/bin/bash
# Run IELM with stdio interface (no terminal UI)

# Run Emacs directly in batch mode with our IELM stdio script
emacs --batch -Q --load "$(dirname "$0")/ielm-stdio.el"
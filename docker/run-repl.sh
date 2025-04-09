#!/bin/bash
# Run a simple Elisp REPL with stdio interface

# Run Emacs with our simple REPL script
emacs -Q --script "$(dirname "$0")/simple-repl.el"
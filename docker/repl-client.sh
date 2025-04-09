#!/bin/bash
# REPL client to connect to the Emacs daemon

# Read from stdin, process one line at a time to handle complete expressions
while IFS= read -r line; do
  # Send the line to the daemon for evaluation
  result=$(echo "$line" | emacsclient -s repl-daemon -e "(elisp-stdio-repl)")
  
  # Print the result
  echo "$result"
done
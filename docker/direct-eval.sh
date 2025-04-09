#!/bin/bash
# Evaluate Elisp expressions directly using emacsclient

# Simple emacsclient evaluator
while read -r expr; do
  # Send the expression directly to emacsclient for evaluation
  result=$(emacsclient -e "$expr" 2>/dev/null)
  echo "$result"
done
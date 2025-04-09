#!/bin/bash
# Evaluate multi-line Elisp expressions using emacsclient

# Create temporary files for our input/output
TEMP_DIR=$(mktemp -d)
INPUT_FILE="$TEMP_DIR/input.el"
touch "$INPUT_FILE"

# Start Emacs daemon if not already running
if ! emacsclient -e '(+ 1 1)' &>/dev/null; then
  echo "Starting Emacs daemon..."
  emacs --daemon
  sleep 1
fi

# Clean up on exit
cleanup() {
  rm -rf "$TEMP_DIR"
}
trap cleanup EXIT

# Function to evaluate the current buffer
eval_buffer() {
  if [ -s "$INPUT_FILE" ]; then
    # Try to evaluate the buffer as an Elisp expression
    result=$(emacsclient -e "$(cat "$INPUT_FILE")" 2>&1)
    echo "$result"
    > "$INPUT_FILE"  # Clear the input file
  fi
}

echo "Multi-line Elisp REPL (type expressions, use empty line to evaluate)"
echo "Type Ctrl+D to exit"

# Read input line by line
buffer=""
while IFS= read -r line; do
  # Skip empty lines if buffer is empty
  if [[ -z "$line" && -z "$buffer" ]]; then
    continue
  fi
  
  # Empty line with content in buffer means evaluate
  if [[ -z "$line" && -n "$buffer" ]]; then
    echo "$buffer" > "$INPUT_FILE"
    eval_buffer
    buffer=""
  else
    # Add line to buffer
    buffer+="$line"$'\n'
  fi
done

# Evaluate any remaining buffer content
if [[ -n "$buffer" ]]; then
  echo "$buffer" > "$INPUT_FILE"
  eval_buffer
fi

# Clean up
cleanup
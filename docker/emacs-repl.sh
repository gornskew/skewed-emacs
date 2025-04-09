#!/bin/bash
# Run a simple Elisp REPL that reads complete expressions from stdin

# Create named pipes for communication
PIPE_DIR=$(mktemp -d)
INPUT_PIPE="$PIPE_DIR/input_pipe"
OUTPUT_PIPE="$PIPE_DIR/output_pipe"
mkfifo "$INPUT_PIPE"
mkfifo "$OUTPUT_PIPE"

# Start emacs in the background with a cleaner REPL implementation
cat > "$PIPE_DIR/repl.el" << 'EOF'
;; Simple function to read a full s-expression
(defun read-full-sexp ()
  (let ((input "")
        (complete nil))
    (while (not complete)
      (let ((line (read-from-minibuffer "")))
        (setq input (concat input " " line))
        (condition-case nil
            (progn
              (read-from-string input)
              (setq complete t))
          (end-of-file nil))))
    input))

;; REPL loop
(while t
  (condition-case err
      (let* ((input (read))  ; Read directly from stdin
             (form (read-from-string input))
             (result (eval (car form) t)))
        (prin1 result)
        (princ "\n")
        (force-output))
    (end-of-file (kill-emacs 0))
    (error 
     (princ (format "Error: %S\n" err))
     (force-output))))
EOF

# Start Emacs in the background
emacs --batch -Q --load "$PIPE_DIR/repl.el" < "$INPUT_PIPE" > "$OUTPUT_PIPE" 2>/dev/null &
EMACS_PID=$!

# Function to cleanup resources
cleanup() {
  kill $EMACS_PID 2>/dev/null
  rm -rf "$PIPE_DIR"
}
trap cleanup EXIT

# Fancy REPL prompt handler
echo "Emacs REPL (type expressions, press Enter to evaluate)"
echo "Type Ctrl+D to exit"

# Read input from user, accumulate until a complete expression, then send to Emacs
buffer=""
while IFS= read -r line || [[ -n "$line" ]]; do
  buffer+=" $line"
  
  # Try to parse as a complete expression
  if [[ "$line" =~ ^[[:space:]]*$ && -n "$buffer" ]]; then
    # Send the expression to Emacs
    echo "$buffer" > "$INPUT_PIPE"
    buffer=""
    
    # Read the result from Emacs
    if [ -p "$OUTPUT_PIPE" ]; then
      result=$(cat "$OUTPUT_PIPE")
      echo "=> $result"
    fi
  fi
done

# Clean up
cleanup
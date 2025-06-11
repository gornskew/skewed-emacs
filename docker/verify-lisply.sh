#!/bin/bash
# verify-lisply.sh - Verify that lisply backend loads properly

set -e

echo "=== Verifying Lisply Backend ==="

# Test: Try to load lisply backend with minimal configuration
echo "Test: Loading lisply backend components..."
cat > /tmp/test-lisply.el << 'EOF'
(setq debug-on-error t)
;; Disable native compilation completely
(setq native-comp-speed -1)
(setq package-native-compile nil)
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation nil)

(message "Initializing package system...")

;; Initialize package system to access installed packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(message "Loading lisply backend components...")

;; Load the backend files
(condition-case err
    (progn
      (require 'simple-httpd)  ; Load the required dependency first
      (load-file "/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/source/http-setup.el")
      (load-file "/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/source/endpoints.el")
      (load-file "/home/emacs-user/skewed-emacs/dot-files/emacs.d/sideloaded/lisply-backend/source/backend.el")
      (message "Ã¢ Lisply backend files loaded successfully")
      (message "Backend functions available:")
      (message "  - emacs-lisply-start-server: %s" (fboundp 'emacs-lisply-start-server))
      (message "  - emacs-lisply-stop-server: %s" (fboundp 'emacs-lisply-stop-server))
      (message "Ã¢ Verification completed successfully"))
  (error 
    (message "Ã¢ Lisply backend load failed: %s" err)
    (kill-emacs 1)))

(kill-emacs 0)
EOF

# Use --no-init-file to avoid loading the problematic init.el
TERM=dumb emacs --batch --no-init-file --load /tmp/test-lisply.el || {
    echo "Ã¢ Lisply backend verification failed"
    exit 1
}

echo "Ã¢ All verification tests passed"

;;; emacs-lisply-backend.el --- Emacs Model Context Protocol integration

;; Copyright (C) 2025 Genworks

;; Author: Genworks
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (simple-httpd "1.5.1"))
;; Keywords: tools, ai, lisply
;; URL: https://github.com/genworks/emacs-lisply

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides an implementation of the Model Context Protocol
;; (MCP) for Emacs Lisp, allowing AI assistants like Claude to interact
;; with Emacs through a standardized API.
;;
;; Usage:
;; 1. Start the Emacs Lisply backend: M-x emacs-lisply-start
;; 2. Connect Claude Desktop with the appropriate configuration.
;; 3. Now Claude can evaluate Emacs Lisp code, inspect buffers,
;;    and perform other Emacs operations via the MCP protocol.

;;; Code:

(require 'lisply-http-setup)
(require 'lisply-endpoints)

;;;###autoload
(defun emacs-lisply-start ()
  "Start the Emacs Lisply integration."
  (interactive)
  (emacs-lisply-start-server)
  (message "Emacs Lisply integration started"))

;;;###autoload
(defun emacs-lisply-stop ()
  "Stop the Emacs Lisply integration."
  (interactive)
  (emacs-lisply-stop-server)
  (message "Emacs Lisply integration stopped"))

(defun emacs-lisply-version ()
  "Return the Emacs Lisply version."
  (interactive)
  (message "Emacs Lisply version 1.0.0"))

(provide 'emacs-lisply-backend)
;;; emacs-lisply-backend.el ends here
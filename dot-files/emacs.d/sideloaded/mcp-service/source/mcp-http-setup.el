;;; mcp-http-setup.el --- HTTP server setup for MCP protocol

;; Copyright (C) 2025 Genworks

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
;; This file sets up the HTTP server for the Model Context Protocol (MCP)
;; It uses simple-httpd for serving HTTP requests

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'simple-httpd)

(defgroup emacs-mcp nil
  "Emacs MCP Server for LLM integration."
  :group 'tools)

(defcustom emacs-mcp-port 7080
  "Port for the Emacs MCP server."
  :type 'integer
  :group 'emacs-mcp)

(defcustom emacs-mcp-debug t
  "Whether to enable debug logging."
  :type 'boolean
  :group 'emacs-mcp)

(defcustom emacs-mcp-base-path "/mcp"
  "Base path for the MCP endpoints."
  :type 'string
  :group 'emacs-mcp)

(defvar emacs-mcp-server-running nil
  "Flag tracking whether the MCP server is running.")

(defun emacs-mcp-log (format-string &rest args)
  "Log a message with FORMAT-STRING and ARGS if debug is enabled."
  (when emacs-mcp-debug
    (let ((msg (apply 'format format-string args)))
      (message "[EMACS-MCP] %s" msg))))

(defun emacs-mcp-get-request-body ()
  "Get the body of the current HTTP request as a string."
  (let ((length (or (cdr (assoc "content-length" httpd-headers)) "0")))
    (if (> (string-to-number length) 0)
        (with-temp-buffer
          (insert-buffer-substring httpd--current-buffer)
          (goto-char (point-min))
          (search-forward "\r\n\r\n" nil t)
          (buffer-substring-no-properties (point) (point-max)))
      nil)))

(defun emacs-mcp-parse-json-body ()
  "Parse the JSON body from the current HTTP request."
  (let ((body (emacs-mcp-get-request-body)))
    (when (and body (not (string-empty-p body)))
      (condition-case err
          (json-read-from-string body)
        (error
         (emacs-mcp-log "Error parsing JSON body: %s" err)
         nil)))))

(defun emacs-mcp-send-response (data &optional content-type)
  "Send a response with DATA and optional CONTENT-TYPE.
DATA can be a string, in which case it will be sent as-is,
or an object which will be JSON-encoded."
  (let ((content-type (or content-type "application/json")))
    (httpd-send-header content-type 200 
                        '("Access-Control-Allow-Origin" . "*"))
    (princ
     (if (stringp data)
         data
       (json-encode data)))))

;;;###autoload
(defun emacs-mcp-start-server ()
  "Start the Emacs MCP server."
  (interactive)
  (httpd-stop)
  (setq httpd-port emacs-mcp-port)
  (httpd-start)
  (setq emacs-mcp-server-running t)
  (emacs-mcp-log "Emacs MCP server started on port %d" emacs-mcp-port)
  (message "Emacs MCP server started on port %d" emacs-mcp-port))

;;;###autoload
(defun emacs-mcp-stop-server ()
  "Stop the Emacs MCP server."
  (interactive)
  (httpd-stop)
  (setq emacs-mcp-server-running nil)
  (emacs-mcp-log "Emacs MCP server stopped")
  (message "Emacs MCP server stopped"))

;;;###autoload
(defun emacs-mcp-server-status ()
  "Show the status of the Emacs MCP server."
  (interactive)
  (if emacs-mcp-server-running
      (message "Emacs MCP server is running on port %d" emacs-mcp-port)
    (message "Emacs MCP server is not running")))

(provide 'mcp-http-setup)
;;; mcp-http-setup.el ends here

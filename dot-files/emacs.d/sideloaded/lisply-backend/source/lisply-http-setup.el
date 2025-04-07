;;; lisply-http-setup.el --- HTTP server setup for MCP protocol

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

(defgroup emacs-lisply nil
  "Emacs Lisply Server for LLM integration."
  :group 'tools)

(defcustom emacs-lisply-port 7080
  "Port for the Emacs Lisply server."
  :type 'integer
  :group 'emacs-lisply)

(defcustom emacs-lisply-debug nil
  "Whether to enable debug logging. Should be disabled in production."
  :type 'boolean
  :group 'emacs-lisply)

(defcustom emacs-lisply-base-path "/lisply"
  "Base path for the Lisply endpoints."
  :type 'string
  :group 'emacs-lisply)

(defvar emacs-lisply-server-running nil
  "Flag tracking whether the Lisply server is running.")

(defun emacs-lisply-log (format-string &rest args)
  "Log a message with FORMAT-STRING and ARGS if debug is enabled."
  (when emacs-lisply-debug
    (let ((msg (apply 'format format-string args)))
      (message "[EMACS-LISPLY] %s" msg))))

(defun emacs-lisply-get-request-body ()
  "Get the body of the current HTTP request as a string."
  (let ((length (or (cdr (assoc "content-length" httpd-headers)) "0")))
    (if (> (string-to-number length) 0)
        (with-temp-buffer
          (insert-buffer-substring httpd--current-buffer)
          (goto-char (point-min))
          (search-forward "\r\n\r\n" nil t)
          (buffer-substring-no-properties (point) (point-max)))
      nil)))

(defun emacs-lisply-parse-json-body ()
  "Parse the JSON body from the current HTTP request."
  (let ((body (emacs-lisply-get-request-body)))
    (when (and body (not (string-empty-p body)))
      (condition-case err
          (json-read-from-string body)
        (error
         (emacs-lisply-log "Error parsing JSON body: %s" err)
         nil)))))

(defun emacs-lisply-send-response (data &optional content-type)
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
(defun emacs-lisply-start-server ()
  "Start the Emacs Lisply server."
  (interactive)
  (httpd-stop)
  (setq httpd-port emacs-lisply-port)
  (httpd-start)
  (setq emacs-lisply-server-running t)
  (emacs-lisply-log "Emacs Lisply server started on port %d" emacs-lisply-port)
  (message "Emacs Lisply server started on port %d" emacs-lisply-port))

;;;###autoload
(defun emacs-lisply-stop-server ()
  "Stop the Emacs Lisply server."
  (interactive)
  (httpd-stop)
  (setq emacs-lisply-server-running nil)
  (emacs-lisply-log "Emacs Lisply server stopped")
  (message "Emacs Lisply server stopped"))

;;;###autoload
(defun emacs-lisply-server-status ()
  "Show the status of the Emacs Lisply server."
  (interactive)
  (if emacs-lisply-server-running
      (message "Emacs Lisply server is running on port %d" emacs-lisply-port)
    (message "Emacs Lisply server is not running")))

(provide 'lisply-http-setup)
;;; lisply-http-setup.el ends here
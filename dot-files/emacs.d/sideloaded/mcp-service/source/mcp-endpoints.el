;;; mcp-endpoints.el --- MCP protocol endpoints for Emacs integration

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
;; This file defines the MCP protocol endpoints for Emacs,
;; compatible with the Model Context Protocol for LLM integration.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'simple-httpd)
(require 'mcp-http-setup)

;; MCP Tool Definitions

(defvar emacs-mcp-tools
  (vector
   ;; ping_lisp tool (generic name)
   `(("name" . "ping_lisp")
     ("description" . "Check if the Lisp server is available")
     ("inputSchema" . (("type" . "object")
                       ("properties" . ,(make-hash-table))
                       ("required" . []))))

   ;; lisp_eval tool (generic name for consistent with Gendl)
   `(("name" . "lisp_eval")
     ("description" . "Evaluate Emacs Lisp code")
     ("inputSchema" . (("type" . "object")
                       ("properties" . (("code" . (("type" . "string")
                                                 ("description" . "The Emacs Lisp code to evaluate")))
                                       ("package" . (("type" . "string")
                                                   ("description" . "Not used in Emacs Lisp but kept for protocol compatibility")))
                                       ("mode" . (("type" . "string") 
                                                 ("description" . "The mode to use to talk to Emacs, either http (default) or stdio. Stdio will only be respected for local Emacs containers started by the MCP server itself.")))))
                       ("required" . ["code"]))))
   )
  "List of tool definitions for the Emacs MCP server.")

(defun emacs-mcp-generate-tool-description ()
  "Generate a tool description for Claude MCP integration."
  `(("tools" . ,emacs-mcp-tools)))

;; MCP HTTP handlers

(defservlet* mcp/ping-lisp text/plain ()
  "Handle generic ping endpoint for MCP."
  (emacs-mcp-log "Handling ping-lisp request")
  (insert "pong"))

(defservlet* mcp/tools/list application/json ()
  "Handle tools list endpoint for MCP."
  (emacs-mcp-log "Handling tools/list request")
  (emacs-mcp-send-response (emacs-mcp-generate-tool-description)))

(defservlet* mcp/lisp-eval application/json ()
  "Handle Emacs Lisp evaluation endpoint for MCP."
  (emacs-mcp-log "Handling lisp-eval request")
  (let* ((json-input (emacs-mcp-parse-json-body))
         (code (and json-input (cdr (assoc 'code json-input))))
         (stdout-string "")
         result
         error
         success)
    
    (emacs-mcp-log "Attempting to evaluate code: %s" code)
    
    (cond
     ((null json-input)
      (setq error "Malformed or missing input. This endpoint needs a JSON object with {code: <elisp-expression>}.")
      (setq success nil))

     ((null code)
      (setq error "Missing required 'code' parameter")
      (setq success nil))

     (t
      (condition-case err
          (progn
            (setq stdout-string
                  (with-temp-buffer
                    (let ((standard-output (current-buffer)))
                      (setq result (eval (read code)))
                      (buffer-string))))
            (setq success t))
        (error
         (setq error (format "%s" err))
         (setq success nil)))))

    (emacs-mcp-send-response 
     `(("success" . ,success)
       ("result" . ,(format "%S" result))
       ("stdout" . ,stdout-string)
       ,@(when error `(("error" . ,error)))))))

(defservlet* mcp/specs application/json ()
  "Handle specs endpoint for MCP configuration."
  (emacs-mcp-log "Handling specs request")
  (let ((local-endpoint (format "http://127.0.0.1:%d/mcp" emacs-mcp-port)))
    (insert (format "
{
  \"tools\": {
    \"emacs\": {
      \"url\": \"%s\"
    }
  }
}
" local-endpoint))))

;; Additional Emacs-specific MCP endpoints

(defservlet* mcp/buffers application/json ()
  "Return information about all Emacs buffers."
  (emacs-mcp-log "Handling buffers request")
  (let (buffer-list)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (push `(("name" . ,(buffer-name))
                ("file" . ,(or (buffer-file-name) ""))
                ("major-mode" . ,(symbol-name major-mode))
                ("modified" . ,(buffer-modified-p))
                ("size" . ,(buffer-size))
                ("read-only" . ,buffer-read-only)) 
              buffer-list)))
    (emacs-mcp-send-response `(("buffers" . ,buffer-list)))))

(defservlet* mcp/current-buffer application/json ()
  "Return information about the current buffer."
  (emacs-mcp-log "Handling current-buffer request")
  (let ((buffer (current-buffer)))
    (with-current-buffer buffer
      (emacs-mcp-send-response
       `(("buffer" . (("name" . ,(buffer-name))
                      ("file" . ,(or (buffer-file-name) ""))
                      ("major-mode" . ,(symbol-name major-mode))
                      ("modified" . ,(buffer-modified-p))
                      ("size" . ,(buffer-size))
                      ("read-only" . ,buffer-read-only)
                      ("point" . ,(point))
                      ("mark" . ,(if (use-region-p) (mark) 0))
                      ("region-active" . ,(use-region-p)))))))))

(defservlet* mcp/buffer-content application/json ()
  "Return the content of a buffer specified by name."
  (emacs-mcp-log "Handling buffer-content request")
  (let* ((json-input (emacs-mcp-parse-json-body))
         (buffer-name (and json-input (cdr (assoc 'buffer json-input))))
         (buffer (and buffer-name (get-buffer buffer-name)))
         content)
    
    (cond
     ((null buffer-name)
      (emacs-mcp-send-response `(("error" . "Missing buffer name parameter"))))
     
     ((null buffer)
      (emacs-mcp-send-response `(("error" . ,(format "Buffer '%s' not found" buffer-name)))))
     
     (t
      (with-current-buffer buffer
        (setq content (buffer-substring-no-properties (point-min) (point-max))))
      (emacs-mcp-send-response 
       `(("buffer" . ,buffer-name)
         ("content" . ,content)))))))

;; File operations

(defservlet* mcp/read-file application/json ()
  "Read a file and return its content."
  (emacs-mcp-log "Handling read-file request")
  (let* ((json-input (emacs-mcp-parse-json-body))
         (filename (and json-input (cdr (assoc 'filename json-input))))
         content)
    
    (cond
     ((null filename)
      (emacs-mcp-send-response `(("error" . "Missing filename parameter"))))
     
     ((not (file-exists-p filename))
      (emacs-mcp-send-response `(("error" . ,(format "File '%s' not found" filename)))))
     
     (t
      (condition-case err
          (progn
            (setq content (with-temp-buffer
                            (insert-file-contents filename)
                            (buffer-string)))
            (emacs-mcp-send-response 
             `(("filename" . ,filename)
               ("content" . ,content))))
        (error
         (emacs-mcp-send-response 
          `(("error" . ,(format "Error reading file: %s" err))))))))))

(defservlet* mcp/write-file application/json ()
  "Write content to a file."
  (emacs-mcp-log "Handling write-file request")
  (let* ((json-input (emacs-mcp-parse-json-body))
         (filename (and json-input (cdr (assoc 'filename json-input))))
         (content (and json-input (cdr (assoc 'content json-input)))))
    
    (cond
     ((null filename)
      (emacs-mcp-send-response `(("error" . "Missing filename parameter"))))
     
     ((null content)
      (emacs-mcp-send-response `(("error" . "Missing content parameter"))))
     
     (t
      (condition-case err
          (progn
            (with-temp-file filename
              (insert content))
            (emacs-mcp-send-response 
             `(("success" . t)
               ("filename" . ,filename)
               ("message" . ,(format "Successfully wrote %d bytes to %s" 
                                    (length content) filename)))))
        (error
         (emacs-mcp-send-response 
          `(("error" . ,(format "Error writing file: %s" err))))))))))

;; Initialize endpoints
(defun initialize-mcp-endpoints ()
  "Initialize all MCP endpoints."
  (emacs-mcp-log "Initializing MCP endpoints"))

;; Run initialization
(initialize-mcp-endpoints)

(provide 'mcp-endpoints)
;;; mcp-endpoints.el ends here

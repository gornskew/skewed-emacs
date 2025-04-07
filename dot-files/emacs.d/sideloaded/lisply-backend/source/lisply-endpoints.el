;;; lisply-endpoints.el --- Lisply protocol endpoints for Emacs integration

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
;; This file defines the Lisply protocol endpoints for Emacs,
;; compatible with the Model Context Protocol for LLM integration.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'simple-httpd)
(require 'lisply-http-setup)

;; Lisply Tool Definitions

(defvar emacs-lisply-tools
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
  "List of tool definitions for the Emacs Lisply server.")

(defun emacs-lisply-generate-tool-description ()
  "Generate a tool description for LLM integration."
  `(("tools" . ,emacs-lisply-tools)))

;; Lisply HTTP handlers

(defservlet* lisply/ping-lisp text/plain ()
  "Handle generic ping-lisp endpoint for Lisply."
  (emacs-lisply-log "Handling ping-lisp request")
  (insert "pong"))

(defservlet* lisply/tools/list application/json ()
  "Handle tools tools/list endpoint for Lisply."
  (emacs-lisply-log "Handling tools/list request")
  (emacs-lisply-send-response (emacs-lisply-generate-tool-description)))

(defservlet* lisply/lisp-eval application/json ()
  "Handle Emacs Lisp evaluation endpoint for Lisply."
  (emacs-lisply-log "Handling lisp-eval request")
  (let* ((json-input (emacs-lisply-parse-json-body))
         (code (and json-input (cdr (assoc 'code json-input))))
         (stdout-string "")
         result
         error
         success)
    
    (emacs-lisply-log "Attempting to evaluate code: %s" code)
    
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

    (emacs-lisply-send-response 
     `(("success" . ,success)
       ("result" . ,(format "%S" result))
       ("stdout" . ,stdout-string)
       ,@(when error `(("error" . ,error)))))))

(defservlet* lisply/specs application/json ()
  "Handle suggested specs endpoint for MCP client configuration."
  (emacs-lisply-log "Handling specs request")
  (let ((local-endpoint (format "http://127.0.0.1:%d/lisply" emacs-lisply-port)))
    (insert (format "
{
  \"tools\": {
    \"emacs\": {
      \"url\": \"%s\"
    }
  }
}
" local-endpoint))))

;; Additional Emacs-specific Lisply endpoints

(defservlet* lisply/buffers application/json ()
  "Return information about all Emacs buffers."
  (emacs-lisply-log "Handling buffers request")
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
    (emacs-lisply-send-response `(("buffers" . ,buffer-list)))))

(defservlet* lisply/current-buffer application/json ()
  "Return information about the current buffer."
  (emacs-lisply-log "Handling current-buffer request")
  (let ((buffer (current-buffer)))
    (with-current-buffer buffer
      (emacs-lisply-send-response
       `(("buffer" . (("name" . ,(buffer-name))
                      ("file" . ,(or (buffer-file-name) ""))
                      ("major-mode" . ,(symbol-name major-mode))
                      ("modified" . ,(buffer-modified-p))
                      ("size" . ,(buffer-size))
                      ("read-only" . ,buffer-read-only)
                      ("point" . ,(point))
                      ("mark" . ,(if (use-region-p) (mark) 0))
                      ("region-active" . ,(use-region-p)))))))))

(defservlet* lisply/buffer-content application/json ()
  "Return the content of a buffer specified by name."
  (emacs-lisply-log "Handling buffer-content request")
  (let* ((json-input (emacs-lisply-parse-json-body))
         (buffer-name (and json-input (cdr (assoc 'buffer json-input))))
         (buffer (and buffer-name (get-buffer buffer-name)))
         content)
    
    (cond
     ((null buffer-name)
      (emacs-lisply-send-response `(("error" . "Missing buffer name parameter"))))
     
     ((null buffer)
      (emacs-lisply-send-response `(("error" . ,(format "Buffer '%s' not found" buffer-name)))))
     
     (t
      (with-current-buffer buffer
        (setq content (buffer-substring-no-properties (point-min) (point-max))))
      (emacs-lisply-send-response 
       `(("buffer" . ,buffer-name)
         ("content" . ,content)))))))

;; File operations

(defservlet* lisply/read-file application/json ()
  "Read a file and return its content."
  (emacs-lisply-log "Handling read-file request")
  (let* ((json-input (emacs-lisply-parse-json-body))
         (filename (and json-input (cdr (assoc 'filename json-input))))
         content)
    
    (cond
     ((null filename)
      (emacs-lisply-send-response `(("error" . "Missing filename parameter"))))
     
     ((not (file-exists-p filename))
      (emacs-lisply-send-response `(("error" . ,(format "File '%s' not found" filename)))))
     
     (t
      (condition-case err
          (progn
            (setq content (with-temp-buffer
                            (insert-file-contents filename)
                            (buffer-string)))
            (emacs-lisply-send-response 
             `(("filename" . ,filename)
               ("content" . ,content))))
        (error
         (emacs-lisply-send-response 
          `(("error" . ,(format "Error reading file: %s" err))))))))))

(defservlet* lisply/write-file application/json ()
  "Write content to a file."
  (emacs-lisply-log "Handling write-file request")
  (let* ((json-input (emacs-lisply-parse-json-body))
         (filename (and json-input (cdr (assoc 'filename json-input))))
         (content (and json-input (cdr (assoc 'content json-input)))))
    
    (cond
     ((null filename)
      (emacs-lisply-send-response `(("error" . "Missing filename parameter"))))
     
     ((null content)
      (emacs-lisply-send-response `(("error" . "Missing content parameter"))))
     
     (t
      (condition-case err
          (progn
            (with-temp-file filename
              (insert content))
            (emacs-lisply-send-response 
             `(("success" . t)
               ("filename" . ,filename)
               ("message" . ,(format "Successfully wrote %d bytes to %s" 
                                    (length content) filename)))))
        (error
         (emacs-lisply-send-response 
          `(("error" . ,(format "Error writing file: %s" err))))))))))

;; Initialize endpoints
(defun initialize-lisply-endpoints ()
  "Initialize all Lisply endpoints."
  (emacs-lisply-log "Initializing Lisply endpoints"))

;; Run initialization
(initialize-lisply-endpoints)

(provide 'lisply-endpoints)
;;; lisply-endpoints.el ends here

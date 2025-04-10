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


   ;;
   ;; FLAG -- http_request is implemented in middleware, not in this backend. 
   ;;
   
   
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


;; Initialize endpoints
(defun initialize-lisply-endpoints ()
  "Initialize all Lisply endpoints."
  (emacs-lisply-log "Initializing Lisply endpoints"))

;; Run initialization
(initialize-lisply-endpoints)

(provide 'lisply-endpoints)
;;; lisply-endpoints.el ends here

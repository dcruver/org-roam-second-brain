;;; org-roam-mcp-http.el --- MCP (Streamable HTTP) server for the second brain -*- lexical-binding: t; -*-

;; Author: Don Cruver
;; Version: 2.0.0
;; Package-Requires: ((emacs "29.1") (org-roam "2.2"))
;; Keywords: org-mode, roam, notes
;; URL: https://github.com/dcruver/org-roam-second-brain

;;; Commentary:
;; A small HTTP JSON-RPC server (MCP Streamable HTTP, POST only) exposing
;; the tools in `orsb-tools'.  Start with (orsb-mcp-start) or
;; (org-roam-mcp-http-start PORT HOST); stop with (orsb-mcp-stop).
;; Options: `orsb-mcp-port', `orsb-mcp-host', `orsb-mcp-auth-token',
;; `orsb-mcp-tool-timeout'.  Requests are logged to `orsb-mcp-log-buffer'.
;; Test: curl -s -X POST http://localhost:8007/mcp -H "Content-Type: application/json" \
;;         -d '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"get_schema","arguments":{}}}'

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'org-id)
(require 'orsb-core)
;; The tool contract (2.0 tools plus the deprecated names).
(require 'orsb-tools)

;; The port lives in `orsb-mcp-port' (transport section below);
;; `org-roam-mcp-http--port' is kept as an alias for old init files.

(defvar org-roam-mcp-http--session-id nil
  "Session id of the request being handled (request-scoped, dynamically bound).
Set by `initialize' to the new session so the response carries it.")

(defvar org-roam-mcp-http--protocol-version "2025-03-26"
  "MCP protocol version this server implements.")

(defvar org-roam-mcp-http--server-info
  '((name . "org-roam-mcp") (version . "1.0.0"))
  "Server info returned in MCP initialize response.")

(defun org-roam-mcp-http--generate-session-id ()
  "Generate a new MCP session ID."
  (org-id-uuid))

;; ---------------------------------------------------------------------------
;; Tool registry: maps tool name -> (:fn FUNC :args-spec ((NAME . TYPE) ...))
;; ---------------------------------------------------------------------------

(defvar org-roam-mcp-http--tools (make-hash-table :test 'equal)
  "Hash table of registered MCP tools.")

(cl-defstruct (org-roam-mcp-tool (:constructor org-roam-mcp-tool-create))
  "An MCP tool registration."
  name description fn args-spec required-args schema)

(defun org-roam-mcp-http--set-tool-description (tool description)
  "Replace TOOL's DESCRIPTION (used by orsb-tools to mark deprecations)."
  (setf (org-roam-mcp-tool-description tool) description))

(defun org-roam-mcp-http--register-tool (name description fn args-spec required-args schema)
  "Register a tool NAME with DESCRIPTION, FN, ARGS-SPEC, REQUIRED-ARGS, and SCHEMA."
  (puthash name (org-roam-mcp-tool-create
                 :name name
                 :description description
                 :fn fn
                 :args-spec args-spec
                 :required-args required-args
                 :schema schema)
           org-roam-mcp-http--tools))

;; ---------------------------------------------------------------------------
;; Tool definitions
;; ---------------------------------------------------------------------------

(defun org-roam-mcp-http--register-all-tools ()
  "Register the tool set (see `orsb-tools-register')."
  (orsb-tools-register))

;; ---------------------------------------------------------------------------
;; JSON-RPC dispatch
;; ---------------------------------------------------------------------------

(defun org-roam-mcp-http--handle-tools-call (params)
  "Handle a tools/call JSON-RPC method with PARAMS."
  (let* ((tool-name (alist-get 'name params))
         (arguments (or (alist-get 'arguments params) '()))
         (tool (gethash tool-name org-roam-mcp-http--tools)))
    (if (not tool)
        (json-encode `((error . ((code . -32601)
                                 (message . ,(format "Unknown tool: %s" tool-name))))))
      ;; Check required args
      (let ((missing (cl-remove-if
                      (lambda (req) (alist-get (intern req) arguments))
                      (org-roam-mcp-tool-required-args tool))))
        (if missing
            (json-encode `((error . ((code . -32602)
                                     (message . ,(format "Missing required arguments: %s"
                                                         (string-join missing ", ")))))))
          ;; Call the tool function, abandoning it after orsb-mcp-tool-timeout
          ;; seconds (effective only where the tool yields to the event loop).
          (condition-case err
              (let* ((started (float-time))
                     (result (with-timeout (orsb-mcp-tool-timeout :timeout)
                               (funcall (org-roam-mcp-tool-fn tool) arguments))))
                (when (eq result :timeout)
                  (orsb-mcp--log "tool %s timed out after %ss" tool-name orsb-mcp-tool-timeout)
                  (setq result
                        (if (orsb-tools-legacy-name-p tool-name)
                            ;; legacy shape
                            (json-encode `((success . :json-false)
                                           (error . ,(format "Tool %s timed out after %s seconds" tool-name orsb-mcp-tool-timeout))))
                          (json-encode `((ok . :json-false)
                                         (error . ((code . "timeout")
                                                   (message . ,(format "Tool %s timed out after %s seconds" tool-name orsb-mcp-tool-timeout))
                                                   (hint . "The server is busy or the operation is slow; try again or narrow the request."))))))))
                (orsb-mcp--log "tool %s %.0f ms%s" tool-name (* 1000 (- (float-time) started))
                               (if (string-prefix-p "{\"ok\":false" result) " (error)" ""))
                ;; result is a JSON string: the {"ok":...} envelope for 2.0
                ;; tools (MCP isError on failure) or the legacy {"success":...}
                ;; shape for deprecated names.
                (let ((is-error (string-prefix-p "{\"ok\":false" result)))
                  (json-encode `((result . ((content . [((type . "text")
                                                         (text . ,result))])
                                            ,@(when is-error '((isError . t)))))))))
            (error
             (json-encode `((error . ((code . -32603)
                                      (message . ,(format "Tool error: %s" (error-message-string err))))))))))))))

(defun org-roam-mcp-http--handle-tools-list (_params)
  "Handle a tools/list JSON-RPC method. Return all registered tool schemas."
  (let ((tools-list '()))
    (maphash
     (lambda (name tool)
       (let ((properties '())
             (schema (org-roam-mcp-tool-schema tool)))
         ;; Build properties from schema
         (dolist (prop schema)
           (push (cons (car prop) (cdr prop)) properties))
         (push `((name . ,name)
                 (description . ,(org-roam-mcp-tool-description tool))
                 (inputSchema . ((type . "object")
                                 ;; Empty alist json-encodes as null; use a hash-table
                                 ;; so zero-arg tools emit "properties": {} (MCP spec).
                                 (properties . ,(if properties
                                                    (nreverse properties)
                                                  (make-hash-table)))
                                 (required . ,(vconcat (org-roam-mcp-tool-required-args tool))))))
               tools-list)))
     org-roam-mcp-http--tools)
    (json-encode `((result . ((tools . ,(vconcat (nreverse tools-list)))))))))

(defgroup orsb-mcp nil
  "org-roam-second-brain MCP server transport."
  :group 'orsb
  :prefix "orsb-mcp-")

(defcustom orsb-mcp-port 8007
  "TCP port the MCP server listens on."
  :type 'integer :group 'orsb-mcp)
(defvaralias 'org-roam-mcp-http--port 'orsb-mcp-port)

(defcustom orsb-mcp-host "127.0.0.1"
  "Address to bind.  \"0.0.0.0\" exposes the server to the network;
combine that with `orsb-mcp-auth-token' unless the network is trusted."
  :type 'string :group 'orsb-mcp)

(defcustom orsb-mcp-auth-token nil
  "When non-nil, POST and DELETE must carry `Authorization: Bearer <token>'."
  :type '(choice (const nil) string) :group 'orsb-mcp)

(defcustom orsb-mcp-tool-timeout 30
  "Seconds a single tool call may run before it is abandoned with an error.
Only effective where the tool yields to the event loop (waiting on a
process or network); pure CPU work is not interrupted."
  :type 'number :group 'orsb-mcp)

(defcustom orsb-mcp-session-max-idle (* 24 60 60)
  "Seconds of inactivity after which a session is forgotten."
  :type 'integer :group 'orsb-mcp)

(defcustom orsb-mcp-session-max 64
  "Most sessions kept at once; the least recently used is dropped first."
  :type 'integer :group 'orsb-mcp)

(defcustom orsb-mcp-log-buffer "*orsb-mcp-log*"
  "Buffer that receives one line per request (nil disables logging)."
  :type '(choice (const nil) string) :group 'orsb-mcp)

(defcustom orsb-mcp-log-max-lines 2000
  "Trim the log buffer to this many lines."
  :type 'integer :group 'orsb-mcp)

(defconst org-roam-mcp-http--supported-protocols '("2025-03-26" "2025-06-18" "2025-11-25")
  "Protocol versions we can speak; the client's choice is echoed when listed.")

;; ---------------------------------------------------------------------------
;; Logging
;; ---------------------------------------------------------------------------

(defun orsb-mcp--log (format-string &rest args)
  "Append a timestamped line to `orsb-mcp-log-buffer'."
  (when orsb-mcp-log-buffer
    (with-current-buffer (get-buffer-create orsb-mcp-log-buffer)
      (goto-char (point-max))
      (insert (format-time-string "%Y-%m-%d %H:%M:%S ") (apply #'format format-string args) "\n")
      (when (> (count-lines (point-min) (point-max)) orsb-mcp-log-max-lines)
        (goto-char (point-min))
        (forward-line (/ orsb-mcp-log-max-lines 4))
        (delete-region (point-min) (point))))))

;; ---------------------------------------------------------------------------
;; Sessions: one entry per client, expiring, capped
;; ---------------------------------------------------------------------------

(defvar orsb-mcp--sessions (make-hash-table :test 'equal)
  "Session id -> plist (:created :last-seen :client).")

(defun orsb-mcp--sessions-expire ()
  "Forget idle sessions and, if still over the cap, the least recently used."
  (let ((now (float-time)) (stale nil))
    (maphash (lambda (id s)
               (when (> (- now (plist-get s :last-seen)) orsb-mcp-session-max-idle)
                 (push id stale)))
             orsb-mcp--sessions)
    (dolist (id stale) (remhash id orsb-mcp--sessions))
    (while (> (hash-table-count orsb-mcp--sessions) orsb-mcp-session-max)
      (let (oldest oldest-seen)
        (maphash (lambda (id s)
                   (when (or (null oldest-seen) (< (plist-get s :last-seen) oldest-seen))
                     (setq oldest id oldest-seen (plist-get s :last-seen))))
                 orsb-mcp--sessions)
        (remhash oldest orsb-mcp--sessions)))))

(defun orsb-mcp--session-create (client)
  "Register a new session for CLIENT (a name string) and return its id."
  (let ((id (org-roam-mcp-http--generate-session-id))
        (now (float-time)))
    (puthash id (list :created now :last-seen now :client client) orsb-mcp--sessions)
    ;; expire after inserting so the cap counts the newcomer
    (orsb-mcp--sessions-expire)
    id))

(defun orsb-mcp--session-touch (id)
  "Mark session ID as used; return non-nil when it is known."
  (when-let ((s (and id (gethash id orsb-mcp--sessions))))
    (plist-put s :last-seen (float-time))
    t))

(defun orsb-mcp--session-delete (id)
  "Forget session ID; return non-nil when it existed."
  (when (and id (gethash id orsb-mcp--sessions))
    (remhash id orsb-mcp--sessions)
    t))

(defun org-roam-mcp-http-sessions ()
  "Return the live sessions as a list of (ID . CLIENT), newest first."
  (let (out)
    (maphash (lambda (id s) (push (cons id (plist-get s :client)) out)) orsb-mcp--sessions)
    (sort out (lambda (a b) (> (plist-get (gethash (car a) orsb-mcp--sessions) :created)
                                (plist-get (gethash (car b) orsb-mcp--sessions) :created))))))

;; ---------------------------------------------------------------------------
;; MCP protocol handlers
;; ---------------------------------------------------------------------------

(defun org-roam-mcp-http--handle-initialize (params)
  "Handle MCP initialize: open a session and return capabilities.
The new id is left in `org-roam-mcp-http--session-id' (request-scoped)
so the HTTP layer puts it in the Mcp-Session-Id response header."
  (let* ((client (or (alist-get 'name (alist-get 'clientInfo params)) "unknown"))
         (wanted (alist-get 'protocolVersion params))
         (version (if (member wanted org-roam-mcp-http--supported-protocols)
                      wanted org-roam-mcp-http--protocol-version)))
    (setq org-roam-mcp-http--session-id (orsb-mcp--session-create client))
    (orsb-mcp--log "initialize client=%s protocol=%s session=%s (%d live)"
                   client version org-roam-mcp-http--session-id (hash-table-count orsb-mcp--sessions))
    (json-encode
     `((result . ((protocolVersion . ,version)
                  (capabilities . ((tools . ((listChanged . :json-false)))))
                  (serverInfo . ,org-roam-mcp-http--server-info)))))))

(defun org-roam-mcp-http--handle-ping (_params)
  "Handle MCP ping request."
  (json-encode '((result . ()))))

(defun org-roam-mcp-http--dispatch (body)
  "Dispatch a JSON-RPC request from BODY string.
Return JSON response string, or symbol `notification' for fire-and-forget messages."
  (condition-case err
      (let* ((request (json-read-from-string body))
             (id (alist-get 'id request))
             (method (alist-get 'method request))
             (params (or (alist-get 'params request) '())))
        ;; Notifications (no id) get no JSON-RPC response
        (if (and (null id) (stringp method) (string-prefix-p "notifications/" method))
            'notification
          (let ((response
                 (cond
                  ((equal method "initialize")
                   (org-roam-mcp-http--handle-initialize params))
                  ((equal method "ping")
                   (org-roam-mcp-http--handle-ping params))
                  ((equal method "tools/call")
                   (org-roam-mcp-http--handle-tools-call params))
                  ((equal method "tools/list")
                   (org-roam-mcp-http--handle-tools-list params))
                  (t
                   (json-encode `((error . ((code . -32601)
                                            (message . ,(format "Unknown method: %s" method))))))))))
            ;; Inject jsonrpc + id after the handler's opening brace via
            ;; string concatenation. Round-tripping through json-read-from-string
            ;; collapses every nested empty object ({}) into nil/null, which
            ;; strict MCP clients reject (e.g. zero-arg tools' inputSchema.properties).
            (concat "{\"jsonrpc\":\"2.0\",\"id\":"
                    (json-encode id)
                    ","
                    (substring response 1)))))
    (error
     (json-encode `((jsonrpc . "2.0")
                    (id . :null)
                    (error . ((code . -32700)
                              (message . ,(format "Parse error: %s" (error-message-string err))))))))))

;; ---------------------------------------------------------------------------
;; HTTP: parsing and handling are pure functions; the process filter only
;; accumulates bytes and defers POST work to a timer.
;; ---------------------------------------------------------------------------

(defvar org-roam-mcp-http--server nil
  "The TCP server process.")

(defun org-roam-mcp-http--make-http-response (status content-type body &optional extra-headers)
  "Create an HTTP response string with STATUS, CONTENT-TYPE, BODY and EXTRA-HEADERS.
The request-scoped `org-roam-mcp-http--session-id', when set, is echoed
in Mcp-Session-Id."
  (let ((session-header (if org-roam-mcp-http--session-id
                            (format "Mcp-Session-Id: %s\r\n" org-roam-mcp-http--session-id)
                          "")))
    (format "HTTP/1.1 %s\r\nContent-Type: %s\r\nContent-Length: %d\r\nAccess-Control-Allow-Origin: *\r\nAccess-Control-Allow-Methods: POST, DELETE, OPTIONS\r\nAccess-Control-Allow-Headers: Content-Type, Accept, Authorization, Mcp-Session-Id\r\n%s%sConnection: close\r\n\r\n%s"
            status content-type (string-bytes body) session-header (or extra-headers "") body)))

(defun org-roam-mcp-http--header (headers name)
  "Return the value of HTTP header NAME (case-insensitive) in HEADERS, or nil."
  (let ((case-fold-search t))
    (when (string-match (concat "^" (regexp-quote name) ": *\\([^\r\n]*\\)") headers)
      (string-trim (match-string 1 headers)))))

(defun org-roam-mcp-http--parse-request (buf)
  "Parse the accumulated request bytes BUF.
Return nil while the request is incomplete, else a plist
\(:method :path :headers :body).  The header/body separator is found once
and its length taken from that match, so a \\r\\n\\r\\n inside the body can
no longer confuse a request whose headers ended with \\n\\n."
  (when (string-match "\r\n\r\n\\|\n\n" buf)
    (let* ((header-end (match-beginning 0))
           (body-start (match-end 0))
           (headers (substring buf 0 header-end))
           (body (substring buf body-start))
           (cl (org-roam-mcp-http--header headers "Content-Length"))
           (cl-val (and cl (string-to-number cl))))
      (when (or (null cl-val) (>= (string-bytes body) cl-val))
        (when (and cl-val (> (string-bytes body) cl-val))
          (let ((encoded (encode-coding-string body 'utf-8)))
            (setq body (decode-coding-string (substring encoded 0 cl-val) 'utf-8))))
        (let* ((request-line (car (split-string headers "\r?\n")))
               (parts (split-string request-line " " t)))
          (list :method (upcase (or (car parts) ""))
                :path (or (cadr parts) "/")
                :headers headers
                :body body))))))

(defun org-roam-mcp-http--authorized-p (headers)
  "Whether HEADERS satisfy `orsb-mcp-auth-token' (always when it is nil)."
  (or (null orsb-mcp-auth-token)
      (equal (org-roam-mcp-http--header headers "Authorization")
             (concat "Bearer " orsb-mcp-auth-token))))

(defun org-roam-mcp-http--handle-request (req)
  "Produce the full HTTP response string for parsed request REQ.
Pure with respect to the network: safe to call from tests."
  (let* ((method (plist-get req :method))
         (headers (plist-get req :headers))
         (client-session (org-roam-mcp-http--header headers "Mcp-Session-Id"))
         ;; request-scoped: what the response header will carry
         (org-roam-mcp-http--session-id client-session))
    (cond
     ((equal method "OPTIONS")
      (org-roam-mcp-http--make-http-response "204 No Content" "text/plain" ""))
     ((equal method "GET")
      ;; MCP Streamable HTTP: GET opens an SSE stream, which we do not offer.
      (org-roam-mcp-http--make-http-response "405 Method Not Allowed" "text/plain" ""))
     ((not (org-roam-mcp-http--authorized-p headers))
      (orsb-mcp--log "%s rejected: bad or missing bearer token" method)
      (org-roam-mcp-http--make-http-response "401 Unauthorized" "text/plain" ""
                                             "WWW-Authenticate: Bearer\r\n"))
     ((equal method "DELETE")
      (if (orsb-mcp--session-delete client-session)
          (progn (orsb-mcp--log "session %s terminated by client" client-session)
                 (setq org-roam-mcp-http--session-id nil)
                 (org-roam-mcp-http--make-http-response "200 OK" "text/plain" ""))
        (org-roam-mcp-http--make-http-response "404 Not Found" "text/plain" "")))
     ((equal method "POST")
      (if (and client-session (not (orsb-mcp--session-touch client-session)))
          (progn (orsb-mcp--log "POST rejected: unknown session %s" client-session)
                 (org-roam-mcp-http--make-http-response "404 Not Found" "text/plain" ""))
        (let ((result (condition-case err
                          (org-roam-mcp-http--dispatch (plist-get req :body))
                        (error (json-encode
                                `((jsonrpc . "2.0") (id . :null)
                                  (error . ((code . -32603)
                                            (message . ,(error-message-string err))))))))))
          (if (eq result 'notification)
              (org-roam-mcp-http--make-http-response "202 Accepted" "text/plain" "")
            (org-roam-mcp-http--make-http-response "200 OK" "application/json" result)))))
     (t (org-roam-mcp-http--make-http-response "405 Method Not Allowed" "text/plain" "")))))

(defun org-roam-mcp-http--respond (proc response)
  "Send RESPONSE on PROC if the client is still connected, then close."
  (if (process-live-p proc)
      (progn (process-send-string proc response)
             (delete-process proc))
    (orsb-mcp--log "client went away before the response could be sent")))

(defun org-roam-mcp-http--handle-connection (proc data)
  "Accumulate DATA for PROC; once a request is complete, answer it.
POST work runs from a zero-delay timer so the filter returns at once."
  (let ((buf (concat (or (process-get proc :buffer) "") data)))
    (process-put proc :buffer buf)
    (when-let ((req (org-roam-mcp-http--parse-request buf)))
      (process-put proc :buffer nil)
      (if (equal (plist-get req :method) "POST")
          (run-at-time 0 nil
                       (lambda ()
                         (let ((started (float-time)))
                           (org-roam-mcp-http--respond proc (org-roam-mcp-http--handle-request req))
                           (orsb-mcp--log "POST %s %.0f ms" (plist-get req :path)
                                          (* 1000 (- (float-time) started))))))
        (org-roam-mcp-http--respond proc (org-roam-mcp-http--handle-request req))))))

(defun org-roam-mcp-http--sentinel (proc event)
  "Handle PROC connection EVENT."
  (when (string-match "\\(closed\\|connection broken\\|deleted\\)" event)
    (process-put proc :buffer nil)))

;;;###autoload
(defun org-roam-mcp-http-start (&optional port host)
  "Start the org-roam MCP HTTP server on PORT (default `orsb-mcp-port')
bound to HOST (default `orsb-mcp-host')."
  (interactive)
  (let ((port (or port orsb-mcp-port))
        (host (or host orsb-mcp-host)))
    (when org-roam-mcp-http--server
      (org-roam-mcp-http-stop))
    (org-roam-mcp-http--register-all-tools)
    (setq org-roam-mcp-http--server
          (make-network-process
           :name "org-roam-mcp-http"
           :server t
           :host host
           :service port
           :family 'ipv4
           :filter #'org-roam-mcp-http--handle-connection
           :sentinel #'org-roam-mcp-http--sentinel
           :coding 'utf-8))
    (orsb-mcp--log "server started on %s:%d with %d tools%s" host port
                   (hash-table-count org-roam-mcp-http--tools)
                   (if orsb-mcp-auth-token " (bearer auth on)" ""))
    (message "org-roam-mcp-http: started on %s:%d with %d tools"
             host port (hash-table-count org-roam-mcp-http--tools))))

;;;###autoload
(defun org-roam-mcp-http-stop ()
  "Stop the org-roam MCP HTTP server.  Sessions survive a restart."
  (interactive)
  (when org-roam-mcp-http--server
    (delete-process org-roam-mcp-http--server)
    (setq org-roam-mcp-http--server nil)
    (orsb-mcp--log "server stopped")
    (message "org-roam-mcp-http: stopped")))

(defalias 'orsb-mcp-start #'org-roam-mcp-http-start)
(defalias 'orsb-mcp-stop #'org-roam-mcp-http-stop)

(provide 'org-roam-mcp-http)
;;; org-roam-mcp-http.el ends here

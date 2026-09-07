;;; orsb-mcp-test.el --- ERT tests for the MCP transport -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercises the pure halves of the HTTP layer: request parsing and
;; `org-roam-mcp-http--handle-request', which produces the full HTTP response
;; for a parsed request without touching a socket.  Sessions, auth, and the
;; tool timeout are covered here.

;;; Code:

(require 'ert)
(require 'org-roam)
(require 'org-roam-mcp-http)
(require 'orsb-core-test)   ; vault fixture and JSON helpers

(defun orsb-mcp-test--request (method body &rest headers)
  "Build a parsed request plist for METHOD with BODY and extra HEADERS (strings)."
  (list :method method :path "/mcp"
        :headers (string-join (append (list (format "%s /mcp HTTP/1.1" method) "Host: test")
                                      headers
                                      (list (format "Content-Length: %d" (string-bytes body))))
                              "\r\n")
        :body body))

(defun orsb-mcp-test--status (response)
  "HTTP status code of RESPONSE as an integer."
  (string-to-number (nth 1 (split-string (car (split-string response "\r\n")) " "))))

(defun orsb-mcp-test--header (response name)
  "Header NAME in RESPONSE, or nil."
  (org-roam-mcp-http--header (car (split-string response "\r\n\r\n")) name))

(defun orsb-mcp-test--body (response)
  "Decoded JSON body of RESPONSE."
  (orsb-test--json (cadr (split-string response "\r\n\r\n"))))

(defun orsb-mcp-test--rpc (method &optional params)
  "JSON-RPC request text for METHOD with PARAMS."
  (json-encode `((jsonrpc . "2.0") (id . 1) (method . ,method) (params . ,(or params :empty-object)))))

(defmacro orsb-mcp-test--fresh (&rest body)
  "Run BODY with an empty session table, no auth, and no log buffer."
  (declare (indent 0))
  `(let ((orsb-mcp--sessions (make-hash-table :test 'equal))
         (orsb-mcp-auth-token nil)
         (orsb-mcp-log-buffer nil)
         (orsb-mcp-session-max-idle 3600)
         (orsb-mcp-session-max 64))
     ,@body))

;;;; parsing

(ert-deftest orsb-mcp-parse-incomplete-and-complete ()
  (should-not (org-roam-mcp-http--parse-request "POST /mcp HTTP/1.1\r\nContent-Length: 10\r\n\r\n{\"a\""))
  (let ((req (org-roam-mcp-http--parse-request "POST /mcp HTTP/1.1\r\nContent-Length: 2\r\n\r\n{}")))
    (should (equal (plist-get req :method) "POST"))
    (should (equal (plist-get req :path) "/mcp"))
    (should (equal (plist-get req :body) "{}")))
  ;; headers ended with \n\n but a \r\n\r\n appears later in the body: the
  ;; old parser measured the wrong separator and shifted the body.
  (let ((req (org-roam-mcp-http--parse-request "POST / HTTP/1.1\nContent-Length: 11\n\n\"a\r\n\r\nb\"  x")))
    (should (equal (plist-get req :body) "\"a\r\n\r\nb\"  x")))
  ;; multibyte body trimmed by bytes, not characters
  (let ((req (org-roam-mcp-http--parse-request (concat "POST / HTTP/1.1\r\nContent-Length: 6\r\n\r\n" "héllo" "extra"))))
    (should (equal (plist-get req :body) "héllo"))))

;;;; sessions

(ert-deftest orsb-mcp-two-clients-keep-their-sessions ()
  (orsb-test-with-vault
    (orsb-mcp-test--fresh
      (let* ((r1 (org-roam-mcp-http--handle-request
                  (orsb-mcp-test--request "POST" (orsb-mcp-test--rpc "initialize" '((protocolVersion . "2025-06-18") (clientInfo . ((name . "one"))))))))
             (r2 (org-roam-mcp-http--handle-request
                  (orsb-mcp-test--request "POST" (orsb-mcp-test--rpc "initialize" '((protocolVersion . "1999-01-01") (clientInfo . ((name . "two"))))))))
             (s1 (orsb-mcp-test--header r1 "Mcp-Session-Id"))
             (s2 (orsb-mcp-test--header r2 "Mcp-Session-Id")))
        (should (= 200 (orsb-mcp-test--status r1)))
        (should (and s1 s2 (not (equal s1 s2))))
        ;; a supported protocol version is echoed, an unknown one falls back
        (should (equal (alist-get 'protocolVersion (alist-get 'result (orsb-mcp-test--body r1))) "2025-06-18"))
        (should (equal (alist-get 'protocolVersion (alist-get 'result (orsb-mcp-test--body r2))) org-roam-mcp-http--protocol-version))
        (should (= 2 (length (org-roam-mcp-http-sessions))))
        ;; both sessions stay valid after the other initialized
        (dolist (s (list s1 s2))
          (let ((r (org-roam-mcp-http--handle-request
                    (orsb-mcp-test--request "POST" (orsb-mcp-test--rpc "ping") (concat "Mcp-Session-Id: " s)))))
            (should (= 200 (orsb-mcp-test--status r)))
            (should (equal (orsb-mcp-test--header r "Mcp-Session-Id") s))))
        ;; unknown session -> 404; no header -> allowed
        (should (= 404 (orsb-mcp-test--status (org-roam-mcp-http--handle-request
                                                (orsb-mcp-test--request "POST" (orsb-mcp-test--rpc "ping") "Mcp-Session-Id: nope")))))
        (should (= 200 (orsb-mcp-test--status (org-roam-mcp-http--handle-request
                                                (orsb-mcp-test--request "POST" (orsb-mcp-test--rpc "ping"))))))
        ;; DELETE removes only its own session
        (should (= 200 (orsb-mcp-test--status (org-roam-mcp-http--handle-request
                                                (orsb-mcp-test--request "DELETE" "" (concat "Mcp-Session-Id: " s1))))))
        (should (= 404 (orsb-mcp-test--status (org-roam-mcp-http--handle-request
                                                (orsb-mcp-test--request "POST" (orsb-mcp-test--rpc "ping") (concat "Mcp-Session-Id: " s1))))))
        (should (= 200 (orsb-mcp-test--status (org-roam-mcp-http--handle-request
                                                (orsb-mcp-test--request "POST" (orsb-mcp-test--rpc "ping") (concat "Mcp-Session-Id: " s2))))))
        (should (= 404 (orsb-mcp-test--status (org-roam-mcp-http--handle-request
                                                (orsb-mcp-test--request "DELETE" "" "Mcp-Session-Id: nope")))))))))

(ert-deftest orsb-mcp-sessions-expire-and-cap ()
  (orsb-mcp-test--fresh
    (let ((orsb-mcp-session-max-idle 0.005))
      (orsb-mcp--session-create "a")
      (sleep-for 0.02)
      (let ((b (orsb-mcp--session-create "b")))
        (should (= 1 (hash-table-count orsb-mcp--sessions)))
        (should (gethash b orsb-mcp--sessions))))
    (let ((orsb-mcp-session-max 2) (orsb-mcp-session-max-idle 3600))
      (clrhash orsb-mcp--sessions)
      (let ((first (orsb-mcp--session-create "a")))
        (sleep-for 0.01)
        (orsb-mcp--session-create "b")
        (sleep-for 0.01)
        (orsb-mcp--session-create "c")
        (should (= 2 (hash-table-count orsb-mcp--sessions)))
        (should-not (gethash first orsb-mcp--sessions))))))

;;;; auth and methods

(ert-deftest orsb-mcp-bearer-token ()
  (orsb-mcp-test--fresh
    (let ((orsb-mcp-auth-token "s3cret"))
      (should (= 401 (orsb-mcp-test--status (org-roam-mcp-http--handle-request
                                              (orsb-mcp-test--request "POST" (orsb-mcp-test--rpc "ping"))))))
      (should (= 401 (orsb-mcp-test--status (org-roam-mcp-http--handle-request
                                              (orsb-mcp-test--request "POST" (orsb-mcp-test--rpc "ping") "Authorization: Bearer wrong")))))
      (should (= 200 (orsb-mcp-test--status (org-roam-mcp-http--handle-request
                                              (orsb-mcp-test--request "POST" (orsb-mcp-test--rpc "ping") "Authorization: Bearer s3cret")))))
      ;; preflight never needs a token
      (should (= 204 (orsb-mcp-test--status (org-roam-mcp-http--handle-request (orsb-mcp-test--request "OPTIONS" ""))))))))

(ert-deftest orsb-mcp-methods-and-notifications ()
  (orsb-mcp-test--fresh
    (should (= 405 (orsb-mcp-test--status (org-roam-mcp-http--handle-request (orsb-mcp-test--request "GET" "")))))
    (should (= 405 (orsb-mcp-test--status (org-roam-mcp-http--handle-request (orsb-mcp-test--request "PUT" "")))))
    (should (= 202 (orsb-mcp-test--status (org-roam-mcp-http--handle-request
                                            (orsb-mcp-test--request "POST" "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")))))
    (let ((r (org-roam-mcp-http--handle-request (orsb-mcp-test--request "POST" "not json"))))
      (should (= 200 (orsb-mcp-test--status r)))
      (should (= -32700 (alist-get 'code (alist-get 'error (orsb-mcp-test--body r))))))))

;;;; tool timeout

(ert-deftest orsb-mcp-tool-timeout-produces-an-error ()
  (orsb-test-with-vault
    (orsb-mcp-test--fresh
      (org-roam-mcp-http--register-all-tools)
      (org-roam-mcp-http--register-tool
       "slow_test_tool" "sleeps" (lambda (_a) (sleep-for 2) "{\"ok\":true,\"data\":{}}") '() '() '())
      (unwind-protect
          (let* ((orsb-mcp-tool-timeout 0.2)
                 (r (org-roam-mcp-http--handle-request
                     (orsb-mcp-test--request "POST" (orsb-mcp-test--rpc "tools/call" '((name . "slow_test_tool") (arguments . :empty-object))))))
                 (result (alist-get 'result (orsb-mcp-test--body r)))
                 (text (orsb-test--json (alist-get 'text (car (alist-get 'content result))))))
            (should (= 200 (orsb-mcp-test--status r)))
            (should (eq (alist-get 'isError result) t))
            (should (equal (alist-get 'code (alist-get 'error text)) "timeout")))
        (remhash "slow_test_tool" org-roam-mcp-http--tools)))))

(provide 'orsb-mcp-test)
;;; orsb-mcp-test.el ends here

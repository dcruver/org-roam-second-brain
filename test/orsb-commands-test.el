;;; orsb-commands-test.el --- ERT tests for the keyboard commands -*- lexical-binding: t; -*-

;;; Commentary:
;; The sb/ commands know no note types: they ask the vault.  These tests run
;; them against the schema vault of orsb-tools-test with the minibuffer stubbed.

;;; Code:

(require 'ert)
(require 'org-roam)
(require 'org-roam-second-brain)
(require 'orsb-core-test)
(require 'orsb-tools-test)

(defmacro orsb-commands-test--answering (answers &rest body)
  "Run BODY with minibuffer questions answered from ANSWERS, in order."
  (declare (indent 1))
  `(let ((queue (copy-sequence ,answers)))
     (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (pop queue)))
               ((symbol-function 'completing-read) (lambda (&rest _) (pop queue)))
               ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
               ((symbol-function 'display-buffer) #'ignore))
       ,@body)))

(defun orsb-commands-test--text (buffer)
  "The text of BUFFER."
  (with-current-buffer buffer (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest orsb-commands-offer-the-types-the-vault-defines ()
  (orsb-test-with-vault
    (should (equal (sb/--type-names) '("note")))
    (orsb-tools-test--schema-vault "strict")
    (should (equal (sb/--type-names) '("note" "project" "post" "telos")))))

(ert-deftest orsb-commands-new-asks-for-what-the-type-requires ()
  (orsb-test-with-vault
    (orsb-tools-test--schema-vault "strict")
    ;; project: STATUS has no default in this vault, so it is asked for, then NEXT-ACTION
    (orsb-commands-test--answering '("active" "Order rails")
      (sb/new "project" "Rack it"))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-prefix-p (expand-file-name "projects/" org-roam-directory) (buffer-file-name)))
      (should (string-match-p "^:STATUS: active$" text))
      (should (string-match-p "^:NEXT-ACTION: Order rails$" text))
      (should (string-match-p "^\\* Current state$" text)))
    ;; post: everything it requires has a default, so nothing is asked
    (orsb-commands-test--answering '()
      (sb/new "post" "Hello there"))
    (should (string-match-p "/projects/posts/" (buffer-file-name)))
    ;; a refusal reaches the user as an error, not a broken note
    (should-error (orsb-commands-test--answering '() (sb/new "note" "Loose")) :type 'user-error)))

(ert-deftest orsb-commands-listings-and-proposals ()
  (orsb-test-with-vault
    (orsb-tools-test--schema-vault "strict")
    (orsb-commands-test--answering '("active" "Start")
      (sb/new "project" "Fan curve"))
    (let ((id (org-roam-node-id (orsb-core-resolve "Fan curve"))))
      (orsb-commands-test--answering '()
        (sb/digest)
        (should (string-match-p "\\[project\\] Fan curve  (active) -> Start" (orsb-commands-test--text sb/digest-buffer-name)))
        (should (string-match-p "Waiting for approval: 0" (orsb-commands-test--text sb/digest-buffer-name)))
        (sb/open)
        (should (string-match-p "Fan curve" (orsb-commands-test--text "*Second Brain: unfinished*")))
        (sb/list "telos")
        (should (string-match-p "\\[telos\\] Mission" (orsb-commands-test--text "*Second Brain: telos*")))
        (sb/search "fan")
        (should (string-match-p "\\[project\\] Fan curve" (orsb-commands-test--text "*Search Results*"))))
      ;; an agent files a change; it shows up, previews, and applies from the buffer
      (orsb-tools-test--data "update_body" 'id id 'section "Design" 'content "Both fans push." 'propose t 'reason "agreed")
      (orsb-commands-test--answering '()
        (sb/proposals)
        (with-current-buffer "*Second Brain: proposals*"
          (should (string-match-p "update_body" (buffer-string)))
          (should (string-match-p "agreed" (buffer-string)))
          (goto-char (point-min))
          (sb/buffer-next-item)
          (should (sb/--proposal-at-point))
          (sb/buffer-visit-item)
          (should (string-match-p "^\\+ Both fans push\\.$" (orsb-commands-test--text "*Second Brain: proposal*")))
          (sb/proposal-approve)))
      (should (string-match-p "Both fans push\\." (orsb-test--file (file-relative-name (org-roam-node-file (orsb-core-resolve id))
                                                                                         org-roam-directory))))
      (should (string-match-p "Nothing is waiting" (orsb-commands-test--text "*Second Brain: proposals*"))))))

(provide 'orsb-commands-test)
;;; orsb-commands-test.el ends here

;;; serve.el --- Run the MCP server on a throwaway vault for the eval harness -*- lexical-binding: t; -*-

;;; Commentary:
;; Started by run_eval.py, once per chore:
;;
;;   emacs -Q --batch -l eval/serve.el
;;
;; Settings come from the environment:
;;   ORSB_EVAL_VAULT      vault directory (a fresh copy of eval/vault)
;;   ORSB_EVAL_STATE      directory for the org-roam database, id locations,
;;                        the undo journal and the search cache (outside the vault)
;;   ORSB_EVAL_PORT       port for the MCP server (127.0.0.1)
;;   ORSB_EVAL_EMBED_URL  OpenAI-compatible embeddings base URL, e.g.
;;                        http://127.0.0.1:8090/v1 (empty: no semantic search)
;;   ORSB_EVAL_EMBED_MODEL, ORSB_EVAL_EMBED_DIMS  model name and vector size
;;   ORSB_EVAL_PROFILE    tool profile, small (default) or full
;;   ORSB_DEPS            same as for `make test': where org-roam is installed
;;
;; Before the server starts it syncs the org-roam database and, when an
;; embeddings URL is set, embeds every note, so semantic search and
;; find_related work from the first call.  While serving, notes that tools
;; changed are re-embedded between requests (batch Emacs never goes idle, so
;; the usual idle-time worker would not run on its own).

;;; Code:

(require 'subr-x)

(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (root (expand-file-name ".." here))
       (deps (getenv "ORSB_DEPS")))
  (add-to-list 'load-path root)
  (when (and deps (not (string-empty-p deps)))
    (dolist (dir (split-string deps ":" t))
      (dolist (sub (directory-files (expand-file-name dir) t "\\`[^.]"))
        (when (and (file-directory-p sub)
                   (not (string-match-p "org-roam-second-brain" sub)))
          (add-to-list 'load-path sub t)))))
  (unless (locate-library "org-roam")
    (require 'package)
    (package-initialize))
  (unless (locate-library "org-roam")
    (message "org-roam not found on load-path; set ORSB_DEPS to a directory of package builds")
    (kill-emacs 2)))

(defun orsb-eval--env (name &optional default)
  "Environment variable NAME, or DEFAULT when unset or empty."
  (let ((v (getenv name)))
    (if (and v (not (string-empty-p v))) v default)))

(let ((vault (orsb-eval--env "ORSB_EVAL_VAULT"))
      (state (orsb-eval--env "ORSB_EVAL_STATE")))
  (unless (and vault state)
    (message "serve.el: set ORSB_EVAL_VAULT and ORSB_EVAL_STATE")
    (kill-emacs 2))
  (make-directory state t)
  (setq org-roam-directory (file-name-as-directory (expand-file-name vault))
        org-roam-db-location (expand-file-name "org-roam.db" state)
        org-id-locations-file (expand-file-name "org-id-locations" state)
        org-id-track-globally t
        org-roam-file-extensions '("org")
        make-backup-files nil
        create-lockfiles nil
        auto-save-default nil
        user-emacs-directory (file-name-as-directory state)))

;; Local services only: keep url.el away from any HTTP proxy in the environment.
(setq url-proxy-services nil)

(require 'org-roam)
(require 'orsb-core)
(require 'orsb-search)
(require 'orsb-tools)
(require 'org-roam-mcp-http)
(require 'org-roam-vector-search)

(let ((state (orsb-eval--env "ORSB_EVAL_STATE"))
      (embed-url (orsb-eval--env "ORSB_EVAL_EMBED_URL")))
  (setq orsb-change-directory (expand-file-name "changes" state)
        orsb-search-cache-file (expand-file-name "orsb-embeddings.eld" state)
        orsb-mcp-host "127.0.0.1"
        orsb-mcp-port (string-to-number (orsb-eval--env "ORSB_EVAL_PORT" "8017"))
        orsb-mcp-auth-token nil
        orsb-mcp-tool-profile (intern (orsb-eval--env "ORSB_EVAL_PROFILE" "small")))
  (when embed-url
    (setq org-roam-semantic-embedding-url embed-url
          org-roam-semantic-embedding-model (orsb-eval--env "ORSB_EVAL_EMBED_MODEL" "stand-in")
          org-roam-semantic-embedding-dimensions
          (string-to-number (orsb-eval--env "ORSB_EVAL_EMBED_DIMS" "768")))))

(org-roam-db-sync 'force)

(defvar orsb-eval--embeddings (and (orsb-eval--env "ORSB_EVAL_EMBED_URL") t)
  "Non-nil when notes are embedded (semantic search available).")

(when orsb-eval--embeddings
  (let ((n 0))
    (dolist (file (org-roam-list-files))
      (setq n (+ n (car (orsb-search-generate-file file)))))
    (message "serve.el: embedded %d chunks" n))
  ;; generating embeddings writes ids and hashes into the notes
  (org-roam-db-sync))

(orsb-mcp-start)
;; run_eval.py waits for this line on stderr
(message "ORSB-EVAL READY port=%d tools=%d embeddings=%s"
         orsb-mcp-port (hash-table-count org-roam-mcp-http--tools)
         (if orsb-eval--embeddings "on" "off"))

(while t
  (accept-process-output nil 0.2)
  (when (and orsb-eval--embeddings (bound-and-true-p orsb-search--queue))
    (condition-case err
        (orsb-search-drain)
      (error (message "serve.el: embedding worker: %s" (error-message-string err))))))

;;; serve.el ends here

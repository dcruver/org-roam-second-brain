# Setup

## 1. Emacs and org-roam

Emacs 28.1 or newer with org-roam 2.2+. Set `org-roam-directory` and run
`M-x org-roam-db-sync` once so the database matches the vault.

## 2. Install the package

Doom:

```elisp
;; packages.el
(package! org-roam-second-brain
  :recipe (:host github :repo "dcruver/org-roam-second-brain"))

;; config.el
(use-package! org-roam-second-brain
  :after org-roam
  :config
  (orsb-mode 1)
  (setq orsb-hugo-base-dir "~/Projects/my-site/"))   ; only if you blog with ox-hugo
```

Vanilla Emacs with straight.el:

```elisp
(straight-use-package
 '(org-roam-second-brain :type git :host github :repo "dcruver/org-roam-second-brain"))
(require 'org-roam-second-brain)
(orsb-mode 1)
```

Update with `doom sync -u` or `M-x straight-pull-package`.

`orsb-mode` binds `C-c b` (second-brain commands) and `C-c v` (semantic
search). Nothing is bound until the mode is on.

## 3. Vault layout

The package expects typed notes in subdirectories (`orsb-directories`):
`projects/`, `people/`, `ideas/`, `admin/`, `blog/`, `reference/`, `howto/`,
plus `daily/` and `archive/`. A typed note starts with a file-level drawer:

```org
:PROPERTIES:
:ID:       0d5a4e0c-...
:NODE-TYPE: project
:STATUS:   active
:NEXT-ACTION: Order the parts
:END:
#+title: Rebuild the shed
```

`M-x orsb-migrate-status` reports (and with a prefix argument rewrites)
status spellings from older conventions.

## 4. Semantic search (optional)

Any OpenAI-compatible embeddings endpoint works. With Ollama:

```sh
ollama pull nomic-embed-text
```

```elisp
(setq org-roam-semantic-embedding-url "http://localhost:11434/v1"
      org-roam-semantic-embedding-model "nomic-embed-text"
      org-roam-semantic-embedding-dimensions 768)
```

For an existing vault: `M-x orsb-search-backfill-hashes`, then
`M-x org-roam-semantic-generate-all-chunks` (or `sync` over MCP with
`embeddings: true`, which queues the work for idle time). Afterwards saving a
note keeps its embeddings current in the background.

## 5. MCP server (optional)

Run it in a long-lived Emacs (a daemon is ideal):

```elisp
(setq orsb-mcp-host "0.0.0.0"          ; default 127.0.0.1
      orsb-mcp-port 8007
      orsb-mcp-auth-token "change-me")  ; nil = no auth; keep nil only on a trusted network
(add-hook 'emacs-startup-hook (lambda () (when (daemonp) (orsb-mcp-start))))
```

Client configuration (Claude Code, Claude Desktop, any Streamable HTTP
client): URL `http://<host>:8007/mcp`, header `Authorization: Bearer
change-me` when a token is set.

Verify:

```sh
curl -s -X POST http://localhost:8007/mcp -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' | head -c 400
```

Diagnostics: `M-x org-roam-mcp-http-sessions` lists live sessions; the
`*orsb-mcp-log*` buffer has one line per request and per tool call.

## 6. Agenda (optional)

```elisp
(with-eval-after-load 'org-agenda (orsb-agenda-setup))
```

adds agenda views over `projects/` and `admin/` and journals TODO state
changes into the daily note.

## 7. Deploying a new version to a running server

```sh
git pull            # in the package checkout
make reload         # emacsclient -f/-s your server: reload modules, restart the MCP server
```

Sessions survive a restart; clients that were mid-session simply
re-initialize.

# Setup

## 1. Emacs and org-roam

Emacs 29.1 or newer (for the builtin SQLite org-roam uses) with org-roam 2.2+. Set `org-roam-directory` and run
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
  (orsb-mode 1))
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

The package uses two folders of its own, `daily/` and `archive/`
(`orsb-daily-directory`, `orsb-archive-directory`). Everything else about the
layout is yours: a folder is a note type when it holds a `_schema.org` that
sets `TYPE`. Start with one file at the top of the vault and add a folder per
type as you need them; the README's "Note types" section has the format.
With no `_schema.org` at all, every note is a `note` and nothing is checked.

A note needs only an id and a title:

```org
:PROPERTIES:
:ID:       0d5a4e0c-...
:STATUS:   active
:END:
#+title: Rebuild the shed
```

Its type is the folder it sits in; there is no type property.

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

Vectors are kept in `~/.cache/org-roam-second-brain/embeddings.db`
(`orsb-embed-cache-file`), outside the notes. To embed a vault:
`M-x orsb-search-generate-all` (or `sync` over MCP with `embeddings: true`,
which queues the work for idle time). A vault whose notes still carry
`:EMBEDDING:` lines can skip the re-embed with
`M-x orsb-search-migrate-properties`; see the README. Afterwards saving a
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

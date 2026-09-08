# org-roam-second-brain

A second brain on top of [org-roam](https://www.orgroam.com/): typed notes
(projects, people, ideas, admin, blog posts) with conventions an agent can
rely on, semantic search over your vault, a daily digest, and an **MCP server**
so an LLM assistant uses the same notes you do. Everything is Emacs Lisp; the
only external service is an OpenAI-compatible embeddings endpoint (optional).

Version 2.0 has one core and one contract:

| Module | What it is |
|---|---|
| `orsb-core.el` | Every capability as a plain function: resolve a note by id, path or title; read and write the `:PROPERTIES:` drawer; create typed notes; edit sections with a destructive-replace guard; daily notes, inbox, follow-ups, blog status. |
| `orsb-tools.el` | The 18-tool MCP contract on top of the core, one `id` parameter, one `{ok, data | error}` envelope, one status vocabulary. The pre-2.0 tool names remain as deprecated mappers (see `MIGRATION.md`). |
| `org-roam-mcp-http.el` | The MCP server (Streamable HTTP, POST): per-client sessions, optional bearer token, tool timeout, request log. |
| `orsb-search.el` | Semantic search: chunk embeddings stored in the notes beside a content hash, an mtime-keyed index with a cache file, and an idle-time worker that re-embeds what changed. |
| `org-roam-vector-search.el` | The embedding primitives and the interactive search commands. |
| `org-roam-second-brain.el` | The human side: `orsb-mode` (`C-c b` and `C-c v`), capture commands, the daily digest, blog helpers. |
| `org-agenda-project-tracking.el` | Optional agenda views and TODO journaling (`orsb-agenda-setup`). |

## Install

Doom Emacs (`packages.el` and `config.el`):

```elisp
(package! org-roam-second-brain
  :recipe (:host github :repo "dcruver/org-roam-second-brain"))

(use-package! org-roam-second-brain
  :after org-roam
  :config
  (orsb-mode 1))
```

straight.el:

```elisp
(straight-use-package
 '(org-roam-second-brain :type git :host github :repo "dcruver/org-roam-second-brain"))
(require 'org-roam-second-brain)
(orsb-mode 1)
```

Requires Emacs 28.1+ and org-roam 2.2+.

## Configure

Everything environment-specific is a defcustom in the `orsb` group:

```elisp
(setq org-roam-directory "~/org-roam")
;; vault layout (defaults shown)
(setq orsb-directories '(("project" . "projects") ("person" . "people") ("idea" . "ideas")
                         ("admin" . "admin") ("blog" . "blog") ("reference" . "reference")
                         ("howto" . "howto") ("note" . "")))
(setq orsb-daily-directory "daily" orsb-archive-directory "archive")
;; status vocabulary and staleness
(setq orsb-status-values '("active" "waiting" "blocked" "someday" "done" "cancelled")
      orsb-stale-days 5)
;; blog posts (ox-hugo); nil refuses blog creation
(setq orsb-hugo-base-dir "~/Projects/my-site/" orsb-hugo-sections '("homelab" "writing"))
;; semantic search (any OpenAI-compatible /embeddings endpoint, e.g. Ollama or Infinity)
(setq org-roam-semantic-embedding-url "http://localhost:11434/v1"
      org-roam-semantic-embedding-model "nomic-embed-text")
```

## The MCP server

```elisp
(setq orsb-mcp-host "127.0.0.1"    ; "0.0.0.0" to serve your LAN
      orsb-mcp-port 8007
      orsb-mcp-auth-token nil)     ; set a string to require Authorization: Bearer
(orsb-mcp-start)
```

Point an MCP client at `http://host:8007/mcp` (Streamable HTTP, POST only; no
SSE stream). The server keeps one session per client, times out a tool after
`orsb-mcp-tool-timeout` seconds, and logs every request to `*orsb-mcp-log*`.

Try it:

```sh
curl -s -X POST http://localhost:8007/mcp -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"get_schema","arguments":{}}}'
```

### Tools

| Tool | Purpose |
|---|---|
| `search` | title / contextual / semantic search; hits carry id, type, status, snippet |
| `get_node` | one node: properties (its drawer), keywords, tags, links, body, or one `section` |
| `list_nodes` | file nodes filtered by type, status, staleness, tags |
| `create_node` | a typed note in its conventional directory |
| `add_heading` | a heading node with its own id |
| `set_node` | title, status (validated), todo, properties, tags, keywords |
| `update_body` | append / prepend / replace, per section or whole note (guarded) |
| `delete_node` | delete a heading, or delete / archive a note |
| `link_nodes` | add or remove an `[[id:...]]` link |
| `add_daily_entry`, `get_daily`, `log_to_inbox` | daily notes and the inbox |
| `get_digest`, `get_projects`, `get_followups`, `get_blog_status` | surfacing |
| `sync` | database and embedding refresh |
| `get_schema` | the vocabulary the server enforces |

Every tool takes `id` as an org-roam id, a vault-relative path, or an exact
title, and answers `{"ok":true,"data":{...}}` or
`{"ok":false,"error":{"code","message","hint"}}` (with MCP `isError`).
Status is validated: `active | waiting | blocked | someday | done | cancelled`
for notes, `idea | stub | draft | published` for blog posts.
`M-x orsb-migrate-status` normalizes older spellings in a vault.

## Semantic search

Embeddings are stored in the notes themselves (`:EMBEDDING:` next to an
`:EMBEDDING_HASH:` of the chunk text), so they travel with the vault. Saving
a note queues it; an idle-time worker re-embeds only the chunks whose text
changed. Queries read an in-memory index persisted to
`orsb-search-cache-file`. For an existing vault run
`M-x orsb-search-backfill-hashes` once, then `sync {"embeddings": true}` or
`M-x org-roam-semantic-generate-all-chunks`.

## Develop

```sh
make test     # ERT against a throwaway vault (ORSB_DEPS points at your org-roam build)
make compile  # byte-compile everything, warnings shown
make lint     # warnings are errors for the orsb-* modules
make reload   # reload into a running Emacs server and restart the MCP server
```

CI runs the same on Emacs 28, 29 and 30. See `SETUP.md` for a full
walkthrough and `MIGRATION.md` for the 1.x → 2.0 tool mapping.

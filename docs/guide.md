# Guide

How to set up org-roam-second-brain and what each part does. The
[README](https://github.com/dcruver/org-roam-second-brain#readme) is the short version;
the [tools page](tools.md) lists what an assistant can call, and the [FAQ](faq.md)
covers what people ask first.

- [Emacs and org-roam](#emacs-and-org-roam)
- [Install the package](#install-the-package)
- [Configure](#configure)
- [Vault layout](#vault-layout)
- [Note types: `_schema.org`](#note-types-_schemaorg)
- [Proposals](#proposals)
- [In Emacs](#in-emacs)
- [Semantic search](#semantic-search)
- [The MCP server](#the-mcp-server)
- [Agenda](#agenda)
- [Updating a running server](#updating-a-running-server)
- [The modules](#the-modules)

## Emacs and org-roam

Emacs 29.1 or newer (for the builtin SQLite org-roam uses) with org-roam 2.2+. Set `org-roam-directory` and run
`M-x org-roam-db-sync` once so the database matches the vault.

## Install the package

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

## Configure

Everything environment-specific is a defcustom in the `orsb` group:

```elisp
(setq org-roam-directory "~/org-roam")
;; the two folders the package itself uses (defaults shown)
(setq orsb-daily-directory "daily" orsb-archive-directory "archive")
;; days without a change before unfinished work counts as stale
(setq orsb-stale-days 5)
;; semantic search (any OpenAI-compatible /embeddings endpoint, e.g. Ollama or Infinity)
(setq org-roam-semantic-embedding-url "http://localhost:11434/v1"
      org-roam-semantic-embedding-model "nomic-embed-text")
```

## Vault layout

The package uses two folders of its own, `daily/` and `archive/`
(`orsb-daily-directory`, `orsb-archive-directory`). Everything else about the
layout is yours: a folder is a note type when it holds a `_schema.org` that
sets `TYPE`. Start with one file at the top of the vault and add a folder per
type as you need them; the next section has the format.
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

## Note types: `_schema.org`

The package defines no note types. A folder becomes a type when it holds a
`_schema.org` that sets `TYPE`; a folder without one is grouping only. A
folder's rules are the vault root's file, then each `_schema.org` on the way
down, merged: a lower file's entry replaces the same one from above, new
entries are added, and a table row whose second column is `remove` deletes an
inherited one. So a note's type is where it lives, a folder says what a note
*is a kind of*, and what a note *has* or *belongs to* is a property whose
form is a link. The files carry no `:ID:`, so they are not notes.

```org
* Type
:PROPERTIES:
:TYPE: project
:WRITE: open                 # open | append | propose | readonly
:LIFECYCLE: STATUS           # the property that says how far along it is
:LIFECYCLE_VALUES: active waiting done
:LIFECYCLE_FINISHED: done
:END:

* Properties
| property    | required | form                        | default |
|-------------+----------+-----------------------------+---------|
| STATUS      | yes      | one of LIFECYCLE_VALUES     | active  |
| NEXT-ACTION | yes      | one sentence                |         |
| IDEA        | no       | link to a note of type idea |         |

* Sections
| section       | required | rule    |
|---------------+----------+---------|
| Current state | yes      | replace |
| Log           | yes      | append  |
| Decisions     | no       | propose |

* Keywords
| keyword | value    |
|---------+----------|
| date    | [%DATE%] |

* Template
** Current state
As of %DATE%.
** Log
```

- Forms the server checks: `id`, `one of A B C` (or the name of a Type key
  holding the list), `date`, `URL`, `four digits`, `link` and `link to a note
  of type T`. Anything else is a description.
- `date, set by the server` is filled in on creation. Defaults, keyword
  values and the template may use `%DATE%`, `%TITLE%` and `%SLUG%`. A keyword value may also use `%TAGS%`
  (the note's tags, space separated). `%BODY%` in
  the template is where the text given at creation goes (before the first
  heading when the template does not say).
- Section rules: `replace` (rewritten whole), `append` (only grows),
  `propose` (needs approval). A type's `WRITE` does the same for the whole
  note; `NEVER: move merge delete` forbids those operations.
- The root file may also set `EXCLUDE` (subfolders outside the rules),
  `NOTES_HERE: no` (no note directly in this folder) and `ENFORCE`:
  `strict` refuses a write that breaks a rule, `warn` (the default) makes it
  and lists what it broke under `warnings`, `off` ignores the rules. `ENFORCE`
  is inherited like any other key, so one folder can be strict in a vault
  that otherwise warns.
- A vault with no `_schema.org` has one type, `note`, and no rules.

`check_schema` lists where existing notes depart from the rules; it changes
nothing. `get_schema` lists the types found, and with `type` one type's
merged rules.

## Proposals

Any write tool takes `propose: true` (and a `reason`): the
change is worked out, stored with its line-by-line preview, and nothing is
written. `get_proposals` lists what is waiting; `decide_proposal` approves
(applies it, returning a `change_id`) or rejects. A proposal whose note
changed since it was filed is refused unless forced. Proposing lifts only the
`propose` rules; every other rule still holds. The server cannot tell callers
apart: restricting who may call `decide_proposal` is the job of whatever sits
in front of it.

Proposals that belong together can share a `batch` label when they are filed.
`decide_proposal` with `batch` decides every pending proposal of that batch
and lists the ones it refused; in Emacs, `B` on the approval screen does the
same for the batch of the proposal at point.

Proposals are kept by the notes server that took them, outside the vault. In an
Emacs that is not that server (a desktop beside a headless server), set
`orsb-server-url` to the server's address (`http://host:8007/mcp`): `sb/proposals`
then lists, shows and decides them there, and the server makes the approved change.

## In Emacs

`orsb-mode` binds two prefixes and nothing else. The commands go through the same
tool functions as the MCP server, so the vault's rules apply to them too.

| Key | Command | What it does |
|---|---|---|
| `C-c b n` | `sb/new` | a note of any type the vault defines, asking for what that type requires |
| `C-c b d` | `sb/digest` | unfinished notes, the stale ones, tasks due, the inbox, and how many changes wait for approval |
| `C-c b o` | `sb/open` | unfinished notes |
| `C-c b s` | `sb/stale` | unfinished notes nobody has touched for `orsb-stale-days` |
| `C-c b l` | `sb/list` | the notes of one type |
| `C-c b A` | `sb/proposals` | changes waiting for approval: `RET` preview, `a` approve, `r` reject, `B` approve the batch |
| `C-c b I` | `sb/inbox` | a line into today's inbox |
| `C-c b /` | `sb/search` | title search |
| `C-c b L` | `sb/suggest-links` | notes close in meaning to this one that it does not link yet |
| `C-c b D l`, `L`, `c` | `sb/daily-link`, `-link-all`, `-connections` | link a daily note to the notes it mentions |
| `C-c v s` | `org-roam-semantic-search` | search by meaning |
| `C-c v i`, `C-c v r` | `org-roam-semantic-insert-similar`, `-insert-related` | insert links to similar notes |

In a listing, `n` and `p` move, `RET` opens, `g` refreshes, `i` copies a link and `q`
quits. Two idle seconds after Emacs starts, `orsb-mode` shows the digest once.

`orsb-blog.el` is optional: load it for the `sb/blog-*` commands over a folder of
ox-hugo posts (`orsb-blog-directory`), under `C-c b B`.

## Semantic search

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
`M-x orsb-search-generate-all`. A vault whose notes still carry
`:EMBEDDING:` lines can skip the re-embed with
`M-x orsb-search-migrate-properties`; see below. Afterwards saving a
note keeps its embeddings current in the background.

Embeddings live in one SQLite file beside the notes, not in them:
`orsb-embed-cache-file`, by default
`~/.cache/org-roam-second-brain/embeddings.db`. Keep it outside any synced
folder. Each row is one org-roam node (keyed by its `:ID:`) and one embedding
model, with the hash of the text it was made from:

```
node_id       org-roam ID of the heading (or file)
model         embedding model that made it
content_hash  sha1 of the normalized chunk text
dims          vector length
vector        little-endian float32s, packed as a blob
updated_at    when it was embedded (ISO 8601, UTC)
```

Saving a note queues it; an idle-time worker re-embeds only the chunks whose
text changed, and writes nothing to the note except an `:ID:` the first time
a heading is embedded (its key). Queries read an in-memory index loaded from
the cache. Changing `org-roam-semantic-embedding-model` makes every chunk
stale for the new model; old rows keep serving the old model until
`M-x orsb-search-prune-cache` removes them (along with rows for nodes that no
longer exist). The file is a cache: delete it and
`M-x orsb-search-generate-all` rebuilds it.

### Moving a vault off in-note embeddings

Vaults from before 2.1 carry `:EMBEDDING:` and `:EMBEDDING_HASH:` lines in
their notes. Two commands, run on the machine that runs the server:

1. `M-x orsb-search-migrate-properties` copies those vectors into the cache.
   It reads the notes and writes nothing to them; running it twice is
   harmless. Vectors in a drawer without an `:ID:` are left for the worker to
   re-embed.
2. When you are happy with search, `M-x orsb-search-strip-properties` removes
   the lines from the notes (`C-u` first for a dry run that only counts). It
   removes a vector only when the cache holds a row for that node, and skips
   notes with unsaved edits. This rewrites each affected note once, so let
   Syncthing settle before editing elsewhere.

Until step 2, other devices still see the old lines; nothing reads them.

## The MCP server

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

What the server offers an assistant, tool by tool, is on the [tools page](tools.md).

## Agenda

```elisp
(with-eval-after-load 'org-agenda (orsb-agenda-setup))
```

adds agenda views over `projects/` and `admin/` and journals TODO state
changes into the daily note.

## Updating a running server

```sh
git pull            # in the package checkout
make reload         # emacsclient -f/-s your server: reload modules, restart the MCP server
```

Sessions survive a restart; clients that were mid-session simply
re-initialize.

## The modules

| Module | What it is |
|---|---|
| `orsb-core.el` | Every capability as a plain function: resolve a note by id, path or title; read and write the `:PROPERTIES:` drawer; create notes; edit sections with a destructive-replace guard; daily notes, inbox, follow-ups, blog status. |
| `orsb-tools.el` | The 22-tool MCP contract on top of the core: one `id` parameter, one `{ok, data | error}` envelope with a hint on every error, the vault's own rules applied to every write, proposals, and an undo journal behind every write. `MIGRATION.md` maps the tool names of earlier versions onto these. |
| `org-roam-mcp-http.el` | The MCP server (Streamable HTTP, POST): per-client sessions, optional bearer token, tool timeout, request log. |
| `orsb-schema.el` | Note types and their rules, read from the vault's `_schema.org` files: inheritance down the folders, the check of existing notes, the rules a write must keep. Needs no org-roam database. |
| `orsb-embed-cache.el` | The embedding store: one SQLite file beside the notes (not in them), one row per node and model. |
| `orsb-search.el` | Semantic search: stale detection by content hash, an in-memory index loaded from the cache, an idle-time worker that re-embeds what changed, and the migration from in-note vectors. |
| `org-roam-vector-search.el` | The embedding primitives and the interactive search commands. |
| `org-roam-second-brain.el` | The human side: `orsb-mode` (`C-c b` and `C-c v`). `sb/new` creates a note of any type the vault defines, asking for what that type requires; `sb/digest`, `sb/open`, `sb/stale` and `sb/list` show unfinished work and notes by type; `sb/proposals` reviews changes waiting for approval (RET preview, `a` approve, `r` reject); plus the inbox, title search, link suggestions and daily-note linking. It goes through the same tool functions as the MCP server, so the same rules apply. |
| `orsb-blog.el` | Optional: the `sb/blog-*` commands for a folder of ox-hugo posts (`orsb-blog-directory`), under `C-c b B` once loaded. |
| `org-agenda-project-tracking.el` | Optional agenda views and TODO journaling (`orsb-agenda-setup`). |

# Changelog

What changed in each release, newest first. Changes that need anything from you, such as
a tool or a command that works differently, are marked **Changed**.
[MIGRATION.md](MIGRATION.md) maps the tools and arguments of earlier versions onto the
current ones.

## Unreleased

- The package states its licence: GPL-3.0-or-later, in `LICENSE` and in each file's
  header.
- The documentation is split: a short README, and the guide, the tools page and the FAQ
  under `docs/`, which is also the project's page at
  https://dcruver.github.io/org-roam-second-brain/. `SETUP.md` is now part of the guide.
- A recorded clip of an assistant filing a proposal and Emacs approving it
  (`demo/record.sh`).
- `org-roam-second-brain.el` and `org-roam-vector-search.el` declare Emacs 29.1, which
  the package has needed since 2.0.

## 3.1.3 (2026-11-26)

- `move_node` with `folder` moves a whole note into another folder of the vault, and so
  to that folder's type. Links keep working.

## 3.1.2 (2026-11-26)

- An `:ID:` or a heading inside an example or source block is text, not a note or a
  section.
- `reassign_id` takes `id` for a shared id that sits on a heading inside the note.

## 3.1.1 (2026-11-26)

- A note's own `#+KEY:` line meets a required property of the same name.

## 3.1.0 (2026-11-26)

- Proposals that belong together share a `batch` label; `decide_proposal` with `batch`
  decides all of them, and `B` on the approval screen does the same.
- `reassign_id` gives a note that shares its id with another note an id of its own, so
  both are visible to search and links.

## 3.0.0 (2026-11-26)

A breaking release: the vault defines its own note types.

- **Changed:** the package has no built-in note types. A folder is a type when it holds
  a `_schema.org`; rules are inherited down the folders. A vault without one has a
  single type, `note`, and no rules. `NODE-TYPE` is no longer read or written.
- **Changed:** status values come from the type's schema (`LIFECYCLE`,
  `LIFECYCLE_VALUES`, `LIFECYCLE_FINISHED`); the built-in lists and `M-x
  orsb-migrate-status` are gone.
- **Changed:** `create_node` takes `properties`; the type-specific arguments are gone.
  Starting sections, defaults and keyword lines come from the type's schema.
- The vault's rules apply to every write: `ENFORCE` is `strict`, `warn` or `off`, and is
  inherited per folder. `check_schema` lists where existing notes depart from the rules.
- Proposals: any write takes `propose: true`; `get_proposals` and `decide_proposal`
  list and decide them; `sb/proposals` (`C-c b A`) is the approval screen in Emacs, and
  `orsb-server-url` lets an Emacs beside a headless server use it.
- The Emacs commands know no note types: `sb/new` creates a note of any type the vault
  defines. The blog commands moved to the optional `orsb-blog.el`.
- Templates take `%BODY%`; keyword lines take `%TAGS%`; `create_node` takes extra
  `keywords`.
- The small-model test kit runs on a sample vault with schema files, with five chores
  for the vault's rules.

## 2.2.2 (2026-11-25)

- An empty section's body no longer reaches into the next heading.
- A JSON null is written as null, not as the string `json-null`.

## 2.2.1 (2026-11-20)

- `orsb-replace-backup` makes the `.bak` copy before a whole-note replace optional.

## 2.2.0 (2026-11-19)

- **Changed:** one tool set. The 2.0 names, the tool profiles, `sync`,
  `get_blog_status` and `get_graph` are removed.
- `GET /graph` answers the whole graph, every node and id link, as a plain HTTP
  endpoint for a program that draws it.

## 2.1.0 (2026-11-19)

- A 19-tool set for small models, with an undo journal (`restore`) and `dry_run` on
  writes.
- Embeddings move out of the notes into a SQLite file (`orsb-embed-cache-file`), with
  commands to migrate a vault that carries `:EMBEDDING:` lines.
- A test kit that gives a model everyday vault chores and scores them on disk
  (`make eval`).

## 2.0.0 (2026-09-07)

- **Changed:** one core (`orsb-core.el`) and one tool contract (`orsb-tools.el`): one
  `id` parameter, one `{ok, data | error}` envelope, a hint on every error. The 1.x API
  is removed.
- The server keeps a session per client, takes an optional bearer token, times out a
  tool, logs every request, and binds to a host you choose.
- Semantic search detects stale embeddings by content hash and re-embeds in idle time.
- Emacs 29.1 is the floor (builtin SQLite). Tests, byte-compilation and lint run in CI
  on Emacs 29 and 30.

## 1.x (2026-01-14 to 2026-09-07)

The first releases: typed notes with a daily digest, semantic link suggestions, and an
MCP server, at first in Python and from March 2026 in Emacs Lisp
(`org-roam-mcp-http.el`).

# org-roam-second-brain

A second brain on [org-roam](https://www.orgroam.com/) that you and an AI assistant
both keep.

Your notes stay plain org files in a folder. The package adds three things to them.
The vault describes its own note types, in small `_schema.org` files: what a project
or a person note must have, which sections may only grow, and what may change only
with your approval. An **MCP server**, written in Emacs Lisp and running inside
Emacs, gives an assistant 23 tools over the same notes, sized for small local models:
one way to name a note, one answer shape, and a hint on every error. And every write,
yours or the assistant's, follows the vault's rules, can be previewed, and can be
undone.

![Claude Code, connected to the notes server, answers a question from the vault and is asked to record a decision in a project; the project type's rules make that a proposal, which appears in the Emacs digest, is previewed and approved on the approval screen, and is then in the note](https://raw.githubusercontent.com/dcruver/org-roam-second-brain/main/docs/images/demo.gif)

```elisp
(use-package org-roam-second-brain
  :after org-roam
  :config
  (orsb-mode 1)        ; C-c b: the digest, new notes, the approval screen
  (orsb-mcp-start))    ; http://127.0.0.1:8007/mcp
```

```sh
claude mcp add --transport http notes http://localhost:8007/mcp
```

Status: version 3. It keeps my own vault every day, mostly through an assistant on a
local model. See "Where it stands" at the end.

## What it does

- **The vault defines its note types.** A folder is a type when it holds a
  `_schema.org`: required properties and their forms, the status values and which of
  them mean finished, the sections a note starts with, and a rule for each section
  (`replace`, `append` or `propose`). Rules are inherited down the folders. The
  package has no types of its own.
- **Rules apply to every write.** A write that breaks a rule comes back with
  `warnings`, or is refused in a folder marked `strict`. `check_schema` lists where
  existing notes depart from the rules and changes nothing.
- **Changes that need your approval wait for it.** A section or a whole type can be
  marked `propose`. The assistant files the change with a reason; you see its
  line-by-line preview in Emacs (`C-c b A`) and approve or reject it. Nothing is
  written until you do.
- **Every write can be undone.** Each one returns a `change_id`, and `restore` puts
  every file it touched back. Most writes take `dry_run`.
- **Tools sized for small models.** 23 tools; a note is named by its id, path, title
  or `today`; answers are `{ok, data}` or `{ok, error}` with a hint saying what to do
  next. The repository carries a test kit that gives a model nineteen everyday vault
  chores and reads the files afterwards to see whether each was really done.
- **Search by meaning.** Embeddings from any OpenAI-compatible endpoint, kept in one
  SQLite file beside the notes, not in them, and refreshed in idle time. Optional:
  without an endpoint, word and title search still work.
- **A digest in Emacs.** Unfinished notes of every type with a lifecycle, the stale
  ones, tasks due, the inbox, and what waits for approval.
- **Nothing but Emacs.** No Python, no Node, no container. The only outside service is
  the embeddings endpoint, and it is optional.

The longer story of sharing one set of notes with an assistant is in the blog post
[A Second Brain That My AI and I Share](https://hullabalooing.cruver.ai/second-brain/posts/a-second-brain-that-my-ai-and-i-share/),
written when the package was at version 1.

## Install

Emacs 29.1 or newer (for the builtin SQLite) and org-roam 2.2 or newer.

Doom Emacs (`packages.el`, then `config.el`):

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

Emacs 29's own `package-vc`:

```elisp
(package-vc-install "https://github.com/dcruver/org-roam-second-brain")
```

Update with `doom sync -u`, `M-x straight-pull-package` or `M-x package-vc-upgrade`.
A particular release is a tag, `v3.2.0` for example; the
[releases](https://github.com/dcruver/org-roam-second-brain/releases) say what each
one changed. It is not on MELPA yet.

## A vault, in a few steps

`orsb-mode` binds `C-c b` (the second-brain commands) and `C-c v` (search by
meaning). With no `_schema.org` anywhere, every note is a `note` and nothing is
checked, so you can start with the vault you have.

Give a folder a type by putting a `_schema.org` in it:

```org
* Type
:PROPERTIES:
:TYPE: project
:LIFECYCLE: STATUS
:LIFECYCLE_VALUES: active waiting done
:LIFECYCLE_FINISHED: done
:END:

* Properties
| property    | required | form                    | default |
|-------------+----------+-------------------------+---------|
| STATUS      | yes      | one of LIFECYCLE_VALUES | active  |
| NEXT-ACTION | yes      | one sentence            |         |

* Sections
| section       | required | rule    |
|---------------+----------+---------|
| Current state | yes      | replace |
| Log           | yes      | append  |
| Decisions     | no       | propose |
```

Now `C-c b n` offers `project` and asks for what it requires, the digest (`C-c b d`)
lists the projects that are not `done`, an assistant can add to `Log` but not rewrite
it, and a change to `Decisions` waits on the approval screen (`C-c b A`). The
[guide](https://github.com/dcruver/org-roam-second-brain/blob/main/docs/guide.md#note-types-_schemaorg)
has the whole format: forms, defaults, templates, inheritance, and `strict`, `warn`
and `off`.

For search by meaning, point it at an embeddings endpoint (Ollama here) and embed the
vault once with `M-x orsb-search-generate-all`:

```elisp
(setq orsb-embedding-url "http://localhost:11434/v1"
      orsb-embedding-model "nomic-embed-text")
```

## Using it from an AI assistant

Start the server in a long-lived Emacs (a daemon is ideal):

```elisp
(setq orsb-mcp-host "127.0.0.1"    ; "0.0.0.0" to serve your LAN
      orsb-mcp-port 8007
      orsb-mcp-auth-token nil)     ; a string requires Authorization: Bearer
(orsb-mcp-start)
```

Then point any MCP client that speaks Streamable HTTP at `http://host:8007/mcp`. With
Claude Code:

```sh
claude mcp add --transport http notes http://localhost:8007/mcp
```

The server tells the model how to behave when it connects (search before creating,
undo with `restore`, follow the hints, never approve its own proposals), so there is
no skill or prompt to install. The [tools page](https://github.com/dcruver/org-roam-second-brain/blob/main/docs/tools.md)
lists the 23 tools. The server cannot tell callers apart: if it listens beyond your
own machine, set a token, and put whatever decides who may approve in front of it.

## Where it stands

Version 3.1. The tool contract was rewritten for 2.0 and again for 3.0, and both were
breaking: [MIGRATION.md](https://github.com/dcruver/org-roam-second-brain/blob/main/MIGRATION.md)
maps the old names onto the new. The tests run on Emacs 29 and 30 with each push. It
has kept one vault so far, mine, so a vault laid out differently will find rough
edges; an [issue](https://github.com/dcruver/org-roam-second-brain/issues) saying what
you hit is welcome. Not done yet: MELPA, and any way for the server to tell one caller
from another. The [FAQ](https://github.com/dcruver/org-roam-second-brain/blob/main/docs/faq.md#what-is-not-done-yet)
has the full list.

## Documentation

- [Guide](https://github.com/dcruver/org-roam-second-brain/blob/main/docs/guide.md): setup, the `_schema.org` format, proposals,
  the Emacs commands, semantic search, running the server.
- [Tools](https://github.com/dcruver/org-roam-second-brain/blob/main/docs/tools.md): the MCP server and its 23 tools.
- [FAQ](https://github.com/dcruver/org-roam-second-brain/blob/main/docs/faq.md): what it needs, what it does to your notes, and
  what is not done yet.
- [MIGRATION.md](https://github.com/dcruver/org-roam-second-brain/blob/main/MIGRATION.md): coming from 1.x or 2.x.
- [eval/README.md](https://github.com/dcruver/org-roam-second-brain/blob/main/eval/README.md): the small-model test kit.
- [CHANGELOG.md](https://github.com/dcruver/org-roam-second-brain/blob/main/CHANGELOG.md): what changed in each release.

## Contributing

[CONTRIBUTING.md](https://github.com/dcruver/org-roam-second-brain/blob/main/CONTRIBUTING.md) says how to run the tests, what a
change needs, and what help is most wanted (using it on a vault that is not mine,
above all).

## License

GPL-3.0-or-later, like org-roam and Emacs. See [LICENSE](https://github.com/dcruver/org-roam-second-brain/blob/main/LICENSE).

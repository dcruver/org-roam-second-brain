# FAQ

- [What do I need?](#what-do-i-need)
- [Do I need an AI assistant to use it?](#do-i-need-an-ai-assistant-to-use-it)
- [Which assistants work?](#which-assistants-work)
- [What does it change in my notes?](#what-does-it-change-in-my-notes)
- [Do I have to reorganise my vault?](#do-i-have-to-reorganise-my-vault)
- [Is the server safe to expose?](#is-the-server-safe-to-expose)
- [Can the assistant approve its own proposals?](#can-the-assistant-approve-its-own-proposals)
- [My notes sync between machines. Where does the server run?](#my-notes-sync-between-machines-where-does-the-server-run)
- [How well do small models manage?](#how-well-do-small-models-manage)
- [What is not done yet?](#what-is-not-done-yet)

## What do I need?

Emacs 29.1 or newer and org-roam 2.2 or newer. Emacs 29 is the floor because org-roam
and the embedding store use the SQLite built into it. Nothing else is installed: no
Python, no Node, no container. Search by meaning needs an OpenAI-compatible
`/embeddings` endpoint (Ollama, Infinity, llama.cpp's server, a hosted one); without
it, word and title search still work.

## Do I need an AI assistant to use it?

No. `orsb-mode` on its own gives you typed notes with rules, the digest, stale and
open listings, the inbox and search. The MCP server is started separately
(`orsb-mcp-start`) and only if you want it.

## Which assistants work?

Any MCP client that speaks Streamable HTTP: Claude Code, Claude Desktop, and agents
built on a local model behind llama.cpp, vLLM or Ollama. The server answers POST only
and opens no event stream. The tools were shaped for small local models, so few
choices and plain answers come first.

## What does it change in my notes?

Only what a command or a tool call asks for, plus one thing: the first time a heading
is embedded for search, it gets an `:ID:` if it had none, because the id is its key in
the embedding store. The vectors themselves are kept in a SQLite file outside the
vault. Every write through a tool is recorded with the files as they were, so
`restore` can put them back; that journal is outside the vault too
(`orsb-change-directory`).

## Do I have to reorganise my vault?

No. Without any `_schema.org`, every note is a `note` and nothing is checked. Types
are added a folder at a time, and with `ENFORCE: warn` (the default) a note that
breaks a rule is reported, not refused. `check_schema` shows where the notes you
already have depart from rules you have just written, before you tighten anything.

## Is the server safe to expose?

It listens on `127.0.0.1` unless you set `orsb-mcp-host`. Beyond your own machine, set
`orsb-mcp-auth-token`, which makes every request carry `Authorization: Bearer`, and
remember the traffic is plain HTTP: keep it on a network you trust or behind a proxy
that adds TLS. Anyone who can call the server can read and change the vault.

## Can the assistant approve its own proposals?

The server cannot tell one caller from another, so `decide_proposal` works for
whoever calls it. Two things stand in the way: the guidance the server sends at
connect tells the model that approving is the owner's decision, and you can keep
`decide_proposal` out of the tools you allow the assistant (most clients take an
allowlist). A hard guarantee needs something in front of the server that knows who is
calling.

## My notes sync between machines. Where does the server run?

On one machine, in a long-lived Emacs. Proposals and the undo journal belong to that
server. An Emacs on another machine can still review proposals: set `orsb-server-url`
to the server's address and `C-c b A` lists, previews and decides them there. Keep
the embedding store out of the synced folder, and let the sync settle before editing
a note the server has just written.

## How well do small models manage?

Measure it on yours. `make eval` gives a model nineteen everyday chores (capture an
idea, merge two notes, tick a box, undo a bad edit, record a decision that needs
approval), each on a fresh copy of a sample vault, and then reads the files to see
whether the chore was really done and nothing else was touched.
[eval/README.md](https://github.com/dcruver/org-roam-second-brain/blob/main/eval/README.md)
has the details.

## What is not done yet?

- **MELPA.** Install from the repository for now.
- **Callers the server can tell apart.** One token, one level of access.
- **A vault other than mine.** It has kept one vault so far. A layout it has not met
  will show rough edges; please open an issue.
- **TLS.** The server speaks plain HTTP.

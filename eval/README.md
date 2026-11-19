# Small-model chores

Can a small local model keep this second brain by itself? This directory is a
test kit for that question. It gives a model fourteen everyday vault chores
through the MCP tools, one at a time on a fresh copy of a sample vault, and
then reads the vault files to see whether the chore was really done.

## What it checks

| Chore | What the user says (shortened) | What has to be true afterwards |
|---|---|---|
| `capture_idea` | save an idea about a solar dehydrator and connect it to Garden | an `idea` note in `ideas/`, linked with Garden |
| `link_person` | Sam is helping with the coop; connect the two | Chicken coop links to Sam Lee |
| `merge_duplicate` | two notes about rainwater; merge into "Rain barrel system" | one note with both texts; the old id still resolves, so Garden's link works |
| `overdue_tasks` | what is overdue? | the reply names the fence gate and no finished task |
| `tick_checkbox` | bought the cedar boards; tick it off | that box `[X]`, its neighbours unchanged |
| `close_todo` | replaced the furnace filter; mark it done | that heading `DONE`, the other one untouched |
| `move_task` | the waterer task is in the wrong project | heading and its text now in Chicken coop, gone from Garden |
| `undo_bad_edit` | I wiped my Well pump note; undo it | the note is back as it was (the harness makes the bad edit first) |
| `answer_from_vault` | what is my well's pressure switch set to? | the reply says 40 and 60 |
| `add_task` | add "buy hardware cloth", due 2027-03-01, to the coop | a `TODO` heading with that `DEADLINE` |
| `set_status` | the fence repair is on hold | `STATUS: waiting` |
| `inbox_capture` | quick note: call the county about the survey | today's daily note has it |
| `link_related` | link notes related to the coop that aren't yet | coop links to Egg production log, and not to unrelated notes |
| `archive_note` | put the router notes away, don't delete them | the note is in `archive/`, not deleted |

The prompts never name a tool, an id or a file, so the model has to find
things on its own. Every chore also has a **no collateral damage** check: any
file the chore had no reason to touch must be byte for byte the same
afterwards, so a model that "fixes" other notes along the way fails.

For each run the harness records pass or fail, the share of checks passed,
tool calls, tool errors (the server answered `ok: false`), bad calls (a tool
that does not exist, or arguments that are not valid JSON), turns, seconds and
tokens.

## What you need

- Emacs 29.1+ with org-roam, the same setup `make test` uses. If org-roam is
  not where `package-initialize` finds it, set `ORSB_DEPS` as for `make test`
  (with Doom, the Makefile already guesses the straight.el build directory).
- Python 3.8 or newer. Nothing to install: the scripts use only the standard
  library.
- A chat model behind an OpenAI-compatible `/v1/chat/completions` endpoint
  that supports tool calling (the `tools` field in the request, `tool_calls`
  in the reply).

Notes for common local servers:

- **llama.cpp's server** (`llama-server`): start it with `--jinja` so it uses
  the model's own chat template, which is what turns tool definitions into a
  prompt and the model's output back into `tool_calls`.
- **vLLM**: add `--enable-auto-tool-choice --tool-call-parser <parser for
  your model family>` (for Qwen models, `hermes`).
- **Ollama**: use `http://host:11434/v1` as the base URL.
- If a server returns tool calls as text (`<tool_call>{...}</tool_call>`)
  instead of `tool_calls`, the harness still runs them and counts them as
  `text_tool_calls`; pass `--no-text-calls` to treat that as a failure.

## Run it

From the repository root:

```sh
python3 eval/run_eval.py --base-url http://localhost:8080/v1 --model qwen3.8-27b
```

or `make eval EVAL_ARGS="--base-url http://localhost:8080/v1 --model qwen3.8-27b"`.

Useful options:

| Option | What it does |
|---|---|
| `--chores merge_duplicate,undo_bad_edit` | run only these (`--list` shows all) |
| `--repeat 5` | run each chore five times; small models vary from run to run |
| `--temperature 0.2` | sampling temperature (default 0.2) |
| `--max-turns 15` | model replies allowed per chore before it counts as stuck |
| `--extra-body '{"top_p":0.9}'` | extra request fields your server understands |
| `--system-prompt my.txt` | your own system prompt; `{today}` is filled in |
| `--no-server-instructions` | leave out the guidance the MCP server sends at startup |
| `--embed-url http://localhost:11434/v1 --embed-model nomic-embed-text --embed-dims 768` | use your real embedding model for semantic search |
| `--no-embeddings` | no semantic search at all |
| `--keep-vaults` | keep each run's vault so you can open it in Emacs |
| `--api-key KEY` | sent as a bearer token, for servers that want one |

Each chore takes a second or two of Emacs startup plus however long the model
takes. Results land in `eval/results/<model>-<time>/`:

- `results.md`: a table, one row per run, with the failed checks spelled out;
- `results.json`: everything, including each check's detail and the final reply;
- `transcripts/<chore>.jsonl`: every model reply and tool call with its result,
  one JSON object per line, for working out why a chore failed.

## Semantic search during the test

Two tools search by meaning: `search` in its default mode and `find_related`.
They need embeddings. Unless you pass `--embed-url`, the harness starts
`stand_in_embedder.py`, a tiny embeddings server that scores texts by the
words they share. It keeps the test self-contained and fast; it is not a
measure of embedding quality. Before each chore the server embeds the sample
vault, and it re-embeds notes the model changes between requests.

## How it works

- `run_eval.py` copies `vault/` to a scratch directory, starts
  `emacs -Q --batch -l eval/serve.el` (the MCP server on that copy, on a free
  port, with its database, undo journal and search cache kept outside the
  vault), lists the tools, and offers them to the model as functions. Each
  tool call the model makes is sent to the MCP server and the answer goes back
  to the model unchanged (cut at `--max-result-chars`).
- When the model answers without calling a tool, or runs out of turns, Emacs
  is stopped and `vaultcheck.py` reads the files on disk. It parses just
  enough org syntax for the checks and never asks Emacs, so the score does not
  depend on the server's own view of the vault.
- `chores.py` holds the chores: the prompt, any setup calls, the checks, the
  files the chore may touch, and a known good sequence of tool calls.

## Checking the kit itself

No model needed:

```sh
make eval-selfcheck
```

runs every chore twice: once with the known good tool calls (every chore must
pass) and once doing nothing (every chore must fail). Run it after changing a
tool or a chore; if a tool's behaviour changes, the known good calls show it.

`mock_model.py` is a fake chat server that replays the known good calls
through the real model path, with options to return them as text and to throw
in bad calls, so the protocol handling and the counting can be checked too:

```sh
python3 eval/mock_model.py --port 8091 --faults &
python3 eval/run_eval.py --base-url http://127.0.0.1:8091/v1 --model mock
```

## Adding a chore

Add a dict to the list in `chores.py`: a prompt in the user's words, the
checks (types are listed at the top of `vaultcheck.py`), the `may_touch`
globs, and a `reference` list of tool calls that does the chore. If it needs
new notes, add them to `vault/` with a fixed `:ID:` (the existing ones start
with `eval-`) and check that the other chores' `may_touch` lists still hold.
Then run `make eval-selfcheck`.

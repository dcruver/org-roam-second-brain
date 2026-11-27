# Contributing to org-roam-second-brain

Thanks for helping. The most useful things right now, in order:

1. **Using it on your own vault, and saying how it went.** The package has kept one
   vault so far. If you try it on yours, open a
   [vault report](https://github.com/dcruver/org-roam-second-brain/issues/new?template=vault_report.yml):
   how the vault is laid out, which types you gave a `_schema.org`, the assistant and
   model if you use one, and what came out right or wrong.
2. **Running the chores with your model.** `make eval` gives a model nineteen everyday
   vault chores and reads the files afterwards. Which chores a model fails, and how, is
   what shapes the tools. [eval/README.md](eval/README.md) says how to run it.
3. **Bugs and rough edges** in the tools and the Emacs commands. If something surprised
   you, that is worth an issue too.

## Running the tests

You need Emacs 29.1 or newer with org-roam, and Python 3.8 or newer for the eval
harness (standard library only).

```
git clone https://github.com/dcruver/org-roam-second-brain.git && cd org-roam-second-brain
make test             # ERT against a throwaway vault
make compile          # byte-compile everything, warnings shown
make lint             # warnings are errors for the orsb-* modules
make eval-selfcheck   # the known good tool calls pass every chore; doing nothing fails every chore
```

The Makefile looks for org-roam in a Doom straight.el build directory. Anywhere else,
set `ORSB_DEPS` to the directory whose subdirectories hold org-roam and its
dependencies (`~/.emacs.d/elpa`, for example), or `ORSB_DEPS=""` to use what
`package-initialize` finds, as CI does. `make reload` loads the working copy into a
running Emacs server and restarts the MCP server.

## Changing it

- The tools are for small models first: few tools, one way to name a note, and an
  error whose hint says what to do next. A new tool or argument needs a reason a small
  model would benefit from; a wording change to a description or a hint is a real
  change, so run the chores.
- Capabilities live in `orsb-core.el` as plain functions; `orsb-tools.el` is the
  contract on top, and the Emacs commands in `org-roam-second-brain.el` go through the
  same tool functions, so the vault's rules apply to both. The package defines no note
  types: anything about a type belongs in a `_schema.org`, not in code.
- Write code like the code around it: its naming, its comment density, and comments
  that say why rather than what.
- Add or change tests with the code (`test/`). The tests use a throwaway vault and no
  network. After changing a tool or a chore, run `make eval-selfcheck`.
- Update the [guide](docs/guide.md) when a command, an option or the `_schema.org`
  format changes, the [tools page](docs/tools.md) when a tool changes (the README only
  when its short tour or install lines are affected), and the Unreleased section of
  [CHANGELOG.md](CHANGELOG.md), marking anything that changes how an existing tool or
  command works as **Changed**. A change that breaks callers also goes in
  [MIGRATION.md](MIGRATION.md).

## The site and the README's clip

`docs/` is the project's page, served by GitHub Pages from `main` at
https://dcruver.github.io/org-roam-second-brain/: `docs/index.html`, one hand-written
page with no build step, and `docs/images/`, which the README's clip lives in too, so
one copy serves both. The documentation is there as well: `docs/guide.md`,
`docs/tools.md` and `docs/faq.md`. The README is the short version; it links the
repository's files by their github.com addresses, so that it reads the same wherever
it is shown. Keep the page to what the README's first screen says.

`docs/images/demo.gif` is recorded, not drawn. `demo/record.sh` starts the notes
server on a fresh copy of the sample vault (`demo/serve.sh`, which runs
`eval/serve.el` with the stand-in embedder), then runs `demo/demo.tape`: two real
requests to Claude Code, connected to that server, and then a bare Emacs with
`demo/init.el`, where the proposal is previewed and approved. Claude Code prints
nothing while it works, so the script cuts those stills short; nothing else is edited.
It needs [VHS](https://github.com/charmbracelet/vhs) with `ttyd`, `ffmpeg`, Emacs with
org-roam, and Claude Code signed in. Because the requests are real, the wording of the
answers differs between recordings. Re-record it when a screen in the clip changes.

## Licence

org-roam-second-brain is licensed GPL-3.0-or-later. By contributing, you agree that
your contribution is licensed the same way.

## Releases

For the maintainer: set `Version:` in the file headers and `orsb-tools-version`, move
CHANGELOG.md's Unreleased section under that version, commit, and push a tag `vX.Y.Z`
with the same version. The release workflow checks that the tag and
`orsb-tools-version` agree and publishes the GitHub release with that version's
section of the changelog as its notes. A new tool or argument is a minor version; only
a fix is a patch.

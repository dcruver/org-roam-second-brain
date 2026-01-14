# org-roam-second-brain

A "Second Brain" system for [org-roam](https://www.orgroam.com/), providing structured note types, semantic search, and proactive knowledge surfacing.

## Features

### Structured Node Types
Create notes with predefined structures:
- **Person** - Track people with context, follow-ups, and notes
- **Project** - Manage projects with status, next actions, and notes
- **Idea** - Capture ideas with one-liners and elaboration
- **Admin** - Track administrative tasks with due dates

### Proactive Surfacing
- **Daily Digest** - See active projects, pending follow-ups, and stale items at a glance
- **Stale Projects** - Find projects that haven't been touched recently
- **Pending Follow-ups** - Track unchecked items mentioning people
- **Dangling Links** - Find `[[Name]]` links without corresponding person nodes

### Semantic Search
- **Link Suggestions** - Find semantically similar notes that aren't already linked
- **Vector Embeddings** - Store embeddings in org properties for portable, git-trackable notes

## Installation

### Using straight.el (recommended)

```elisp
(straight-use-package
 '(org-roam-second-brain :host github :repo "dcruver/org-roam-second-brain"))
```

### Using Doom Emacs

```elisp
;; In packages.el
(package! org-roam-second-brain
  :recipe (:host github :repo "dcruver/org-roam-second-brain"))

;; In config.el
(use-package! org-roam-second-brain
  :after org-roam)
```

## Configuration

Configure the embedding service URL (required for semantic features):

```elisp
(setq org-roam-semantic-embedding-url "http://localhost:7997")
(setq org-roam-semantic-embedding-model "nomic-ai/nomic-embed-text-v1.5")
```

Or use `M-x customize-group RET org-roam-vector-search RET`.

### Second Brain Options

```elisp
(setq sb/stale-days 5)                    ; Days before project is "stale"
(setq sb/similarity-threshold 0.6)        ; Minimum similarity for suggestions
(setq sb/show-digest-on-startup t)        ; Show digest when Emacs starts
(setq sb/proactive-suggestions t)         ; Show suggestion hints when visiting notes
```

Or use `M-x customize-group RET sb RET`.

## Usage

### Key Bindings (C-c b prefix)

| Key | Command | Description |
|-----|---------|-------------|
| `C-c b p` | `sb/person` | Create a person node |
| `C-c b P` | `sb/project` | Create a project node |
| `C-c b i` | `sb/idea` | Create an idea node |
| `C-c b a` | `sb/admin` | Create an admin task |
| `C-c b I` | `sb/inbox` | Log to inbox |
| `C-c b d` | `sb/digest` | Show daily digest |
| `C-c b f` | `sb/followups` | Show pending follow-ups |
| `C-c b s` | `sb/stale` | Show stale projects |
| `C-c b u` | `sb/dangling` | Show untracked people |
| `C-c b L` | `sb/suggest-links` | Find unlinked similar notes |
| `C-c b w` | `sb/weekly` | Show weekly review |
| `C-c b /` | `sb/search` | Search notes by title |
| `C-c b l p` | `sb/projects` | List all projects |
| `C-c b l e` | `sb/people` | List all people |
| `C-c b l i` | `sb/ideas` | List all ideas |

### Buffer Navigation (in result buffers)

| Key | Description |
|-----|-------------|
| `j` / `n` | Next item |
| `k` / `p` | Previous item |
| `RET` | Open item |
| `g` / `gr` | Refresh |
| `q` | Quit |
| `c` | Create person (for dangling links) |

## Embedding Service

Semantic features require an embedding service. Options:

### Infinity (recommended)
```bash
docker run -p 7997:7997 michaelf34/infinity:latest \
  --model-id nomic-ai/nomic-embed-text-v1.5 --port 7997
```

### Ollama
```bash
ollama pull nomic-embed-text
# Uses port 11434 by default
```

Then configure:
```elisp
(setq org-roam-semantic-embedding-url "http://localhost:11434/v1")
(setq org-roam-semantic-embedding-model "nomic-embed-text")
```

## Package Contents

- `org-roam-vector-search.el` - Vector embeddings and semantic search
- `org-roam-second-brain.el` - Structured notes and proactive surfacing

## License

GPL-3.0

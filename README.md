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
- **Daily Digest** - See active projects, pending follow-ups, stale items, and blog status at a glance
- **Stale Projects** - Find projects that haven't been touched recently
- **Pending Follow-ups** - Track unchecked items mentioning people
- **Dangling Links** - Find `[[Name]]` links without corresponding person nodes
- **Blog Status** - Track drafts, published posts, and ideas that could become posts

### Blog Management
- **Create Posts** - Create blog posts with Hugo export metadata
- **Draft Tracking** - Track outline progress and content status
- **AI Assistance** - Generate outlines, expand sections, adjust tone
- **Publish Workflow** - Validate and publish posts

### Daily Auto-Linking
- **Semantic Connections** - Automatically link daily notes to related concepts
- **Cross-Day Links** - Find connections between daily entries across time

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
```

```elisp
;; In config.el
(setq org-roam-directory (file-truename "~/org-roam"))

;; Load org-roam-second-brain after org-roam
(use-package! org-roam-second-brain
  :after org-roam)

;; Configure embedding server (required for semantic features)
(after! org-roam-vector-search
  (setq org-roam-semantic-embedding-url "http://localhost:8080")
  (setq org-roam-semantic-embedding-model "nomic-ai/nomic-embed-text-v1.5"))
```

Then run `doom sync`.

### Updating the Package

To update to the latest version:

```bash
doom sync -u
```

Or rebuild just this package:

```elisp
M-x straight-rebuild-package RET org-roam-second-brain RET
```

Or via command line:
```bash
emacsclient --eval '(straight-rebuild-package "org-roam-second-brain")'
```

## Configuration

Configure the embedding service URL (required for semantic features):

```elisp
(setq org-roam-semantic-embedding-url "http://localhost:8080")
(setq org-roam-semantic-embedding-model "nomic-ai/nomic-embed-text-v1.5")
```

Or use `M-x customize-group RET org-roam-vector-search RET`.

### Second Brain Options

```elisp
(setq sb/stale-days 5)                    ; Days before project is "stale"
(setq sb/similarity-threshold 0.6)        ; Minimum similarity for suggestions
(setq sb/show-digest-on-startup t)        ; Show digest when Emacs starts
(setq sb/proactive-suggestions nil)       ; Show suggestion hints when visiting notes (off by default)
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
| `C-c b B` | `sb/blog` | Create a blog post |
| `C-c b l b` | `sb/blog-list` | List all blog posts |
| `C-c b D l` | `sb/daily-link` | Link current daily to related notes |
| `C-c b D L` | `sb/daily-link-all` | Batch link all daily files |
| `C-c b D c` | `sb/daily-connections` | Show daily connections report |

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
docker run -p 8080:7997 michaelf34/infinity:latest \
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

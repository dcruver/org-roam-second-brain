# org-roam-second-brain Setup Guide

Complete setup for a new org-roam second brain with semantic search and MCP server.

## Prerequisites

- Emacs 27.1+
- org-roam 2.2+
- Python 3.10+ (for MCP server)
- Docker or Ollama (for embeddings)

## Step 1: Install Emacs Packages

### Doom Emacs

```elisp
;; packages.el
(package! org-roam-second-brain
  :recipe (:host github :repo "dcruver/org-roam-second-brain"))
```

```elisp
;; config.el
(setq org-roam-directory "~/org-roam/")

(use-package! org-roam-second-brain
  :after org-roam
  :config
  (require 'org-roam-api))  ; for MCP server support

(after! org-roam-vector-search
  (setq org-roam-semantic-embedding-url "http://localhost:8080")
  (setq org-roam-semantic-embedding-model "nomic-ai/nomic-embed-text-v1.5"))
```

Then: `doom sync`

### Vanilla Emacs / straight.el

```elisp
(straight-use-package
  '(org-roam-second-brain :host github :repo "dcruver/org-roam-second-brain"))

(require 'org-roam-second-brain)
(require 'org-roam-api)

(setq org-roam-semantic-embedding-url "http://localhost:8080")
(setq org-roam-semantic-embedding-model "nomic-ai/nomic-embed-text-v1.5")
```

## Step 2: Embedding Server

Semantic search requires an embedding model. Choose one:

### Option A: Infinity (recommended for GPU)

```bash
docker run -d --name infinity \
  -p 8080:7997 \
  --gpus all \
  michaelf34/infinity:latest \
  --model-id nomic-ai/nomic-embed-text-v1.5 \
  --port 7997
```

CPU-only:
```bash
docker run -d --name infinity \
  -p 8080:7997 \
  michaelf34/infinity:latest \
  --model-id nomic-ai/nomic-embed-text-v1.5 \
  --port 7997
```

Config:
```elisp
(setq org-roam-semantic-embedding-url "http://localhost:8080")
(setq org-roam-semantic-embedding-model "nomic-ai/nomic-embed-text-v1.5")
```

### Option B: Ollama (simpler setup)

```bash
# Install Ollama (https://ollama.ai)
curl -fsSL https://ollama.ai/install.sh | sh

# Pull embedding model
ollama pull nomic-embed-text
```

Config:
```elisp
(setq org-roam-semantic-embedding-url "http://localhost:11434/api")
(setq org-roam-semantic-embedding-model "nomic-embed-text")
```

### Verify Embedding Server

```bash
# Infinity
curl http://localhost:8080/models

# Ollama
curl http://localhost:11434/api/tags
```

## Step 3: Emacs Daemon (for MCP server)

The MCP server needs emacsclient access:

```bash
# Start Emacs daemon
emacs --daemon

# Or with custom server file location
emacs --daemon --eval '(setq server-name "~/emacs-server/server")'
```

For systemd (recommended):

```ini
# ~/.config/systemd/user/emacs.service
[Unit]
Description=Emacs Daemon
After=network.target

[Service]
Type=forking
ExecStart=/usr/bin/emacs --daemon
ExecStop=/usr/bin/emacsclient --eval '(kill-emacs)'
Restart=on-failure

[Install]
WantedBy=default.target
```

```bash
systemctl --user enable emacs
systemctl --user start emacs
```

## Step 4: MCP Server (optional)

For external tool integration (n8n, chatbots, etc.):

```bash
# Install
pip install org-roam-mcp

# Run (point to your Emacs server)
export EMACS_SERVER_FILE=~/emacs-server/server
org-roam-mcp --port 8001
```

For systemd:

```ini
# ~/.config/systemd/user/org-roam-mcp.service
[Unit]
Description=Org-Roam MCP Server
After=emacs.service
Requires=emacs.service

[Service]
Environment=EMACS_SERVER_FILE=%h/emacs-server/server
ExecStart=%h/.local/bin/org-roam-mcp --port 8001
Restart=on-failure

[Install]
WantedBy=default.target
```

## Step 5: Generate Embeddings

After setup, generate embeddings for existing notes:

```elisp
M-x org-roam-semantic-generate-all-embeddings
```

Or via MCP:
```bash
curl -X POST http://localhost:8001 \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"generate_embeddings","arguments":{}}}'
```

## Verification Checklist

- [ ] `M-x sb/digest` shows daily digest
- [ ] `M-x org-roam-semantic-search` returns results
- [ ] `emacsclient --eval '(+ 1 1)'` returns 2
- [ ] `curl http://localhost:8001` returns health check (if MCP enabled)

## Troubleshooting

| Issue | Check |
|-------|-------|
| No semantic results | Embedding server running? Embeddings generated? |
| MCP tools fail | Emacs daemon running? EMACS_SERVER_FILE correct? |
| Slow embedding generation | Use GPU-enabled Infinity container |
| org-roam-api not found | Did you `(require 'org-roam-api)`? |

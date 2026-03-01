# Org-roam MCP Server

**[org-roam-ai](../README.md) > MCP Server**

An MCP (Model Context Protocol) server that provides clean, typed tool interfaces for AI agents to interact with an org-roam knowledge management system running in Emacs.

## Overview

This server wraps existing org-roam elisp functions via `emacsclient`, providing structured MCP tools that can be used by AI agents (particularly designed for n8n AI Agent nodes) instead of complex shell command workflows.

## Features

### Search Tools

- **semantic_search** - Vector-based semantic search using AI embeddings (cosine similarity)
- **contextual_search** - Enhanced search with full content, backlinks, and relevance scores for RAG
- **search_notes** - Basic search across note titles and content

### Note Creation

- **create_note** - Create org-roam notes with optional structured formatting (supports video notes)
- **create_person** - Create person node with context, follow-ups, and notes
- **create_project** - Create project node with status tracking and next actions
- **create_idea** - Create idea node with one-liner summary and elaboration
- **create_admin** - Create admin task node with due dates

### Daily Notes & Inbox

- **add_daily_entry** - Add structured entries to daily notes (journal or TODO types)
- **get_daily_content** - Retrieve daily note content by date
- **add_inbox_entry** - Log command audit trail to inbox
- **log_to_inbox** - Log text to inbox; auto-creates person nodes for [[Name]] links

### Proactive Surfacing

- **get_digest_data** - All digest data (projects, follow-ups, stale) in one call
- **get_active_projects** - Active projects with next actions, ordered by last modified
- **get_stale_projects** - Projects with no activity in N days
- **get_pending_followups** - People with unchecked follow-up items
- **get_dangling_followups** - Untracked people mentioned in follow-ups
- **get_weekly_inbox** - Inbox entries from past N days

### Maintenance

- **generate_note_embedding** - Generate embedding for a single note (fast, use after create/update)
- **generate_embeddings** - Batch embedding generation for all notes (slow, use sparingly)
- **sync_database** - Sync org-roam database

## Requirements

- Python 3.8+
- Emacs with org-roam running
- Emacs server accessible via `emacsclient`
- The `org-roam-api.el` functions available in your Emacs instance

## Installation

### For Users

**One-shot installation (Recommended):**

```bash
# Download and run the installation script
curl -fsSL https://raw.githubusercontent.com/dcruver/org-roam-ai/main/install.sh | bash
```

**Install from PyPI:**

```bash
pip install org-roam-mcp
```

**Install from GitHub (Development):**

```bash
pip install git+https://github.com/dcruver/org-roam-ai.git#subdirectory=mcp
```

### For Developers

```bash
# Clone repository
git clone gitea@gitea-backend.cruver.network:dcruver/org-roam-mcp.git
cd org-roam-mcp

# Create and activate virtual environment
python -m venv .venv
source .venv/bin/activate  # On Linux/Mac
# OR: .venv\Scripts\activate  # On Windows

# Install in development mode
pip install -e .

# Or install with dev dependencies for testing
pip install -e ".[dev]"
```

For complete distribution guide (publishing to package registries, CI/CD, etc.), see [DISTRIBUTION.md](DISTRIBUTION.md).

## Configuration

The server expects an Emacs server running with the org-roam API functions loaded (`org-roam-api.el` must be loaded).

By default, it looks for the server file at `~/.emacs.d/server/server` (the standard Emacs default). You can override this with the `EMACS_SERVER_FILE` environment variable:

```bash
export EMACS_SERVER_FILE=/path/to/your/emacs-server/server
org-roam-mcp
```

## Usage

### Running the MCP Server

```bash
# Run the server (defaults to port 8001, configurable via MCP_PORT)
python -m org_roam_mcp.server

# Or use the installed script
org-roam-mcp

# With custom port
MCP_PORT=8002 org-roam-mcp
```

The server runs in HTTP mode by default on port 8001 (configurable via MCP_PORT environment variable). It also supports stdio mode for MCP-compatible clients.

### Tool Examples

#### Contextual Search
```json
{
  "name": "contextual_search",
  "arguments": {
    "query": "docker networking concepts",
    "limit": 5
  }
}
```

#### Create Note
```json
{
  "name": "create_note", 
  "arguments": {
    "title": "Docker Networking Overview",
    "content": "Docker networks enable communication between containers...",
    "type": "reference",
    "confidence": "high"
  }
}
```

#### Add Daily Entry
```json
{
  "name": "add_daily_entry",
  "arguments": {
    "timestamp": "14:30",
    "title": "Fixed SSL configuration issue", 
    "points": [
      "Updated nginx configuration",
      "Renewed SSL certificates",
      "Tested HTTPS connections"
    ],
    "next_steps": [
      "Monitor for 24 hours", 
      "Update documentation"
    ],
    "tags": ["ssl", "nginx", "troubleshooting"],
    "type": "journal"
  }
}
```

## Integration with n8n

This server is designed to replace complex Execute Command nodes in n8n workflows with clean MCP tool calls:

**Before:**
```
Webhook → Intent Classification → Execute Command (complex elisp escaping) → Response Formatter → Respond
```

**After:**
```
Webhook → Intent Classification → AI Agent (with MCP tools) → Respond
```

The MCP tools handle all the complexity of:
- Parameter escaping for elisp
- JSON response parsing
- Error handling
- Database synchronization

## Architecture

```
org-roam-mcp/
├── src/org_roam_mcp/
│   ├── __init__.py
│   ├── server.py          # Main MCP server
│   ├── emacs_client.py    # Emacsclient wrapper
│   └── tools/             # Tool implementations
├── tests/                 # Test files
└── pyproject.toml        # Project configuration
```

### Key Components

- **EmacsClient**: Handles communication with Emacs via `emacsclient`, including parameter escaping and JSON response parsing
- **MCP Server**: Implements the MCP protocol and exposes org-roam functionality as structured tools
- **Tool Handlers**: Individual functions that map MCP tool calls to elisp functions

## Error Handling

The server includes comprehensive error handling for:
- Emacs connection failures
- Elisp execution errors
- JSON parsing issues
- Parameter validation
- Timeout conditions

## Development

```bash
# Install development dependencies
pip install -e ".[dev]"

# Run tests
pytest

# Format code
black src/ tests/
ruff check src/ tests/

# Type checking
mypy src/
```

## Logging

The server logs important operations and errors. Configure logging level as needed:

```python
import logging
logging.basicConfig(level=logging.DEBUG)  # For verbose output
```

## Contributing

1. Ensure all tests pass
2. Follow code formatting standards (black, ruff)
3. Add type hints for new functions
4. Update documentation for new features

## License

MIT License
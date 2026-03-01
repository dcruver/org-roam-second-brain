# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 🚀 CURRENT STATUS (2025-10-23)

**COMPLETED**: MCP-based embedding generation feature fully implemented and deployed!

**What was done**:
1. ✅ Added `generate_embeddings()` method to `EmacsClient` (emacs_client.py:379-420)
2. ✅ Added `generate_embeddings` tool to MCP server with HTTP handler (server.py:218-232, 717-732, 840-858)
3. ✅ Fixed response parsing to handle both numeric and message string formats
4. ✅ Deployed to production server (192.168.20.136) - MCP server running and tested
5. ✅ Tool verified working: generates 20 embeddings, skips 130 that already exist

**Next steps for agent integration**:
- Deploy updated agent JAR to server (build in `../agent` directory)
- Test end-to-end: Agent → MCP → Emacs → org-roam-semantic
- Verify agent audit shows embeddings are managed via MCP (not local SQLite)
- Confirm nightly job works with new MCP-based approach

**Key architectural change**: Agent now delegates ALL embedding operations to MCP server, which calls `org-roam-semantic-generate-all-embeddings` in Emacs. Embeddings are stored in org files as `:PROPERTIES:`, not in local SQLite database.

---

# Org-roam MCP Server

## Project Overview

An MCP (Model Context Protocol) server that wraps an existing Emacs org-roam knowledge management system. This server provides clean, typed tool interfaces for AI agents (specifically n8n AI Agent nodes) to interact with a personal knowledge base, replacing complex shell command executions with structured MCP tool calls.

## Development Environment Setup

### Virtual Environment

**Two environments exist:**
- **Local development**: `source .venv/bin/activate` (recommended for development)
- **Production/n8n**: `source /home/dcruver/.config/org-roam-mcp/bin/activate`

For development work, use the local `.venv`:
```bash
source .venv/bin/activate
```

**Important**: Make sure the package is installed in your active environment:
```bash
pip install -e ".[dev]"  # For development with test dependencies
```

### Common Development Commands

**Run the server:**
```bash
# HTTP mode (default - for n8n integration on port 8000)
python -m src.org_roam_mcp.server
# or
org-roam-mcp

# Note: The server runs in HTTP mode by default. For stdio MCP mode,
# MCP clients should launch the server via their own stdio transport.
```

**Run tests:**
```bash
pytest
# Run specific test file
pytest tests/test_emacs_client.py
# Run with verbose output
pytest -v
# Run with coverage (requires pytest-cov)
pytest --cov=src/
# Run specific test by name
pytest -k test_name
```

**Code formatting and linting:**
```bash
black src/ tests/
ruff check src/ tests/
# Fix auto-fixable ruff issues
ruff check --fix src/ tests/
```

**Type checking:**
```bash
mypy src/
```

**Install in development mode:**
```bash
pip install -e ".[dev]"
```

**Build and publish:**
```bash
# Build distribution packages
make build

# Publish to Gitea package registry
make publish-gitea

# Or use the commands directly
python -m build
python -m twine upload --repository gitea dist/*
```

See `DISTRIBUTION.md` for complete distribution guide including Gitea package registry, PyPI publishing, and CI/CD setup.

## Architecture Overview

The server implements a dual-mode architecture supporting both MCP stdio protocol and HTTP JSON-RPC:

**Mode Selection:**
- **HTTP mode (default)**: Runs on port 8000, used by n8n AI Agent nodes via JSON-RPC 2.0
- **stdio mode**: Used by MCP-compatible clients that launch the server with stdio transport
- Both modes share the same tool implementations and Emacs client backend

### Core Architecture Components

1. **EmacsClient** (`src/org_roam_mcp/emacs_client.py`)
   - Handles all communication with Emacs via `emacsclient`
   - Manages parameter escaping for safe elisp execution
   - Parses JSON responses from elisp functions
   - Default server file: `~/emacs-server/server` (dynamically resolved at startup)

2. **MCP Server** (`src/org_roam_mcp/server.py`)
   - Implements MCP protocol via `mcp.server.Server` class for stdio mode
   - Provides HTTP JSON-RPC endpoint via Starlette for n8n integration
   - Tool registration with JSON schema validation
   - Error handling and response formatting
   - Main entry point runs HTTP server on port 8000

3. **Tool Interface**
   - Each MCP tool maps to a specific elisp function in `org-roam-api.el`
   - Strict parameter validation via JSON schemas
   - Structured responses with success/error states
   - Same tool implementations used by both stdio and HTTP modes

**Data Flow:**
```
n8n/MCP Client → HTTP/stdio → Server.call_tool() → EmacsClient → emacsclient → Emacs elisp → JSON response → Client
```

### Emacs Integration Details

**Prerequisites:**
- Emacs with org-roam installed and configured
- Emacs daemon running with server socket accessible
- `org-roam-api.el` loaded in Emacs (provides all required API functions)
- Optional: `org-roam-semantic` for semantic search capabilities

**Connection Method:**
- Uses `emacsclient --server-file=~/emacs-server/server --eval`
- Server file path can be overridden with `EMACS_SERVER_FILE` environment variable
- All elisp functions return JSON strings
- 30-second timeout on emacsclient calls

**Required Elisp Functions:**
```elisp
(my/api-search-notes "query")
(my/api-contextual-search "query" limit)
(my/api-semantic-search "query" limit cutoff)  ; requires org-roam-semantic
(my/api-create-note "title" "content" "type" "confidence")
(my/add-daily-entry-structured "timestamp" "title" points_list steps_list tags_list)
(my/get-daily-note-content "date")  ; optional date parameter
(org-roam-db-sync 'force)
```

**Critical Implementation Notes:**
- `my/add-daily-entry-structured` takes exactly 5 parameters: timestamp, title, points_list, steps_list, tags_list
- `my/api-create-note` takes 4 parameters: title, content, type, confidence (type and confidence are still used)
- `my/api-create-note` supports video notes via client-side formatting when url and metadata provided
- `my/api-create-note` automatically generates embeddings if org-roam-semantic is available
- Functions that return `nil` are handled as success cases in Python client
- String parameters must be escaped for shell safety using `EmacsClient._escape_for_elisp()`
- Daily entry title formatting: entry_type parameter controls client-side formatting - TODO entries get "TODO " prefix added in Python before calling elisp
- Entry type is not passed to elisp - it's used client-side to determine title formatting
- Character arrays in JSON responses (where keys are numeric indices) are automatically reconstructed to strings by `EmacsClient._parse_json_response()`

### Tool Parameter Patterns

**Search tools**: Return structured JSON with notes, relevance scores, connections
**Creation tools**: Accept title, content, and optional metadata parameters
**Daily entry tool**: Requires timestamp (HH:MM), title, points array, type; optional next_steps and tags arrays
**Daily content tool**: Optional date parameter (YYYY-MM-DD format), defaults to today
**Database sync tool**: Optional force parameter (defaults to true)

### Available MCP Tools

1. **semantic_search** - AI-powered semantic search using vector embeddings (requires org-roam-semantic)
2. **contextual_search** - Enhanced search with full context for RAG applications
3. **search_notes** - Basic search across note titles and content
4. **create_note** - Create new org-roam notes with proper structure (supports both regular and video notes)
5. **add_daily_entry** - Add structured entries to daily notes (journal or TODO types)
6. **get_daily_content** - Retrieve daily note content for specified date
7. **sync_database** - Sync org-roam database

## Semantic Search Integration

### Prerequisites for Semantic Search

The **semantic_search** tool requires the `org-roam-semantic` package to be installed and configured:

1. **Install org-roam-semantic**: Available at `/home/dcruver/.config/emacs/.local/straight/repos/org-roam-semantic`
2. **Install Ollama** with required models:
   ```bash
   ollama pull nomic-embed-text    # For embeddings
   ollama pull llama3.1:8b         # For AI generation
   ```
3. **Generate embeddings** for existing notes:
   ```elisp
   M-x org-roam-semantic-generate-all-embeddings
   ```

**Note**: New notes created via the `create_note` MCP tool will automatically have embeddings generated if org-roam-semantic is properly configured and the elisp function `org-roam-semantic-generate-embedding` is available. You only need to run the bulk generation command for existing notes that were created before org-roam-semantic was installed.

### Semantic vs. Contextual Search

**Semantic Search (`semantic_search`)**:
- Uses vector embeddings to understand meaning and context
- Finds conceptually related notes, not just keyword matches
- Returns similarity scores (0.0-1.0) based on semantic similarity
- Ideal for RAG applications requiring deep understanding
- Requires org-roam-semantic and pre-generated embeddings

**Contextual Search (`contextual_search`)**:
- Keyword-based search with enhanced context
- Searches titles and full content for exact word matches
- Returns relevance scores based on word frequency
- Works with standard org-roam installation
- Good fallback when semantic search unavailable

### Example Use Cases

**Semantic Search** - "Find notes about container networking":
- Finds notes about Docker, Kubernetes, virtual networks
- Understands concepts like "bridge networks", "service mesh"
- Returns notes even if they don't contain exact terms

**Contextual Search** - "docker networking":
- Finds notes containing the words "docker" and "networking"
- Limited to exact keyword matches
- Faster but less conceptually aware

## Error Handling Strategy

1. **Emacs Connection Failures**: Graceful degradation with meaningful error messages
2. **Elisp Execution Errors**: Parse and expose elisp error details
3. **JSON Parsing Issues**: Handle malformed responses from elisp functions
4. **Parameter Validation**: JSON schema validation before elisp calls

## Integration with n8n

The server replaces complex n8n Execute Command workflows:

**Old pattern:**
Webhook → Intent → Execute Command (complex elisp escaping) → Response Formatter

**New pattern:**
Webhook → Intent → AI Agent (MCP tools) → Response

**HTTP JSON-RPC Endpoint:**
- POST to `http://localhost:8000`
- Standard JSON-RPC 2.0 format
- Method: `tools/call` for tool execution
- Method: `tools/list` for tool discovery
- Method: `initialize` for protocol initialization
- GET to `http://localhost:8000` for health check

**n8n Deployment Configuration:**
When running the MCP server from n8n (which typically runs as root), set the environment variable:
```bash
export EMACS_SERVER_FILE="/home/dcruver/emacs-server/server"
```
This ensures the server connects to the correct Emacs instance regardless of the user context.

## Testing Strategy

**Key test scenarios:**
- Emacs server connectivity (may require mock in CI)
- Parameter escaping and elisp injection prevention
- JSON response parsing edge cases
- Error condition handling
- Tool parameter validation

**Test Structure:**
- Tests located in `tests/` directory
- Main test file: `tests/test_emacs_client.py`
- Uses pytest framework with mocking for Emacs integration
- Run tests with `pytest` or `pytest tests/test_emacs_client.py` for specific file

## Performance Considerations

- Vector search operations may take 1-2 seconds
- Elisp functions handle `org-roam-db-sync` internally 
- Consider connection pooling for high-frequency usage
- Emacsclient calls are synchronous with timeout

## Common Issues and Solutions

**"coroutine 'main' was never awaited"**: Fixed in current version. The entry point now uses `cli_main()` which properly wraps the async `main()` function. If you encounter this, reinstall with `pip install -e .`

**"max iterations" in n8n workflows:** Usually caused by incorrect elisp function signatures or Emacs server connection failures

**JSON parsing errors:** Check for character arrays returned by elisp - the client handles reconstruction

**Parameter escaping:** Use `EmacsClient._escape_for_elisp()` for all string parameters passed to elisp

**Server connection issues:** Verify `EMACS_SERVER_FILE` environment variable points to correct server file; check server file permissions and Emacs daemon status

**Daily entry formatting:** Remember that TODO entries automatically get "TODO " prefix; use `entry_type` parameter to control formatting

**ModuleNotFoundError:** Make sure you've activated the virtual environment and run `pip install -e .` or `pip install -e ".[dev]"`

## Security Notes

- All string inputs are escaped before elisp evaluation
- No direct elisp code injection possible through tool parameters
- Server file path validation prevents directory traversal
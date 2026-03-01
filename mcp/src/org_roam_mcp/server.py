"""MCP server for org-roam knowledge management."""

import asyncio
import logging
from typing import Any, Dict, List, Optional

from mcp.server import Server
from mcp.server.models import InitializationOptions
from mcp.types import ServerCapabilities
from starlette.applications import Starlette
from starlette.routing import Route, Mount
from starlette.middleware.cors import CORSMiddleware
from starlette.responses import JSONResponse, Response
from starlette.requests import Request
from mcp.server.sse import SseServerTransport
import json
from mcp.types import (
    CallToolRequest,
    CallToolResult,
    ListToolsRequest,
    ListToolsResult,
    TextContent,
    Tool,
)

from .emacs_client import EmacsClient, EmacsClientError
from . import __version__

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Initialize Emacs client
emacs_client = EmacsClient()

# Shared tool schema definitions (used by both stdio and HTTP modes)
TOOL_SCHEMAS = [
    {
        "name": "contextual_search",
        "description": "Enhanced search with full context for RAG applications",
        "inputSchema": {
            "type": "object",
            "properties": {
                "query": {
                    "type": "string",
                    "description": "Search terms to query the knowledge base"
                },
                "limit": {
                    "type": "integer",
                    "description": "Maximum number of results to return",
                    "default": 10,
                    "minimum": 1,
                    "maximum": 50
                }
            },
            "required": ["query"]
        }
    },
    {
        "name": "search_notes",
        "description": "Basic search across note titles and content",
        "inputSchema": {
            "type": "object",
            "properties": {
                "query": {
                    "type": "string",
                    "description": "Search terms to query note titles and content"
                }
            },
            "required": ["query"]
        }
    },
    {
        "name": "read_note",
        "description": "Read full content of a note by ID or path. Use this to retrieve note contents without searching.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "identifier": {
                    "type": "string",
                    "description": "Org-roam node ID or path relative to org-roam directory (e.g. projects/my-project.org)"
                },
                "section": {
                    "type": "string",
                    "description": "Optional: heading name to return only that section content"
                }
            },
            "required": ["identifier"]
        }
    },
    {
        "name": "semantic_search",
        "description": "Semantic vector search using AI embeddings to find conceptually related notes",
        "inputSchema": {
            "type": "object",
            "properties": {
                "query": {
                    "type": "string",
                    "description": "Search query to find semantically similar content"
                },
                "limit": {
                    "type": "integer",
                    "description": "Maximum number of results to return",
                    "default": 10,
                    "minimum": 1,
                    "maximum": 50
                },
                "cutoff": {
                    "type": "number",
                    "description": "Similarity threshold (0.0-1.0, higher = more similar)",
                    "default": 0.55,
                    "minimum": 0.0,
                    "maximum": 1.0
                }
            },
            "required": ["query"]
        }
    },
    {
        "name": "create_note",
        "description": "Create a new org-roam note with optional structured formatting for videos",
        "inputSchema": {
            "type": "object",
            "properties": {
                "title": {
                    "type": "string",
                    "description": "Title for the new note"
                },
                "content": {
                    "type": "string",
                    "description": "Content for the new note (transcript for videos)"
                },
                "type": {
                    "type": "string",
                    "description": "Type of note (reference, video, concept, etc.)",
                    "default": "reference"
                },
                "confidence": {
                    "type": "string",
                    "description": "Confidence level (high, medium, low)",
                    "default": "medium"
                },
                "url": {
                    "type": "string",
                    "description": "Optional URL (required for video notes)"
                },
                "metadata": {
                    "type": "object",
                    "description": "Optional metadata for structured content (video duration, channel, etc.)",
                    "properties": {
                        "duration": {"type": "string"},
                        "channel": {"type": "string"},
                        "views": {"type": "string"},
                        "upload_date": {"type": "string"}
                    }
                }
            },
            "required": ["title", "content"]
        }
    },
    {
        "name": "add_daily_entry",
        "description": "Add structured entry to daily note",
        "inputSchema": {
            "type": "object",
            "properties": {
                "timestamp": {
                    "type": "string",
                    "description": "Entry timestamp in HH:MM format"
                },
                "title": {
                    "type": "string",
                    "description": "Entry title"
                },
                "points": {
                    "type": "array",
                    "items": {"type": "string"},
                    "description": "Main points or content items"
                },
                "next_steps": {
                    "type": "array",
                    "items": {"type": "string"},
                    "description": "Optional action items or next steps"
                },
                "tags": {
                    "type": "array",
                    "items": {"type": "string"},
                    "description": "Optional tags for categorization"
                },
                "entry_type": {
                    "type": "string",
                    "description": "Entry type: 'journal' (past tense) or 'todo' (future tense)",
                    "enum": ["journal", "todo"],
                    "default": "journal"
                }
            },
            "required": ["timestamp", "title", "points"]
        }
    },
    {
        "name": "get_daily_content",
        "description": "Get content of daily note",
        "inputSchema": {
            "type": "object",
            "properties": {
                "date": {
                    "type": "string",
                    "description": "Date in YYYY-MM-DD format (defaults to today)",
                    "pattern": r"^\d{4}-\d{2}-\d{2}$"
                }
            },
            "required": []
        }
    },
    {
        "name": "sync_database",
        "description": "Sync org-roam database to ensure latest data",
        "inputSchema": {
            "type": "object",
            "properties": {
                "force": {
                    "type": "boolean",
                    "description": "Whether to force database sync",
                    "default": True
                }
            },
            "required": []
        }
    },
    {
        "name": "generate_embeddings",
        "description": "Generate semantic embeddings for org-roam notes using org-roam-semantic. Processes notes missing embeddings or with stale embeddings.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "force": {
                    "type": "boolean",
                    "description": "Regenerate embeddings even for notes that already have them",
                    "default": False
                }
            },
            "required": []
        }
    },
    {
        "name": "generate_note_embedding",
        "description": "Generate semantic embedding for a single org-roam note. Use after creating or updating a note instead of regenerating all embeddings.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "file_path": {
                    "type": "string",
                    "description": "Absolute path to the org file (e.g., /home/dcruver/org-roam/my-note.org)"
                }
            },
            "required": ["file_path"]
        }
    },
    {
        "name": "add_inbox_entry",
        "description": "Add an inbox entry to today's daily note for audit trail. Use this to log commands and actions for later review.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "command": {
                    "type": "string",
                    "description": "The command used (e.g., 'journal', 'note', 'yt', 'search')"
                },
                "original_text": {
                    "type": "string",
                    "description": "The full original message including the command"
                },
                "linked_note_id": {
                    "type": "string",
                    "description": "Optional ID of a note that was created"
                },
                "linked_note_title": {
                    "type": "string",
                    "description": "Optional title of a note that was created"
                }
            },
            "required": ["command", "original_text"]
        }
    },
    # Phase 2: Structured Node Types
    {
        "name": "create_person",
        "description": "Create a person node with structured fields for tracking relationships and follow-ups",
        "inputSchema": {
            "type": "object",
            "properties": {
                "name": {
                    "type": "string",
                    "description": "Person's name"
                },
                "context": {
                    "type": "string",
                    "description": "How you know them, their role, company, etc."
                },
                "follow_ups": {
                    "type": "array",
                    "items": {"type": "string"},
                    "description": "Action items related to this person"
                },
                "notes": {
                    "type": "string",
                    "description": "Additional notes about this person"
                }
            },
            "required": ["name"]
        }
    },
    {
        "name": "create_project",
        "description": "Create a project node with structured fields for tracking status and next actions",
        "inputSchema": {
            "type": "object",
            "properties": {
                "title": {
                    "type": "string",
                    "description": "Project name"
                },
                "status": {
                    "type": "string",
                    "enum": ["active", "waiting", "blocked", "someday", "done"],
                    "description": "Project status",
                    "default": "active"
                },
                "next_action": {
                    "type": "string",
                    "description": "Next actionable step (must be specific and actionable)"
                },
                "notes": {
                    "type": "string",
                    "description": "Additional notes about this project"
                },
                "content": {
                    "type": "string",
                    "description": "Full org-mode content (if provided, replaces template-based creation)"
                }
            },
            "required": ["title"]
        }
    },
    {
        "name": "create_idea",
        "description": "Create an idea node with a one-liner summary and optional elaboration",
        "inputSchema": {
            "type": "object",
            "properties": {
                "title": {
                    "type": "string",
                    "description": "Idea title"
                },
                "one_liner": {
                    "type": "string",
                    "description": "Brief summary of the insight (required)"
                },
                "elaboration": {
                    "type": "string",
                    "description": "Detailed explanation of the idea"
                }
            },
            "required": ["title", "one_liner"]
        }
    },
    {
        "name": "create_admin",
        "description": "Create an admin task node for tracking administrative tasks with due dates",
        "inputSchema": {
            "type": "object",
            "properties": {
                "title": {
                    "type": "string",
                    "description": "Task title"
                },
                "due_date": {
                    "type": "string",
                    "description": "Due date in YYYY-MM-DD format",
                    "pattern": r"^\d{4}-\d{2}-\d{2}$"
                },
                "notes": {
                    "type": "string",
                    "description": "Additional notes about this task"
                }
            },
            "required": ["title"]
        }
    },
    # Phase 3: Proactive Surfacing
    {
        "name": "get_active_projects",
        "description": "Get all active projects with their next actions, ordered by last modified. Use for daily digest.",
        "inputSchema": {
            "type": "object",
            "properties": {},
            "required": []
        }
    },
    {
        "name": "get_pending_followups",
        "description": "Get all people with pending follow-up items. Use for daily digest.",
        "inputSchema": {
            "type": "object",
            "properties": {},
            "required": []
        }
    },
    {
        "name": "get_stale_projects",
        "description": "Get projects with no activity in X days that might be stuck or forgotten.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "days_threshold": {
                    "type": "integer",
                    "description": "Number of days of inactivity to consider stale",
                    "default": 5,
                    "minimum": 1
                }
            },
            "required": []
        }
    },
    {
        "name": "get_weekly_inbox",
        "description": "Get inbox entries from the past N days for weekly review.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "days": {
                    "type": "integer",
                    "description": "Number of days to look back",
                    "default": 7,
                    "minimum": 1,
                    "maximum": 30
                }
            },
            "required": []
        }
    },
    {
        "name": "get_digest_data",
        "description": "Get all data needed for daily digest in a single call. Combines active projects, pending follow-ups, and stale projects.",
        "inputSchema": {
            "type": "object",
            "properties": {},
            "required": []
        }
    },
    {
        "name": "log_to_inbox",
        "description": "Log text to inbox section of daily note. Auto-creates person nodes for any [[Name]] links found.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "text": {
                    "type": "string",
                    "description": "Text to log. Use [[Name]] syntax to create backlinks to people."
                }
            },
            "required": ["text"]
        }
    },
    {
        "name": "get_dangling_followups",
        "description": "Find unchecked items with [[Name]] links where the person node doesn't exist. Use for daily digest to surface untracked people.",
        "inputSchema": {
            "type": "object",
            "properties": {},
            "required": []
        }
    },
    {
        "name": "get_note_properties",
        "description": "Get metadata for a note without full content.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "identifier": {"type": "string", "description": "Org-roam node ID or path"}
            },
            "required": ["identifier"]
        }
    },
    {
        "name": "delete_note",
        "description": "Delete or archive a note.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "identifier": {"type": "string", "description": "Org-roam node ID or path"},
                "archive": {"type": "boolean", "default": False}
            },
            "required": ["identifier"]
        }
    },
    {
        "name": "rename_note",
        "description": "Rename a note.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "identifier": {"type": "string"},
                "new_title": {"type": "string"}
            },
            "required": ["identifier", "new_title"]
        }
    },
    {
        "name": "manage_tags",
        "description": "Add or remove filetags from a note.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "identifier": {"type": "string"},
                "action": {"type": "string", "enum": ["add", "remove"]},
                "tag": {"type": "string"}
            },
            "required": ["identifier", "action", "tag"]
        }
    },
    {
        "name": "add_link",
        "description": "Create an org-roam link from one note to another.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "from_id": {"type": "string"},
                "to_id": {"type": "string"},
                "section": {"type": "string"}
            },
            "required": ["from_id", "to_id"]
        }
    },
    {
        "name": "change_task_state",
        "description": "Change the TODO state of a task in an org-roam project file. Triggers journal logging hooks for state change tracking.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "file": {
                    "type": "string",
                    "description": "Path to the org file (e.g., ~/org-roam/projects/my-project.org)"
                },
                "heading": {
                    "type": "string",
                    "description": "The task heading text to find"
                },
                "new_state": {
                    "type": "string",
                    "description": "New TODO state",
                    "enum": ["TODO", "IN-PROGRESS", "BLOCKED", "DONE", "CANCELLED"]
                }
            },
            "required": ["file", "heading", "new_state"]
        }
    },

    {
        "name": "update_note",
        "description": "Update content in a note by appending, prepending, or replacing. Can target specific sections.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "identifier": {
                    "type": "string",
                    "description": "Org-roam node ID or path relative to org-roam directory"
                },
                "content": {
                    "type": "string",
                    "description": "Content to add to the note"
                },
                "section": {
                    "type": "string",
                    "description": "Optional: heading name to target (creates if not found)"
                },
                "mode": {
                    "type": "string",
                    "description": "How to add content: append (default), prepend, or replace",
                    "enum": ["append", "prepend", "replace"],
                    "default": "append"
                }
            },
            "required": ["identifier", "content"]
        }
    },
    {
        "name": "list_notes",
        "description": "List org-roam notes with optional filters by type and status. Use when you need to discover notes without a search query.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "node_type": {
                    "type": "string",
                    "description": "Filter by note type",
                    "enum": ["project", "person", "idea", "admin", "blog"]
                },
                "status": {
                    "type": "string",
                    "description": "Filter by status",
                    "enum": ["active", "stale", "done", "cancelled"]
                },
                "limit": {
                    "type": "integer",
                    "description": "Maximum results to return",
                    "default": 50,
                    "minimum": 1,
                    "maximum": 200
                },
                "sort_by": {
                    "type": "string",
                    "description": "Sort order",
                    "enum": ["modified", "created", "title"],
                    "default": "modified"
                }
            },
            "required": []
        }
    },
]

# Create MCP server
app = Server("org-roam-mcp")


@app.list_tools()
async def list_tools() -> ListToolsResult:
    """List available MCP tools."""
    # Convert shared schema definitions to Tool objects
    tools = [Tool(**schema) for schema in TOOL_SCHEMAS]
    return ListToolsResult(tools=tools)


@app.call_tool()
async def call_tool(name: str, arguments: dict) -> CallToolResult:
    """Handle tool calls."""
    try:
        tool_name = name
        arguments = arguments or {}
        
        logger.info(f"Calling tool: {tool_name} with arguments: {arguments}")
        
        if tool_name == "contextual_search":
            query = arguments["query"]
            limit = arguments.get("limit", 10)

            result = emacs_client.contextual_search(query, limit)
            result = _sanitize_result(result)  # Sanitize control characters

            return CallToolResult(
                content=[
                    TextContent(
                        type="text",
                        text=f"Contextual search results for '{query}':\n\n" +
                             _format_contextual_search_results(result)
                    )
                ]
            )
            
        elif tool_name == "search_notes":
            query = arguments["query"]

            result = emacs_client.search_notes(query)
            result = _sanitize_result(result)  # Sanitize control characters

            return CallToolResult(
                content=[
                    TextContent(
                        type="text",
                        text=f"Search results for '{query}':\n\n" +
                             _format_basic_search_results(result)
                    )
                ]
            )
        elif tool_name == "read_note":
            identifier = arguments["identifier"]
            section = arguments.get("section")

            result = emacs_client.read_note(identifier, section)
            result = _sanitize_result(result)

            if result.get("success"):
                title = result.get("title", "Unknown")
                file_path = result.get("file", "")
                note_content = result.get("content", "")
                
                # Truncate content if too long
                if len(note_content) > 50000:
                    note_content = note_content[:50000] + "\n\n... [truncated]"
                
                response = f"**{title}**\nFile: {file_path}\n\n{note_content}"
            else:
                response = f"Error: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[
                    TextContent(
                        type="text",
                        text=response
                    )
                ]
            )



        elif tool_name == "update_note":
            identifier = arguments["identifier"]
            content = arguments["content"]
            section = arguments.get("section")
            mode = arguments.get("mode", "append")

            result = emacs_client.update_note(identifier, content, section, mode)
            result = _sanitize_result(result)

            if result.get("success"):
                file_path = result.get("file", "")
                section_desc = section if section else "(whole file)"
                response = f"Note updated successfully.\nFile: {file_path}\nMode: {mode}\nSection: {section_desc}"
            else:
                response = f"Error: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[
                    TextContent(
                        type="text",
                        text=response
                    )
                ]
            )

        elif tool_name == "list_notes":
            node_type = arguments.get("node_type")
            status = arguments.get("status")
            limit = arguments.get("limit", 50)
            sort_by = arguments.get("sort_by", "modified")

            result = emacs_client.list_notes(node_type, status, limit, sort_by)
            result = _sanitize_result(result)

            if result.get("success"):
                notes = result.get("notes", [])
                total = result.get("total_found", 0)
                lines = [f"Found {total} notes (showing {len(notes)}):"]
                for note in notes:
                    status_str = f" [{note.get('status', '')}]" if note.get('status') else ""
                    type_str = f" ({note.get('node_type', '')})" if note.get('node_type') else ""
                    lines.append(f"- **{note.get('title', 'Untitled')}**{type_str}{status_str}")
                    lines.append(f"  ID: {note.get('id', 'N/A')} | Modified: {note.get('modified', 'N/A')}")
                response = "\n".join(lines)
            else:
                response = f"Error: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[
                    TextContent(
                        type="text",
                        text=response
                    )
                ]
            )


        elif tool_name == "get_note_properties":
            identifier = arguments["identifier"]
            result = emacs_client.get_note_properties(identifier)
            result = _sanitize_result(result)
            if result.get("success"):
                props = [f"**{result.get('title', 'Untitled')}**"]
                props.append(f"ID: {result.get('id', 'N/A')}")
                props.append(f"File: {result.get('file', 'N/A')}")
                if result.get('node_type'): props.append(f"Type: {result.get('node_type')}")
                if result.get('status'): props.append(f"Status: {result.get('status')}")
                response = "\n".join(props)
            else:
                response = f"Error: {result.get('error', 'Unknown error')}"
            return CallToolResult(content=[TextContent(type="text", text=response)])

        elif tool_name == "delete_note":
            identifier = arguments["identifier"]
            archive = arguments.get("archive", False)
            result = emacs_client.delete_note(identifier, archive)
            result = _sanitize_result(result)
            action = "archived" if archive else "deleted"
            if result.get("success"):
                response = f"Note {action}: {result.get('file', identifier)}"
            else:
                response = f"Error: {result.get('error', 'Unknown error')}"
            return CallToolResult(content=[TextContent(type="text", text=response)])

        elif tool_name == "rename_note":
            identifier = arguments["identifier"]
            new_title = arguments["new_title"]
            result = emacs_client.rename_note(identifier, new_title)
            result = _sanitize_result(result)
            if result.get("success"):
                response = f"Renamed to: {result.get('new_title')}\nNew file: {result.get('new_file')}"
            else:
                response = f"Error: {result.get('error', 'Unknown error')}"
            return CallToolResult(content=[TextContent(type="text", text=response)])

        elif tool_name == "manage_tags":
            identifier = arguments["identifier"]
            action = arguments["action"]
            tag = arguments["tag"]
            result = emacs_client.manage_tags(identifier, action, tag)
            result = _sanitize_result(result)
            if result.get("success"):
                response = f"Tag '{tag}' {action}ed on {result.get('file', identifier)}"
            else:
                response = f"Error: {result.get('error', 'Unknown error')}"
            return CallToolResult(content=[TextContent(type="text", text=response)])

        elif tool_name == "add_link":
            from_id = arguments["from_id"]
            to_id = arguments["to_id"]
            section = arguments.get("section")
            result = emacs_client.add_link(from_id, to_id, section)
            result = _sanitize_result(result)
            if result.get("success"):
                response = f"Link added: {result.get('from')} -> {result.get('to_title')}"
            else:
                response = f"Error: {result.get('error', 'Unknown error')}"
            return CallToolResult(content=[TextContent(type="text", text=response)])

        elif tool_name == "semantic_search":
            query = arguments["query"]
            limit = arguments.get("limit", 10)
            cutoff = arguments.get("cutoff", 0.55)

            result = emacs_client.semantic_search(query, limit, cutoff)
            result = _sanitize_result(result)  # Sanitize control characters

            return CallToolResult(
                content=[
                    TextContent(
                        type="text",
                        text=f"Semantic search results for '{query}':\n\n" +
                             _format_semantic_search_results(result)
                    )
                ]
            )
            
        elif tool_name == "create_note":
            title = arguments["title"]
            content = arguments["content"]
            note_type = arguments.get("type", "reference")
            confidence = arguments.get("confidence", "medium")
            url = arguments.get("url")
            metadata = arguments.get("metadata", {})

            result = emacs_client.create_note(title, content, note_type, confidence, url, metadata)

            if result.get("success"):
                note_info = result.get("note", {})
                embedding_generated = result.get("embedding_generated", False)

                if note_type == "video":
                    response = f"🎥 Video note created successfully: '{title}'\n"
                    response += f"ID: {note_info.get('id', 'N/A')}\n"
                    response += f"File: {note_info.get('file', 'N/A')}\n"
                    if url:
                        response += f"URL: {url}\n"
                else:
                    response = f"✅ Note created successfully: '{title}'\n"
                    response += f"ID: {note_info.get('id', 'N/A')}\n"
                    response += f"File: {note_info.get('file', 'N/A')}\n"
                    response += f"Type: {note_type}, Confidence: {confidence}\n"

                # Add embedding status
                if embedding_generated:
                    response += f"🔍 Semantic embedding generated automatically"
                else:
                    response += f"ℹ️ Semantic embeddings not available (org-roam-semantic not loaded)"
            else:
                response = f"❌ Failed to create note: {result.get('message', 'Unknown error')}"

            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )
            
        elif tool_name == "add_daily_entry":
            timestamp = arguments["timestamp"]
            title = arguments["title"]
            points = arguments["points"]
            next_steps = arguments.get("next_steps", [])
            tags = arguments.get("tags", [])
            entry_type = arguments.get("entry_type", "journal")
            
            result = emacs_client.add_daily_entry(
                timestamp, title, points, next_steps, tags, entry_type
            )
            
            if result.get("success"):
                # Return the exact response format the agent expects to see
                entry_label = "TODO" if entry_type == "todo" else "JOURNAL"
                response = f"✅ **{entry_label}** added to daily note: {title}"
            else:
                response = f"❌ Failed to add daily entry: {result.get('error', 'Unknown error')}"
            
            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )
            
        elif tool_name == "get_daily_content":
            date = arguments.get("date")
            
            result = emacs_client.get_daily_content(date)
            
            if result.get("success"):
                content = result.get("content", "")
                date_str = result.get("date", "today")
                
                if content:
                    response = f"📝 Daily note content for {date_str}:\n\n{content}"
                else:
                    response = f"📝 No content found for daily note ({date_str})"
            else:
                response = f"❌ Failed to get daily content: {result.get('error', 'Unknown error')}"
            
            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )
            
        elif tool_name == "sync_database":
            force = arguments.get("force", True)

            result = emacs_client.sync_database(force)

            if result.get("success"):
                response = "✅ Org-roam database synced successfully"
            else:
                response = f"❌ Failed to sync database: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )

        elif tool_name == "generate_embeddings":
            force = arguments.get("force", False)

            result = emacs_client.generate_embeddings(force)

            if result.get("success"):
                count = result.get("count", 0)
                response = f"✅ Generated {count} embeddings for org-roam notes"
                if count == 0:
                    response += "\n(All notes already have current embeddings)"
            else:
                response = f"❌ Failed to generate embeddings: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )



        elif tool_name == "change_task_state":
            file = arguments["file"]
            heading = arguments["heading"]
            new_state = arguments["new_state"]

            result = emacs_client.change_task_state(file, heading, new_state)

            if result.get("success"):
                response = f"Task state changed: {heading} to {new_state}"
                if result.get("message"):
                    response += "\n" + result.get("message")
            else:
                response = f"Failed to change task state: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )
        elif tool_name == "generate_note_embedding":
            file_path = arguments["file_path"]

            result = emacs_client.generate_note_embedding(file_path)

            if result.get("success"):
                response = f"\u2705 {result.get('message', 'Embedding generated')}"
            else:
                response = f"\u274c Failed to generate embedding: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )

        elif tool_name == "add_inbox_entry":
            command = arguments["command"]
            original_text = arguments["original_text"]
            linked_note_id = arguments.get("linked_note_id")
            linked_note_title = arguments.get("linked_note_title")

            result = emacs_client.add_inbox_entry(
                command, original_text, linked_note_id, linked_note_title
            )

            if result.get("success"):
                response = f"✅ Inbox entry added for /{command}"
            else:
                response = f"❌ Failed to add inbox entry: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )

        # Phase 2: Structured Node Types
        elif tool_name == "create_person":
            name = arguments["name"]
            context = arguments.get("context")
            follow_ups = arguments.get("follow_ups", [])
            notes = arguments.get("notes")

            result = emacs_client.create_person(name, context, follow_ups, notes)

            if result.get("success"):
                note_info = result.get("note", {})
                response = f"👤 Person note created: '{name}'\n"
                response += f"ID: {note_info.get('id', 'N/A')}\n"
                response += f"File: {note_info.get('file', 'N/A')}"
            else:
                response = f"❌ Failed to create person: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )

        elif tool_name == "create_project":
            title = arguments["title"]
            status = arguments.get("status", "active")
            next_action = arguments.get("next_action")
            notes = arguments.get("notes")
            content = arguments.get("content")

            result = emacs_client.create_project(title, status, next_action, notes, content)

            if result.get("success"):
                note_info = result.get("note", {})
                response = f"📋 Project note created: '{title}'\n"
                response += f"ID: {note_info.get('id', 'N/A')}\n"
                response += f"Status: {status}\n"
                response += f"File: {note_info.get('file', 'N/A')}"
            else:
                response = f"❌ Failed to create project: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )

        elif tool_name == "create_idea":
            title = arguments["title"]
            one_liner = arguments["one_liner"]
            elaboration = arguments.get("elaboration")

            result = emacs_client.create_idea(title, one_liner, elaboration)

            if result.get("success"):
                note_info = result.get("note", {})
                response = f"💡 Idea note created: '{title}'\n"
                response += f"ID: {note_info.get('id', 'N/A')}\n"
                response += f"File: {note_info.get('file', 'N/A')}"
            else:
                response = f"❌ Failed to create idea: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )

        elif tool_name == "create_admin":
            title = arguments["title"]
            due_date = arguments.get("due_date")
            notes = arguments.get("notes")

            result = emacs_client.create_admin(title, due_date, notes)

            if result.get("success"):
                note_info = result.get("note", {})
                response = f"📝 Admin task created: '{title}'\n"
                response += f"ID: {note_info.get('id', 'N/A')}\n"
                if due_date:
                    response += f"Due: {due_date}\n"
                response += f"File: {note_info.get('file', 'N/A')}"
            else:
                response = f"❌ Failed to create admin task: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )

        # Phase 3: Proactive Surfacing
        elif tool_name == "get_active_projects":
            result = emacs_client.get_active_projects()

            if result.get("success"):
                projects = result.get("projects", [])
                response = f"📋 Active Projects ({result.get('total', 0)} total):\n\n"
                for i, proj in enumerate(projects[:10], 1):
                    response += f"{i}. **{proj.get('title', 'Untitled')}**\n"
                    if proj.get('next_action'):
                        response += f"   Next: {proj.get('next_action')}\n"
                    response += f"   Modified: {proj.get('days_since_modified', '?')} days ago\n"
            else:
                response = f"❌ Failed to get active projects: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )

        elif tool_name == "get_pending_followups":
            result = emacs_client.get_pending_followups()

            if result.get("success"):
                people = result.get("people", [])
                response = f"👥 Pending Follow-ups ({result.get('total', 0)} people):\n\n"
                for person in people:
                    response += f"**{person.get('name', 'Unknown')}**\n"
                    if person.get('context'):
                        response += f"   Context: {person.get('context')}\n"
                    for followup in person.get('pending_followups', []):
                        response += f"   - [ ] {followup}\n"
                    response += "\n"
            else:
                response = f"❌ Failed to get pending follow-ups: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )

        elif tool_name == "get_stale_projects":
            days_threshold = arguments.get("days_threshold", 5)
            result = emacs_client.get_stale_projects(days_threshold)

            if result.get("success"):
                projects = result.get("projects", [])
                response = f"⚠️ Stale Projects (no activity in {days_threshold}+ days):\n\n"
                if projects:
                    for proj in projects:
                        response += f"**{proj.get('title', 'Untitled')}** - {proj.get('days_since_modified', '?')} days\n"
                        if proj.get('next_action'):
                            response += f"   Next: {proj.get('next_action')}\n"
                        response += f"   Status: {proj.get('status', 'unknown')}\n\n"
                else:
                    response += "No stale projects found!\n"
            else:
                response = f"❌ Failed to get stale projects: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )

        elif tool_name == "get_weekly_inbox":
            days = arguments.get("days", 7)
            result = emacs_client.get_weekly_inbox(days)

            if result.get("success"):
                response = f"📥 Inbox Summary (past {days} days):\n\n"
                response += f"Total entries: {result.get('total_entries', 0)}\n\n"
                for day_data in result.get("by_day", []):
                    response += f"**{day_data.get('date')}** ({day_data.get('count', 0)} entries)\n"
                    for entry in day_data.get("entries", []):
                        response += f"   /{entry.get('command')} → {entry.get('result', 'processed')}\n"
                    response += "\n"
            else:
                response = f"❌ Failed to get weekly inbox: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )

        elif tool_name == "get_digest_data":
            result = emacs_client.get_digest_data()

            if result.get("success"):
                response = f"📊 Daily Digest Data (generated {result.get('generated_at', 'now')}):\n\n"

                # Active projects
                active = result.get("active_projects", {})
                response += f"**Active Projects** ({active.get('total', 0)} total):\n"
                for proj in active.get("top_3", []):
                    response += f"   - {proj.get('title', 'Untitled')}"
                    if proj.get('next_action'):
                        response += f": {proj.get('next_action')}"
                    response += "\n"

                # Pending follow-ups
                followups = result.get("pending_followups", {})
                response += f"\n**Pending Follow-ups** ({followups.get('total', 0)} people):\n"
                for person in followups.get("people", [])[:5]:
                    response += f"   - {person.get('name', 'Unknown')}: {person.get('followup_count', 0)} items\n"

                # Stale projects
                stale = result.get("stale_projects", {})
                response += f"\n**Might be stuck** ({stale.get('total', 0)} projects):\n"
                for proj in stale.get("projects", [])[:3]:
                    response += f"   - {proj.get('title', 'Untitled')} ({proj.get('days_since_modified', '?')} days)\n"
            else:
                response = f"❌ Failed to get digest data: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )

        elif tool_name == "log_to_inbox":
            text = arguments.get("text", "")
            result = emacs_client.log_to_inbox(text)

            if result.get("success"):
                created = result.get("created_people", [])
                if created:
                    response = f"✅ Logged to inbox. Auto-created person nodes: {', '.join(created)}"
                else:
                    response = "✅ Logged to inbox"
            else:
                response = f"❌ Failed to log: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )

        elif tool_name == "get_dangling_followups":
            result = emacs_client.get_dangling_followups()

            if result.get("success"):
                total = result.get("total", 0)
                if total > 0:
                    response = f"⚠️ Found {total} untracked people with follow-ups:\n"
                    for item in result.get("untracked_people", []):
                        response += f"   - [[{item.get('name', '?')}]]: {item.get('item', '')}\n"
                else:
                    response = "✅ No untracked people found"
            else:
                response = f"❌ Failed: {result.get('error', 'Unknown error')}"

            return CallToolResult(
                content=[TextContent(type="text", text=response)]
            )

        else:
            return CallToolResult(
                content=[
                    TextContent(
                        type="text",
                        text=f"Unknown tool: {tool_name}"
                    )
                ]
            )

    except EmacsClientError as e:
        logger.error(f"Emacs client error in {tool_name}: {e}")
        return CallToolResult(
            content=[
                TextContent(
                    type="text",
                    text=f"❌ Emacs communication error: {str(e)}"
                )
            ]
        )
    except Exception as e:
        logger.error(f"Unexpected error in {tool_name}: {e}")
        return CallToolResult(
            content=[
                TextContent(
                    type="text",
                    text=f"❌ Unexpected error: {str(e)}"
                )
            ]
        )


def _sanitize_control_chars(text: str) -> str:
    """Remove or escape control characters from a string for JSON safety."""
    if not isinstance(text, str):
        return text
    result = []
    for char in text:
        code = ord(char)
        if code < 32:  # Control characters
            if code == 9:  # tab
                result.append(' ')
            elif code == 10:  # newline
                result.append(' ')
            elif code == 13:  # carriage return
                result.append(' ')
            else:
                result.append(' ')  # Replace other control chars with space
        elif code == 127:  # DEL
            result.append(' ')
        elif 128 <= code <= 159:  # C1 control codes
            result.append(' ')
        else:
            result.append(char)
    return ''.join(result)


def _sanitize_result(obj: Any) -> Any:
    """Recursively sanitize control characters from all strings in a result object."""
    if isinstance(obj, str):
        return _sanitize_control_chars(obj)
    elif isinstance(obj, dict):
        return {k: _sanitize_result(v) for k, v in obj.items()}
    elif isinstance(obj, list):
        return [_sanitize_result(item) for item in obj]
    else:
        return obj


def _strip_embeddings(content: str) -> str:
    """Strip embedding vectors from org-roam content."""
    import re
    # Remove :EMBEDDING: property and its value (numbers, spaces, dashes, dots)
    # Match until we hit a letter (start of actual content) or end of string
    content = re.sub(r':EMBEDDING:\s*[-\d.\s]+', '', content)
    return content


def _format_contextual_search_results(result: Dict[str, Any]) -> str:
    """Format contextual search results for display."""
    if not result.get("success"):
        return f"Search failed: {result.get('error', 'Unknown error')}"

    notes = result.get("notes", [])
    # Deduplicate notes from the same file
    notes = _deduplicate_notes(notes)
    context = result.get("knowledge_context", {})

    if not notes:
        return "No relevant notes found."

    response = f"Found {len(notes)} relevant notes:\n\n"

    for i, note in enumerate(notes, 1):
        response += f"{i}. **{note.get('title', 'Untitled')}**\n"
        file_path = note.get('file', '')
        if file_path:
            # Extract just the filename and prepend /org-roam/
            import os
            filename = os.path.basename(file_path)
            response += f"   File: /org-roam/{filename}\n"
        response += f"   Relevance: {note.get('relevance_score', 0):.3f}\n"

        # Show snippet of content - escape all control characters
        content = note.get('full_content', '')
        if content:
            # Strip embeddings and clean content
            clean_content = _strip_embeddings(content)
            clean_content = clean_content.replace(':PROPERTIES:', '').replace(':END:', '')
            # Split into words and take first 50 (more useful context now without embeddings)
            clean_content = ' '.join(clean_content.split()[:50])
            # Escape any remaining control characters for JSON safety
            clean_content = (clean_content
                           .replace('\n', ' ')
                           .replace('\r', ' ')
                           .replace('\t', ' ')
                           .replace('\x0b', ' ')
                            .replace('\x0c', ' '))
            response += f"   Content: {clean_content}...\n"

        # Show connections
        backlinks = note.get('backlinks') or []
        forward_links = note.get('forward_links') or []
        if backlinks or forward_links:
            response += f"   Connections: {len(backlinks)} backlinks, {len(forward_links)} forward links\n"

        response += "\n"

    # Add knowledge context summary
    total_connections = context.get('total_connections', 0)
    if total_connections > 0:
        response += f"Knowledge graph summary: {total_connections} total connections found\n"

    return response


def _deduplicate_notes(notes: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """Deduplicate notes by file path, keeping highest similarity score."""
    if not notes:
        return []
    seen_files = {}
    for note in notes:
        file_path = note.get('file', note.get('id', ''))
        score = note.get('similarity_score', note.get('relevance_score', 0))
        if file_path not in seen_files or score > seen_files[file_path].get('similarity_score', seen_files[file_path].get('relevance_score', 0)):
            seen_files[file_path] = note
    return list(seen_files.values())


def _format_semantic_search_results(result: Dict[str, Any]) -> str:
    """Format semantic search results for display."""
    if not result.get("success"):
        error_msg = result.get('error', 'Unknown error')
        if 'fallback_available' in result:
            return f"Semantic search failed: {error_msg}\n\nFallback to contextual search recommended."
        return f"Semantic search failed: {error_msg}"

    notes = result.get("notes", [])
    # Deduplicate notes from the same file
    notes = _deduplicate_notes(notes)
    context = result.get("knowledge_context", {})

    if not notes:
        return "No semantically similar notes found."

    search_type = result.get("search_type", "unknown")
    cutoff = result.get("similarity_cutoff", 0.55)

    response = f"Found {len(notes)} semantically similar notes (similarity >= {cutoff:.2f}):\n\n"

    for i, note in enumerate(notes, 1):
        response += f"{i}. **{note.get('title', 'Untitled')}**\n"
        file_path = note.get('file', '')
        if file_path:
            # Extract just the filename and prepend /org-roam/
            import os
            filename = os.path.basename(file_path)
            response += f"   File: /org-roam/{filename}\n"
        response += f"   Similarity: {note.get('similarity_score', 0):.3f}\n"

        # Show snippet of content - escape all control characters
        content = note.get('full_content', '')
        if content:
            # Strip embeddings and clean content
            clean_content = _strip_embeddings(content)
            clean_content = clean_content.replace(':PROPERTIES:', '').replace(':END:', '')
            # Split into words and take first 50 (more useful context now without embeddings)
            clean_content = ' '.join(clean_content.split()[:50])
            # Escape any remaining control characters for JSON safety
            clean_content = (clean_content
                           .replace('\n', ' ')
                           .replace('\r', ' ')
                           .replace('\t', ' ')
                           .replace('\x0b', ' ')
                           .replace('\x0c', ' '))
            response += f"   Content: {clean_content}...\n"

        # Show connections
        backlinks = note.get('backlinks') or []
        forward_links = note.get('forward_links') or []
        if backlinks or forward_links:
            response += f"   Connections: {len(backlinks)} backlinks, {len(forward_links)} forward links\n"

        response += "\n"

    # Add semantic context summary
    avg_similarity = context.get('average_similarity', 0)
    total_connections = context.get('total_connections', 0)
    embedding_model = context.get('embedding_model', 'unknown')

    response += f"Semantic analysis summary:\n"
    response += f"- Search method: {search_type}\n"
    response += f"- Average similarity: {avg_similarity:.3f}\n"
    response += f"- Total connections: {total_connections}\n"
    response += f"- Embedding model: {embedding_model}\n"

    return response


def _format_basic_search_results(result: Dict[str, Any]) -> str:
    """Format basic search results for display."""
    if not result.get("success"):
        return f"Search failed: {result.get('error', 'Unknown error')}"
    
    notes = result.get("notes", [])
    
    if not notes:
        return "No notes found."
    
    response = f"Found {len(notes)} notes:\n\n"
    
    for i, note in enumerate(notes, 1):
        response += f"{i}. {note.get('title', 'Untitled')}\n"
        response += f"   ID: {note.get('id', 'N/A')}\n"
        response += f"   Created: {note.get('created', 'N/A')}\n"
        if note.get('status'):
            response += f"   Status: {note['status']}\n"
        response += "\n"
    
    return response


async def health_check(request):
    """Health check endpoint."""
    return Response("OK", status_code=200)

def create_starlette_app():
    """Create Starlette app with MCP HTTP endpoint."""
    
    # Create Emacs client instance
    emacs_client = EmacsClient()
    
    async def handle_mcp_request(request: Request):
        """Handle MCP JSON-RPC requests via HTTP POST."""
        try:
            # Parse JSON-RPC request
            body = await request.body()
            rpc_request = json.loads(body)
            
            # Handle different MCP request types
            if rpc_request.get("method") == "tools/list":
                # Return list of available tools from shared schema
                return JSONResponse({
                    "jsonrpc": "2.0",
                    "id": rpc_request.get("id"),
                    "result": {
                        "tools": TOOL_SCHEMAS
                    }
                })
            
            elif rpc_request.get("method") == "tools/call":
                # Handle tool execution
                params = rpc_request.get("params", {})
                tool_name = params.get("name")
                arguments = params.get("arguments", {})
                
                if tool_name == "search_notes":
                    result = emacs_client.search_notes(arguments.get("query"))

                    # Format result like MCP tools do
                    formatted_text = f"Search results for '{arguments.get('query')}':\n\n" + _format_basic_search_results(result)

                    response_data = {
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": {
                            "content": [
                                {
                                    "type": "text",
                                    "text": formatted_text
                                }
                            ]
                        }
                    }

                    json_content = json.dumps(response_data, ensure_ascii=False)
                    return Response(
                        content=json_content,
                        media_type="application/json",
                        headers={"Content-Type": "application/json; charset=utf-8"}
                    )
                elif tool_name == "contextual_search":
                    result = emacs_client.contextual_search(
                        arguments.get("query"),
                        arguments.get("limit", 10)
                    )
                    result = _sanitize_result(result)  # Sanitize control characters

                    # Debug: log the result
                    logger.info(f"Contextual search result type: {type(result)}")

                    # Format result like MCP tools do
                    formatted_text = f"Contextual search results for '{arguments.get('query')}':\n\n" + _format_contextual_search_results(result)

                    response_data = {
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": {
                            "content": [
                                {
                                    "type": "text",
                                    "text": formatted_text
                                }
                            ]
                        }
                    }

                    json_content = json.dumps(response_data, ensure_ascii=False)
                    return Response(
                        content=json_content,
                        media_type="application/json",
                        headers={"Content-Type": "application/json; charset=utf-8"}
                    )
                
                elif tool_name == "read_note":
                    result = emacs_client.read_note(
                        arguments.get("identifier"),
                        arguments.get("section")
                    )
                    result = _sanitize_result(result)

                    if result.get("success"):
                        title = result.get("title", "Unknown")
                        file_path = result.get("file", "")
                        note_content = result.get("content", "")
                        
                        if len(note_content) > 50000:
                            note_content = note_content[:50000] + "\n\n... [truncated]"
                        
                        formatted_text = f"**{title}**\nFile: {file_path}\n\n{note_content}"
                    else:
                        formatted_text = f"Error: {result.get('error', 'Unknown error')}"

                    response_data = {
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": {
                            "content": [
                                {
                                    "type": "text",
                                    "text": formatted_text
                                }
                            ]
                        }
                    }

                    json_content = json.dumps(response_data, ensure_ascii=False)
                    return Response(
                        content=json_content,
                        media_type="application/json",
                        headers={"Content-Type": "application/json; charset=utf-8"}
                    )


                elif tool_name == "update_note":
                    result = emacs_client.update_note(
                        arguments.get("identifier"),
                        arguments.get("content"),
                        arguments.get("section"),
                        arguments.get("mode", "append")
                    )
                    result = _sanitize_result(result)

                    if result.get("success"):
                        file_path = result.get("file", "")
                        mode = arguments.get("mode", "append")
                        section = arguments.get("section")
                        section_desc = section if section else "(whole file)"
                        formatted_text = f"Note updated successfully.\nFile: {file_path}\nMode: {mode}\nSection: {section_desc}"
                    else:
                        formatted_text = f"Error: {result.get('error', 'Unknown error')}"

                    response_data = {
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": {
                            "content": [
                                {
                                    "type": "text",
                                    "text": formatted_text
                                }
                            ]
                        }
                    }

                    json_content = json.dumps(response_data, ensure_ascii=False)
                    return Response(
                        content=json_content,
                        media_type="application/json",
                        headers={"Content-Type": "application/json; charset=utf-8"}
                    )

                elif tool_name == "list_notes":
                    result = emacs_client.list_notes(
                        arguments.get("node_type"),
                        arguments.get("status"),
                        arguments.get("limit", 50),
                        arguments.get("sort_by", "modified")
                    )
                    result = _sanitize_result(result)

                    if result.get("success"):
                        notes = result.get("notes", [])
                        total = result.get("total_found", 0)
                        lines = [f"Found {total} notes (showing {len(notes)}):"]
                        for note in notes:
                            status_str = f" [{note.get('status', '')}]" if note.get('status') else ""
                            type_str = f" ({note.get('node_type', '')})" if note.get('node_type') else ""
                            lines.append(f"- **{note.get('title', 'Untitled')}**{type_str}{status_str}")
                            lines.append(f"  ID: {note.get('id', 'N/A')} | Modified: {note.get('modified', 'N/A')}")
                        formatted_text = "\n".join(lines)
                    else:
                        formatted_text = f"Error: {result.get('error', 'Unknown error')}"

                    response_data = {
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": {
                            "content": [
                                {
                                    "type": "text",
                                    "text": formatted_text
                                }
                            ]
                        }
                    }

                    json_content = json.dumps(response_data, ensure_ascii=False)
                    return Response(
                        content=json_content,
                        media_type="application/json",
                        headers={"Content-Type": "application/json; charset=utf-8"}
                    )
                elif tool_name == "semantic_search":
                    result = emacs_client.semantic_search(
                        arguments.get("query"),
                        arguments.get("limit", 10),
                        arguments.get("cutoff", 0.55)
                    )
                    result = _sanitize_result(result)  # Sanitize control characters

                    # Format result like MCP tools do
                    formatted_text = f"Semantic search results for '{arguments.get('query')}':\n\n" + _format_semantic_search_results(result)

                    response_data = {
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": {
                            "content": [
                                {
                                    "type": "text",
                                    "text": formatted_text
                                }
                            ]
                        }
                    }

                    json_content = json.dumps(response_data, ensure_ascii=False)
                    return Response(
                        content=json_content,
                        media_type="application/json",
                        headers={"Content-Type": "application/json; charset=utf-8"}
                    )
                elif tool_name == "create_note":
                    title = arguments.get("title")
                    content = arguments.get("content")
                    note_type = arguments.get("type", "reference")
                    confidence = arguments.get("confidence", "medium")
                    url = arguments.get("url")
                    metadata = arguments.get("metadata", {})

                    result = emacs_client.create_note(title, content, note_type, confidence, url, metadata)

                    if result.get("success"):
                        note_info = result.get("note", {})
                        if note_type == "video":
                            response = f"🎥 Video note created successfully: '{title}'\n"
                            response += f"ID: {note_info.get('id', 'N/A')}\n"
                            response += f"File: {note_info.get('file', 'N/A')}\n"
                            if url:
                                response += f"URL: {url}"
                        else:
                            response = f"✅ Note created successfully: '{title}'\n"
                            response += f"ID: {note_info.get('id', 'N/A')}\n"
                            response += f"File: {note_info.get('file', 'N/A')}\n"
                            response += f"Type: {note_type}, Confidence: {confidence}"
                    else:
                        response = f"❌ Failed to create note: {result.get('message', 'Unknown error')}"

                    return JSONResponse({
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": {
                            "content": [{"type": "text", "text": response}]
                        }
                    })

                elif tool_name == "get_note_properties":
                    result = emacs_client.get_note_properties(arguments.get("identifier"))
                    return JSONResponse({"jsonrpc": "2.0", "id": rpc_request.get("id"), "result": result})

                elif tool_name == "delete_note":
                    result = emacs_client.delete_note(arguments.get("identifier"), arguments.get("archive", False))
                    return JSONResponse({"jsonrpc": "2.0", "id": rpc_request.get("id"), "result": result})

                elif tool_name == "rename_note":
                    result = emacs_client.rename_note(arguments.get("identifier"), arguments.get("new_title"))
                    return JSONResponse({"jsonrpc": "2.0", "id": rpc_request.get("id"), "result": result})

                elif tool_name == "manage_tags":
                    result = emacs_client.manage_tags(arguments.get("identifier"), arguments.get("action"), arguments.get("tag"))
                    return JSONResponse({"jsonrpc": "2.0", "id": rpc_request.get("id"), "result": result})

                elif tool_name == "add_link":
                    result = emacs_client.add_link(arguments.get("from_id"), arguments.get("to_id"), arguments.get("section"))
                    return JSONResponse({"jsonrpc": "2.0", "id": rpc_request.get("id"), "result": result})
                elif tool_name == "add_daily_entry":
                    timestamp = arguments.get("timestamp")
                    title = arguments.get("title")
                    points = arguments.get("points", [])
                    next_steps = arguments.get("next_steps", [])
                    tags = arguments.get("tags", [])
                    entry_type = arguments.get("entry_type", "journal")
                    
                    result = emacs_client.add_daily_entry(
                        timestamp, title, points, next_steps, tags, entry_type
                    )
                    
                    if result.get("success"):
                        # Return the exact response format the agent expects to see
                        entry_label = "TODO" if entry_type == "todo" else "JOURNAL"
                        response = f"✅ **{entry_label}** added to daily note: {title}"
                    else:
                        response = f"❌ Failed to add daily entry: {result.get('error', 'Unknown error')}"
                    
                    return JSONResponse({
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": {
                            "content": [{"type": "text", "text": response}]
                        }
                    })
                elif tool_name == "get_daily_content":
                    result = emacs_client.get_daily_content(
                        arguments.get("date")
                    )

                    # Result is a dict - ensure proper JSON encoding
                    response_data = {
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": result
                    }

                    json_content = json.dumps(response_data, ensure_ascii=False)
                    return Response(
                        content=json_content,
                        media_type="application/json",
                        headers={"Content-Type": "application/json; charset=utf-8"}
                    )
                elif tool_name == "generate_embeddings":
                    force = arguments.get("force", False)
                    result = emacs_client.generate_embeddings(force)

                    if result.get("success"):
                        count = result.get("count", 0)
                        response = f"✅ Generated {count} embeddings for org-roam notes"
                        if count == 0:
                            response += "\n(All notes already have current embeddings)"
                    else:
                        response = f"❌ Failed to generate embeddings: {result.get('error', 'Unknown error')}"

                    return JSONResponse({
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": {
                            "content": [{"type": "text", "text": response}]
                        }
                    })
                elif tool_name == "generate_note_embedding":
                    file_path = arguments.get("file_path")
                    result = emacs_client.generate_note_embedding(file_path)

                    if result.get("success"):
                        response = f"\u2705 {result.get('message', 'Embedding generated')}"
                    else:
                        response = f"\u274c Failed to generate embedding: {result.get('error', 'Unknown error')}"

                    return JSONResponse({
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": {
                            "content": [{"type": "text", "text": response}]
                        }
                    })
                elif tool_name == "add_inbox_entry":
                    command = arguments.get("command")
                    original_text = arguments.get("original_text")
                    linked_note_id = arguments.get("linked_note_id")
                    linked_note_title = arguments.get("linked_note_title")

                    result = emacs_client.add_inbox_entry(
                        command, original_text, linked_note_id, linked_note_title
                    )

                    if result.get("success"):
                        response = f"✅ Inbox entry added for /{command}"
                    else:
                        response = f"❌ Failed to add inbox entry: {result.get('error', 'Unknown error')}"

                    return JSONResponse({
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": {
                            "content": [{"type": "text", "text": response}]
                        }
                    })

                # Phase 2: Structured Node Types
                elif tool_name == "create_person":
                    name = arguments.get("name")
                    context = arguments.get("context")
                    follow_ups = arguments.get("follow_ups", [])
                    notes = arguments.get("notes")

                    result = emacs_client.create_person(name, context, follow_ups, notes)

                    if result.get("success"):
                        note_info = result.get("note", {})
                        response = f"👤 Person note created: '{name}'\n"
                        response += f"ID: {note_info.get('id', 'N/A')}\n"
                        response += f"File: {note_info.get('file', 'N/A')}"
                    else:
                        response = f"❌ Failed to create person: {result.get('error', 'Unknown error')}"

                    return JSONResponse({
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": {
                            "content": [{"type": "text", "text": response}],
                            "note": result.get("note", {})
                        }
                    })

                elif tool_name == "create_project":
                    title = arguments.get("title")
                    status = arguments.get("status", "active")
                    next_action = arguments.get("next_action")
                    notes = arguments.get("notes")
                    content = arguments.get("content")

                    result = emacs_client.create_project(title, status, next_action, notes, content)

                    if result.get("success"):
                        note_info = result.get("note", {})
                        response = f"📋 Project note created: '{title}'\n"
                        response += f"ID: {note_info.get('id', 'N/A')}\n"
                        response += f"Status: {status}\n"
                        response += f"File: {note_info.get('file', 'N/A')}"
                    else:
                        response = f"❌ Failed to create project: {result.get('error', 'Unknown error')}"

                    return JSONResponse({
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": {
                            "content": [{"type": "text", "text": response}],
                            "note": result.get("note", {})
                        }
                    })

                elif tool_name == "create_idea":
                    title = arguments.get("title")
                    one_liner = arguments.get("one_liner")
                    elaboration = arguments.get("elaboration")

                    result = emacs_client.create_idea(title, one_liner, elaboration)

                    if result.get("success"):
                        note_info = result.get("note", {})
                        response = f"💡 Idea note created: '{title}'\n"
                        response += f"ID: {note_info.get('id', 'N/A')}\n"
                        response += f"File: {note_info.get('file', 'N/A')}"
                    else:
                        response = f"❌ Failed to create idea: {result.get('error', 'Unknown error')}"

                    return JSONResponse({
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": {
                            "content": [{"type": "text", "text": response}],
                            "note": result.get("note", {})
                        }
                    })

                elif tool_name == "create_admin":
                    title = arguments.get("title")
                    due_date = arguments.get("due_date")
                    notes = arguments.get("notes")

                    result = emacs_client.create_admin(title, due_date, notes)

                    if result.get("success"):
                        note_info = result.get("note", {})
                        response = f"📝 Admin task created: '{title}'\n"
                        response += f"ID: {note_info.get('id', 'N/A')}\n"
                        if due_date:
                            response += f"Due: {due_date}\n"
                        response += f"File: {note_info.get('file', 'N/A')}"
                    else:
                        response = f"❌ Failed to create admin task: {result.get('error', 'Unknown error')}"

                    return JSONResponse({
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": {
                            "content": [{"type": "text", "text": response}],
                            "note": result.get("note", {})
                        }
                    })

                # Phase 3: Proactive Surfacing
                elif tool_name == "get_active_projects":
                    result = emacs_client.get_active_projects()
                    return JSONResponse({
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": result
                    })

                elif tool_name == "get_pending_followups":
                    result = emacs_client.get_pending_followups()
                    return JSONResponse({
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": result
                    })

                elif tool_name == "get_stale_projects":
                    days_threshold = arguments.get("days_threshold", 5)
                    result = emacs_client.get_stale_projects(days_threshold)
                    return JSONResponse({
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": result
                    })

                elif tool_name == "get_weekly_inbox":
                    days = arguments.get("days", 7)
                    result = emacs_client.get_weekly_inbox(days)
                    return JSONResponse({
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": result
                    })

                elif tool_name == "get_digest_data":
                    result = emacs_client.get_digest_data()
                    return JSONResponse({
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": result
                    })

                elif tool_name == "log_to_inbox":
                    text = arguments.get("text", "")
                    result = emacs_client.log_to_inbox(text)
                    return JSONResponse({
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": result
                    })

                elif tool_name == "get_dangling_followups":
                    result = emacs_client.get_dangling_followups()
                    return JSONResponse({
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": result
                    })

                elif tool_name == "update_note":
                    identifier = arguments.get("identifier")
                    content = arguments.get("content")
                    section = arguments.get("section")
                    mode = arguments.get("mode", "append")
                    result = emacs_client.update_note(identifier, content, section, mode)
                    return JSONResponse({
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": result
                    })

                elif tool_name == "list_notes":
                    node_type = arguments.get("node_type")
                    status = arguments.get("status")
                    limit = arguments.get("limit", 50)
                    sort_by = arguments.get("sort_by", "modified")
                    result = emacs_client.list_notes(node_type, status, limit, sort_by)
                    return JSONResponse({
                        "jsonrpc": "2.0",
                        "id": rpc_request.get("id"),
                        "result": result
                    })

                return JSONResponse({
                    "jsonrpc": "2.0",
                    "id": rpc_request.get("id"),
                    "error": {
                        "code": -32601,
                        "message": f"Unknown tool: {tool_name}"
                    }
                })
            
            # Handle initialize request
            elif rpc_request.get("method") == "initialize":
                return JSONResponse({
                    "jsonrpc": "2.0",
                    "id": rpc_request.get("id"),
                    "result": {
                        "protocolVersion": "2025-03-26",
                        "capabilities": {
                            "tools": {}
                        },
                        "serverInfo": {
                            "name": "org-roam-mcp",
                            "version": __version__
                        }
                    }
                })

            # Handle initialized notification (required by MCP spec)
            elif rpc_request.get("method") == "notifications/initialized":
                # Notifications don't require a response body, but should return 200
                return Response(content="", status_code=200, media_type="application/json")

            else:
                return JSONResponse({
                    "jsonrpc": "2.0",
                    "id": rpc_request.get("id"),
                    "error": {
                        "code": -32601,
                        "message": f"Unknown method: {rpc_request.get('method')}"
                    }
                })
        
        except Exception as e:
            logger.error(f"Error handling MCP request: {e}")
            return JSONResponse({
                "jsonrpc": "2.0",
                "id": rpc_request.get("id") if "rpc_request" in locals() else None,
                "error": {
                    "code": -32603,
                    "message": f"Internal error: {str(e)}"
                }
            }, status_code=500)

    # Create SSE transport for MCP clients like Agent Zero
    # The endpoint "/mcp" is where clients will POST messages after connecting via SSE
    sse_transport = SseServerTransport("/mcp")

    async def handle_sse(request: Request):
        """Handle SSE connections from MCP clients like Agent Zero."""
        logger.info("SSE connection established")
        async with sse_transport.connect_sse(
            request.scope, request.receive, request._send
        ) as streams:
            await app.run(
                streams[0],
                streams[1],
                InitializationOptions(
                    server_name="org-roam-mcp",
                    server_version=__version__,
                    capabilities=ServerCapabilities()
                )
            )
        # Return empty response to avoid NoneType error when client disconnects
        return Response()

    starlette_app = Starlette(
        routes=[
            # Existing JSON-RPC endpoints for n8n
            Route("/", health_check, methods=["GET"]),
            Route("/", handle_mcp_request, methods=["POST"]),
            # SSE endpoints for MCP clients like Agent Zero
            Route("/sse", endpoint=handle_sse, methods=["GET"]),
            Mount("/mcp", app=sse_transport.handle_post_message),
        ]
    )

    # Add CORS middleware
    starlette_app.add_middleware(
        CORSMiddleware,
        allow_origins=["*"],
        allow_credentials=True,
        allow_methods=["*"],
        allow_headers=["*"],
    )

    return starlette_app

async def main():
    """Main async entry point for the MCP server."""
    import sys

    # Check if --stdio flag is provided
    if "--stdio" in sys.argv:
        # Run in stdio mode for MCP clients like Codex
        from mcp.server.stdio import stdio_server
        logger.info("Starting org-roam MCP server in stdio mode...")
        async with stdio_server() as (read_stream, write_stream):
            await app.run(
                read_stream,
                write_stream,
                InitializationOptions(
                    server_name="org-roam-mcp",
                    server_version=__version__,
                    capabilities=ServerCapabilities()
                )
            )
    else:
        # Run in HTTP mode for n8n and other HTTP clients
        import uvicorn
        import os
        port = int(os.environ.get("MCP_PORT", "8001"))
        logger.info(f"Starting org-roam MCP HTTP server on port {port}...")
        logger.info("Available endpoints:")
        logger.info("  GET  / - Health check")
        logger.info("  POST / - MCP JSON-RPC endpoint for n8n")
        logger.info("  GET  /sse - SSE endpoint for MCP clients (Agent Zero)")
        logger.info("  POST /mcp - SSE message endpoint for MCP clients")

        starlette_app = create_starlette_app()

        config = uvicorn.Config(
            starlette_app,
            host="0.0.0.0",
            port=port,
            log_level="info"
        )
        server = uvicorn.Server(config)
        await server.serve()


def cli_main():
    """Synchronous entry point for console script."""
    asyncio.run(main())


if __name__ == "__main__":
    cli_main()
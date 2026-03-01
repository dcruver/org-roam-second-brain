"""Emacs client for executing elisp functions via emacsclient."""

import json
import logging
import os
import subprocess
from typing import Any, Dict, List, Optional, Union

logger = logging.getLogger(__name__)


class EmacsClientError(Exception):
    """Raised when emacsclient operation fails."""
    pass


class EmacsClient:
    """Client for communicating with Emacs via emacsclient."""
    
    def __init__(self, server_file: Optional[str] = None):
        """Initialize EmacsClient.
        
        Args:
            server_file: Path to the Emacs server file (defaults to EMACS_SERVER_FILE env var or ~/emacs-server/server)
        """
        if server_file:
            self.server_file = server_file
        else:
            # Check for environment variable first, then fall back to HOME
            env_server_file = os.environ.get('EMACS_SERVER_FILE')
            if env_server_file:
                self.server_file = env_server_file
                logger.info(f"Using EMACS_SERVER_FILE environment variable: {self.server_file}")
            else:
                self.server_file = os.path.expanduser("~/.emacs.d/server/server")
                logger.info(f"Using default Emacs server path: {self.server_file}")
        
        self.timeout = 300  # seconds
        
        # Log the resolved server file path for debugging
        logger.info(f"EmacsClient initialized with server file: {self.server_file}")
        logger.info(f"Server file exists: {os.path.exists(self.server_file)}")
        if os.path.exists(self.server_file):
            logger.info(f"Server file permissions: {oct(os.stat(self.server_file).st_mode)}")
        else:
            logger.warning(f"Server file does not exist: {self.server_file}")
        logger.info(f"Current user HOME: {os.path.expanduser('~')}")
        logger.info(f"Current working directory: {os.getcwd()}")

        # Load required Emacs packages from local directory
        self._load_emacs_packages()

    def _load_emacs_packages(self) -> None:
        """Verify that required elisp packages are loaded in Emacs."""
        try:
            logger.info("Verifying Emacs packages are loaded...")

            # Check if packages are loaded
            packages_to_check = [
                'org-roam-vector-search',
                'org-roam-second-brain',
                'org-roam-api'
            ]

            missing_packages = []
            for package in packages_to_check:
                if self._is_package_available(package):
                    logger.info(f"Package {package} is loaded")
                else:
                    missing_packages.append(package)
                    logger.warning(f"Package {package} is not loaded")

            if missing_packages:
                logger.error(f"Missing packages: {', '.join(missing_packages)}")
                logger.error("Please ensure the Emacs configuration includes the MCP package setup")
                logger.error("The install script should have added this automatically")
                raise RuntimeError(f"Required Emacs packages not loaded: {missing_packages}")
            else:
                logger.info("All required Emacs packages are loaded")

        except Exception as e:
            logger.error(f"Failed to verify Emacs packages: {e}")
            raise  # This is critical - we can't function without the packages

    def _is_package_available(self, package_name: str) -> bool:
        """Check if a package is loaded in Emacs."""
        try:
            # Check if the package feature is loaded
            check_expr = f'(featurep (quote {package_name}))'
            if os.path.exists(self.server_file):
                command = f"emacsclient --server-file={self.server_file} -e '{check_expr}'"
            else:
                command = f"emacsclient -e '{check_expr}'"
            response = self._execute_command(command)
            return response.strip() == 't'
        except Exception as e:
            logger.error(f"Failed to check package {package_name}: {e}")
            return False

    def _escape_for_elisp(self, value: str) -> str:
        """Escape string for safe elisp evaluation.
        
        Args:
            value: String to escape
            
        Returns:
            Escaped string safe for elisp
        """
        if not isinstance(value, str):
            value = str(value)

        return (value
                .replace("\\", "\\\\")
                .replace('"', '\\"')
                .replace('\n', '\\n')
                .replace('\t', '\\t'))
    
    def _build_elisp_list(self, items: List[str]) -> str:
        """Build elisp list from Python list.
        
        Args:
            items: List of strings
            
        Returns:
            Elisp list representation
        """
        escaped_items = [f'"{self._escape_for_elisp(item)}"' for item in items]
        return f"(list {' '.join(escaped_items)})"
    
    def _execute_command(self, command: str) -> str:
        """Execute emacsclient command.
        
        Args:
            command: Shell command to execute
            
        Returns:
            Command output
            
        Raises:
            EmacsClientError: If command fails
        """
        try:
            logger.info(f"Executing command: {command}")
            logger.info(f"Server file path: {self.server_file}")
            logger.info(f"Server file exists: {os.path.exists(self.server_file)}")
            
            result = subprocess.run(
                command,
                shell=True,
                capture_output=True,
                text=True,
                timeout=self.timeout,
                check=False
            )
            
            logger.info(f"Command return code: {result.returncode}")
            logger.info(f"Command stderr: {result.stderr}")
            logger.info(f"Command stdout: {result.stdout}")
            
            if result.returncode != 0:
                error_msg = f"emacsclient failed with code {result.returncode}: {result.stderr}"
                logger.error(error_msg)
                raise EmacsClientError(error_msg)
            
            logger.debug(f"Command output: {result.stdout}")
            return result.stdout.strip()
            
        except subprocess.TimeoutExpired as e:
            error_msg = f"emacsclient timed out after {self.timeout} seconds"
            logger.error(error_msg)
            raise EmacsClientError(error_msg) from e
        except Exception as e:
            error_msg = f"Failed to execute emacsclient: {e}"
            logger.error(error_msg)
            raise EmacsClientError(error_msg) from e

    def _parse_json_with_control_char_fix(self, json_str: str) -> Dict[str, Any]:
        """Parse JSON string with control character escaping.

        Args:
            json_str: JSON string potentially containing unescaped control characters

        Returns:
            Parsed JSON data

        Raises:
            json.JSONDecodeError: If parsing fails even after fixing
        """
        # Escape ALL control characters in one pass for efficiency
        # This handles: 0-31 (ASCII control), 127 (DEL), and 128-159 (C1 control codes)
        result = []
        i = 0
        while i < len(json_str):
            char = json_str[i]
            code = ord(char)

            # Check if this is an already-escaped sequence (backslash followed by valid escape char)
            if char == '\\' and i + 1 < len(json_str):
                next_char = json_str[i + 1]
                # Valid JSON escape sequences: \", \\, \/, \b, \f, \n, \r, \t, \uXXXX
                if next_char in '"\\bfnrt/':
                    result.append(char)
                    result.append(next_char)
                    i += 2
                    continue
                elif next_char == 'u' and i + 5 < len(json_str):
                    # \uXXXX escape - keep as-is
                    result.append(json_str[i:i+6])
                    i += 6
                    continue

            # Escape unescaped control characters
            if code < 32:  # ASCII control characters
                if code == 9:  # tab
                    result.append('\\t')
                elif code == 10:  # newline
                    result.append('\\n')
                elif code == 13:  # carriage return
                    result.append('\\r')
                else:
                    result.append(f'\\u{code:04x}')
            elif code == 127:  # DEL
                result.append('\\u007f')
            elif 128 <= code <= 159:  # C1 control codes
                result.append(f'\\u{code:04x}')
            else:
                result.append(char)
            i += 1

        cleaned = ''.join(result)
        return json.loads(cleaned)

    def _pre_clean_response(self, response: str) -> str:
        """Pre-clean response by removing raw control characters that would break JSON parsing.

        This handles the case where emacsclient returns strings with unescaped control
        characters embedded in JSON string values.
        """
        result = []
        in_string = False
        i = 0
        while i < len(response):
            char = response[i]

            # Track string boundaries (but be careful of escaped quotes)
            if char == '"' and (i == 0 or response[i-1] != '\\'):
                in_string = not in_string
                result.append(char)
                i += 1
                continue

            # If we're inside a JSON string value, escape control characters
            if in_string:
                code = ord(char)
                if code < 32:  # Control characters
                    if code == 9:  # tab
                        result.append('\\t')
                    elif code == 10:  # newline
                        result.append('\\n')
                    elif code == 13:  # carriage return
                        result.append('\\r')
                    else:
                        result.append(f'\\u{code:04x}')
                elif code == 127:  # DEL
                    result.append('\\u007f')
                else:
                    result.append(char)
            else:
                result.append(char)
            i += 1

        return ''.join(result)

    def _parse_json_response(self, response: str) -> Dict[str, Any]:
        """Parse JSON response from elisp function.

        Args:
            response: Raw response string

        Returns:
            Parsed JSON data

        Raises:
            EmacsClientError: If JSON parsing fails
        """
        # Pre-clean the response to handle raw control characters in string values
        response = self._pre_clean_response(response)

        try:
            # Handle case where response might be a character array
            parsed = json.loads(response)

            # emacsclient returns quoted strings, so if the elisp function
            # returns a JSON string, we get ""{"success":true,...}"" which
            # json.loads() parses to a string. Parse it again if needed.
            if isinstance(parsed, str):
                # Pre-clean the inner string as well
                parsed = self._pre_clean_response(parsed)
                try:
                    parsed = json.loads(parsed)
                except json.JSONDecodeError:
                    # The inner string might have control characters - use the cleaning logic
                    parsed = self._parse_json_with_control_char_fix(parsed)

            # Check if we got a character array (keys are numbers)
            if isinstance(parsed, dict) and all(key.isdigit() for key in parsed.keys()):
                # Reconstruct string from character array
                reconstructed = ''.join(parsed[str(i)] for i in sorted(int(k) for k in parsed.keys()))
                parsed = json.loads(reconstructed)

            return parsed
        except json.JSONDecodeError as e:
            # Emacs json-encode doesn't always properly escape control characters in strings
            # This is particularly problematic with full_content fields containing org files
            logger.warning(f"Initial JSON parse failed: {e}. Attempting to fix control characters...")

            try:
                # Log the problematic area for debugging
                error_pos = getattr(e, 'pos', 0)
                snippet_start = max(0, error_pos - 100)
                snippet_end = min(len(response), error_pos + 100)
                logger.debug(f"Parse error at position {error_pos}")
                logger.debug(f"Context: {repr(response[snippet_start:snippet_end])}")

                # Use the control character fixing method
                parsed = self._parse_json_with_control_char_fix(response)
                logger.info("Successfully parsed after fixing control characters in strings")

                # After fixing, check if we got a string (double-quoted by emacsclient)
                if isinstance(parsed, str):
                    try:
                        parsed = json.loads(parsed)
                    except json.JSONDecodeError:
                        # Still has control characters in the inner string
                        parsed = self._parse_json_with_control_char_fix(parsed)

                return parsed

            except Exception as e2:
                error_msg = f"Failed to parse JSON response: {e}. Raw response (first 1000 chars): {response[:1000]}"
                logger.error(error_msg)
                logger.error(f"Cleaning attempt also failed: {e2}")
                raise EmacsClientError(error_msg) from e
    
    def eval_elisp(self, expression: str) -> Dict[str, Any]:
        """Evaluate elisp expression and return parsed JSON response.
        
        Args:
            expression: Elisp expression to evaluate
            
        Returns:
            Parsed JSON response from elisp function
            
        Raises:
            EmacsClientError: If evaluation fails
        """
        # Escape the elisp expression for shell
        safe_expression = (expression
                          .replace("\\", "\\\\")
                          .replace('"', '\\"')
                          .replace('`', '\\`')
                          .replace('$', '\\$'))

        if os.path.exists(self.server_file):
            command = f'emacsclient --server-file={self.server_file} -e "{safe_expression}"'
        else:
            command = f'emacsclient -e "{safe_expression}"'
        
        response = self._execute_command(command)
        return self._parse_json_response(response)
    
    def contextual_search(self, query: str, limit: int = 10) -> Dict[str, Any]:
        """Perform contextual search in org-roam.

        Args:
            query: Search query
            limit: Maximum number of results

        Returns:
            Search results with enhanced context
        """
        expression = f'(my/api-contextual-search "{self._escape_for_elisp(query)}" {limit})'
        return self.eval_elisp(expression)

    def semantic_search(self, query: str, limit: int = 10, cutoff: float = 0.55) -> Dict[str, Any]:
        """Perform semantic vector search using org-roam-semantic.

        Args:
            query: Search query
            limit: Maximum number of results
            cutoff: Similarity threshold (0.0-1.0)

        Returns:
            Semantically similar notes with full content and similarity scores
        """
        expression = f'(my/api-semantic-search "{self._escape_for_elisp(query)}" {limit} {cutoff})'
        return self.eval_elisp(expression)
    
    def search_notes(self, query: str) -> Dict[str, Any]:
        """Perform basic search in org-roam.
        
        Args:
            query: Search query
            
        Returns:
            Basic search results
        """
        expression = f'(my/api-search-notes "{self._escape_for_elisp(query)}")'
        return self.eval_elisp(expression)
    
    def create_note(
        self,
        title: str,
        content: str,
        note_type: str = "reference",
        confidence: str = "medium",
        url: Optional[str] = None,
        metadata: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """Create a new org-roam note with optional structured formatting.

        Args:
            title: Note title
            content: Note content (can be plain text or structured)
            note_type: Type of note (reference, video, etc.)
            confidence: Confidence level (high, medium, low)
            url: Optional URL for video/web content
            metadata: Optional metadata for structured content

        Returns:
            Creation result with note details
        """
        # If this is a video note with URL and metadata, format it specially
        if note_type == "video" and url:
            content = self._format_video_content(content, url, metadata or {})

        expression = (f'(my/api-create-note "{self._escape_for_elisp(title)}" '
                     f'"{self._escape_for_elisp(content)}" '
                     f'"{self._escape_for_elisp(note_type)}" '
                     f'"{self._escape_for_elisp(confidence)}")')
        return self.eval_elisp(expression)

    def _format_video_content(
        self,
        transcript: str,
        url: str,
        metadata: Dict[str, Any]
    ) -> str:
        """Format video content with structured layout.

        Args:
            transcript: Organized transcript content
            url: Video URL
            metadata: Video metadata

        Returns:
            Formatted note content
        """
        from datetime import datetime
        current_time = datetime.now()
        date_str = current_time.strftime("%Y-%m-%d")
        time_str = current_time.strftime("%H:%M")

        # Build structured note content
        note_content = f"""* Video Information
- *URL:* {url}
- *Captured:* {date_str} {time_str}
- *Type:* YouTube Video
"""

        # Add metadata if provided
        if metadata:
            for key, value in metadata.items():
                if value:  # Only add non-empty values
                    note_content += f"- *{key.title()}:* {value}\n"

        note_content += f"""
-----

{transcript}

-----
/Note created from YouTube video transcript using org-roam-mcp/"""

        return note_content
    
    def add_daily_entry(
        self,
        timestamp: str,
        title: str,
        points: List[str],
        next_steps: Optional[List[str]] = None,
        tags: Optional[List[str]] = None,
        entry_type: str = "journal"
    ) -> Dict[str, Any]:
        """Add structured entry to daily note.
        
        Args:
            timestamp: Entry timestamp (HH:MM format)
            title: Entry title
            points: Main points/content
            next_steps: Optional action items
            tags: Optional tags
            entry_type: "journal" or "todo" - determines formatting
            
        Returns:
            Result of adding entry
        """
        next_steps = next_steps or []
        tags = tags or []
        
        # Format title based on entry type
        if entry_type.lower() == "todo":
            formatted_title = f"TODO {title}"
        else:
            formatted_title = title
            
        points_list = self._build_elisp_list(points)
        steps_list = self._build_elisp_list(next_steps) 
        tags_list = self._build_elisp_list(tags)
        
        expression = (f'(my/add-daily-entry-structured "{self._escape_for_elisp(timestamp)}" '
                     f'"{self._escape_for_elisp(formatted_title)}" {points_list} {steps_list} {tags_list})')

        # This function returns a plain string message, not JSON
        # Use _execute_command directly instead of eval_elisp
        safe_expression = (expression
                          .replace("\\", "\\\\")
                          .replace('"', '\\"')
                          .replace('`', '\\`')
                          .replace('$', '\\$'))

        if os.path.exists(self.server_file):
            command = f'emacsclient --server-file={self.server_file} -e "{safe_expression}"'
        else:
            command = f'emacsclient -e "{safe_expression}"'

        try:
            response = self._execute_command(command)
            # Response is a quoted string like "Added journal entry..."
            return {"success": True, "message": response.strip().strip('"')}
        except EmacsClientError as e:
            return {"success": False, "error": str(e)}
    
    def get_daily_content(self, date: Optional[str] = None) -> Dict[str, Any]:
        """Get content of daily note.

        Args:
            date: Date in YYYY-MM-DD format (defaults to today)

        Returns:
            Daily note content
        """
        if date:
            expression = f'(my/get-daily-note-content "{date}")'
        else:
            expression = '(my/get-daily-note-content)'

        # This returns raw content, not JSON
        try:
            if os.path.exists(self.server_file):
                command = f'emacsclient --server-file={self.server_file} -e "{expression}"'
            else:
                command = f'emacsclient -e "{expression}"'
            response = self._execute_command(command)

            # Clean the response - it may have literal \n characters
            content = response.replace('\\n', '\n').strip()
            if content.startswith('"') and content.endswith('"'):
                content = content[1:-1]  # Remove surrounding quotes

            return {
                "success": True,
                "content": content,
                "date": date or "today"
            }
        except Exception as e:
            logger.error(f"Failed to get daily content: {e}")
            return {
                "success": False,
                "error": str(e),
                "content": "",
                "date": date or "today"
            }

    def generate_embeddings(self, force: bool = False) -> Dict[str, Any]:
        """Generate embeddings for all org-roam notes using org-roam-semantic.

        This calls the org-roam-semantic-generate-all-embeddings function which
        generates vector embeddings for all notes that don't have them or have
        stale embeddings.

        Args:
            force: If True, regenerate embeddings even for notes that already have them

        Returns:
            Result with count of embeddings generated
        """
        # Call the org-roam-semantic function
        # This function processes all notes and returns a count
        if force:
            expression = '(org-roam-semantic-generate-all-embeddings t)'
        else:
            expression = '(org-roam-semantic-generate-all-embeddings)'

        try:
            # This function may take a while for large corpora (handled by default subprocess timeout)
            # The function returns the number of embeddings generated
            if os.path.exists(self.server_file):
                command = f'emacsclient --server-file={self.server_file} -e "{expression}"'
            else:
                command = f'emacsclient -e "{expression}"'
            response = self._execute_command(command)

            # Parse the response - might be a number or a message string
            response_str = response.strip().strip('"')  # Remove quotes if present

            # Try to parse as number first
            try:
                count = int(response_str)
            except ValueError:
                # If it's a message string, extract the number
                # Format: "Embedding generation complete: 20 processed, 130 skipped"
                import re
                match = re.search(r'(\d+)\s+processed', response_str)
                count = int(match.group(1)) if match else 0

            # Save all modified org-roam buffers to persist embeddings to disk
            # This ensures the agent can immediately see the updated files
            if os.path.exists(self.server_file):
                save_cmd = f'emacsclient --server-file={self.server_file} -e "(save-some-buffers t (lambda () (and (buffer-file-name) (string-match-p \\"\\\\.org\\$\\" (buffer-file-name)))))"'
            else:
                save_cmd = f'emacsclient -e "(save-some-buffers t (lambda () (and (buffer-file-name) (string-match-p \\"\\\\.org\\$\\" (buffer-file-name)))))"'
            try:
                self._execute_command(save_cmd)
                logger.info(f"Saved all modified org-roam buffers")
            except Exception as save_err:
                logger.warning(f"Failed to save buffers after embedding generation: {save_err}")

            return {
                "success": True,
                "count": count,
                "message": f"Generated {count} embeddings"
            }
        except Exception as e:
            logger.error(f"Failed to generate embeddings: {e}")
            return {
                "success": False,
                "error": str(e),
                "count": 0
            }



    def generate_note_embedding(self, file_path: str) -> Dict[str, Any]:
        """Generate embedding for a single org-roam note file.

        Calls org-roam-semantic-generate-embedding for one file instead of
        batch-processing all notes. Much faster for newly created/updated notes.

        Args:
            file_path: Absolute path to the org file

        Returns:
            Result with success status and message
        """
        escaped_path = self._escape_for_elisp(file_path)
        # Use single quotes for shell to preserve double quotes in elisp string
        elisp = f'(org-roam-semantic-generate-embedding "{escaped_path}")'

        try:
            if os.path.exists(self.server_file):
                command = f"emacsclient --server-file={self.server_file} -e '{elisp}'"
            else:
                command = f"emacsclient -e '{elisp}'"
            response = self._execute_command(command)

            # Response like: "Chunk embedding generation complete for file.org: 12 processed, 1 skipped"
            response_str = response.strip().strip('"')

            # Save modified org buffer to persist embedding to disk
            save_elisp = f'(let ((buf (find-buffer-visiting "{escaped_path}"))) (when buf (with-current-buffer buf (save-buffer))))'
            if os.path.exists(self.server_file):
                save_cmd = f"emacsclient --server-file={self.server_file} -e '{save_elisp}'"
            else:
                save_cmd = f"emacsclient -e '{save_elisp}'"
            try:
                self._execute_command(save_cmd)
            except Exception as save_err:
                logger.warning(f"Failed to save buffer after embedding: {save_err}")

            return {
                "success": True,
                "message": response_str,
                "file": file_path
            }
        except Exception as e:
            logger.error(f"Failed to generate note embedding: {e}")
            return {
                "success": False,
                "error": str(e),
                "file": file_path
            }

    def add_inbox_entry(
        self,
        command: str,
        original_text: str,
        linked_note_id: Optional[str] = None,
        linked_note_title: Optional[str] = None
    ) -> Dict[str, Any]:
        """Add an inbox entry to today's daily note for audit trail.

        Args:
            command: The command used (e.g., 'journal', 'note', 'yt')
            original_text: The full original message including the command
            linked_note_id: Optional ID of a note that was created
            linked_note_title: Optional title of a note that was created

        Returns:
            Result with success status
        """
        # Build the elisp call
        if linked_note_id and linked_note_title:
            expression = (
                f'(my/api-add-inbox-entry '
                f'"{self._escape_for_elisp(command)}" '
                f'"{self._escape_for_elisp(original_text)}" '
                f'"{self._escape_for_elisp(linked_note_id)}" '
                f'"{self._escape_for_elisp(linked_note_title)}")'
            )
        else:
            expression = (
                f'(my/api-add-inbox-entry '
                f'"{self._escape_for_elisp(command)}" '
                f'"{self._escape_for_elisp(original_text)}")'
            )

        return self.eval_elisp(expression)

    def sync_database(self, force: bool = True) -> Dict[str, Any]:
        """Sync org-roam database.

        Args:
            force: Whether to force sync

        Returns:
            Sync result
        """
        force_param = "'force" if force else "nil"
        expression = f"(org-roam-db-sync {force_param})"

        try:
            self.eval_elisp(expression)
            return {"success": True, "message": "Database synced successfully"}
        except EmacsClientError as e:
            if "nil" in str(e).lower():
                return {"success": True, "message": "Database synced successfully"}
            logger.error(f"Database sync failed: {e}")
            return {"success": False, "error": str(e)}

    # =========================================================================
    # Structured Node Types (Phase 2)
    # =========================================================================

    def create_person(
        self,
        name: str,
        context: Optional[str] = None,
        follow_ups: Optional[List[str]] = None,
        notes: Optional[str] = None
    ) -> Dict[str, Any]:
        """Create a person node with structured fields.

        Args:
            name: Person's name
            context: How you know them, their role
            follow_ups: List of action items related to this person
            notes: Additional notes about this person

        Returns:
            Creation result with note details
        """
        # Build the elisp call with optional parameters
        params = [f'"{self._escape_for_elisp(name)}"']

        if context:
            params.append(f'"{self._escape_for_elisp(context)}"')
        else:
            params.append('nil')

        if follow_ups:
            params.append(self._build_elisp_list(follow_ups))
        else:
            params.append('nil')

        if notes:
            params.append(f'"{self._escape_for_elisp(notes)}"')
        else:
            params.append('nil')

        expression = f'(my/api-create-person {" ".join(params)})'
        return self.eval_elisp(expression)

    def create_project(
        self,
        title: str,
        status: Optional[str] = None,
        next_action: Optional[str] = None,
        notes: Optional[str] = None,
        content: Optional[str] = None
    ) -> Dict[str, Any]:
        """Create a project node with structured fields.

        Args:
            title: Project name
            status: Project status (active, waiting, blocked, someday, done)
            next_action: Next actionable step
            notes: Additional notes about this project
            content: Full org-mode content (bypasses template if provided)

        Returns:
            Creation result with note details
        """
        # If full content provided, write file directly instead of using elisp template
        if content:
            return self._create_project_with_content(title, status or "active", content)
        
        # Otherwise use the elisp template approach
        params = [f'"{self._escape_for_elisp(title)}"']

        if status:
            params.append(f'"{self._escape_for_elisp(status)}"')
        else:
            params.append('nil')

        if next_action:
            params.append(f'"{self._escape_for_elisp(next_action)}"')
        else:
            params.append('nil')

        if notes:
            params.append(f'"{self._escape_for_elisp(notes)}"')
        else:
            params.append('nil')

        expression = f'(my/api-create-project {" ".join(params)})'
        return self.eval_elisp(expression)

    def _create_project_with_content(
        self,
        title: str,
        status: str,
        content: str
    ) -> Dict[str, Any]:
        """Create a project note with full custom content."""
        import time
        import re as re_module
        
        note_id = str(int(time.time()))
        safe_title = re_module.sub(r'[^a-zA-Z0-9-]', '-', title.lower())
        safe_title = re_module.sub(r'-+', '-', safe_title).strip('-')[:50]
        filename = f"{safe_title}-{note_id}.org"
        
        org_roam_dir = self._get_org_roam_directory()
        file_path = os.path.join(org_roam_dir, "projects", filename)
        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        
        if content.strip().startswith(":PROPERTIES:"):
            file_content = content
        else:
            file_content = f""":PROPERTIES:
:ID: {note_id}
:NODE-TYPE: project
:STATUS: {status}
:END:
#+title: {title}
#+filetags: :project:

{content}
"""
        
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(file_content)
        
        self._eval_elisp_raw('(org-roam-db-sync)')
        
        try:
            self._eval_elisp_raw(f'(my/api--generate-and-save-embedding "{self._escape_for_elisp(file_path)}")')
        except Exception:
            pass
        
        return {
            "success": True,
            "message": "Project note created successfully",
            "note": {
                "id": note_id,
                "title": title,
                "file": file_path,
                "node_type": "project",
                "status": status
            }
        }


    def _eval_elisp_raw(self, expression: str) -> str:
        """Evaluate elisp expression and return raw string (no JSON parsing)."""
        safe_expression = (expression
                          .replace("\\", "\\\\")
                          .replace('"', '\\"')
                          .replace('`', '\\`')
                          .replace('$', '\\$'))
        if os.path.exists(self.server_file):
            command = f'emacsclient --server-file={self.server_file} -e "{safe_expression}"'
        else:
            command = f'emacsclient -e "{safe_expression}"'
        return self._execute_command(command)

    def _get_org_roam_directory(self) -> str:
        """Get the org-roam directory path."""
        # Use _execute_command directly since this returns a plain string, not JSON
        expression = '(expand-file-name org-roam-directory)'
        if os.path.exists(self.server_file):
            command = f'emacsclient --server-file={self.server_file} -e "{expression}"'
        else:
            command = f'emacsclient -e "{expression}"'
        try:
            result = self._execute_command(command)
            return result.strip().strip('"')
        except Exception:
            return os.path.expanduser("~/org-roam")

    def create_idea(
        self,
        title: str,
        one_liner: str,
        elaboration: Optional[str] = None
    ) -> Dict[str, Any]:
        """Create an idea node with structured fields.

        Args:
            title: Idea title
            one_liner: Brief summary of the insight
            elaboration: Detailed explanation (optional)

        Returns:
            Creation result with note details
        """
        params = [
            f'"{self._escape_for_elisp(title)}"',
            f'"{self._escape_for_elisp(one_liner)}"'
        ]

        if elaboration:
            params.append(f'"{self._escape_for_elisp(elaboration)}"')
        else:
            params.append('nil')

        expression = f'(my/api-create-idea {" ".join(params)})'
        return self.eval_elisp(expression)

    def create_admin(
        self,
        title: str,
        due_date: Optional[str] = None,
        notes: Optional[str] = None
    ) -> Dict[str, Any]:
        """Create an admin task node with structured fields.

        Args:
            title: Task title
            due_date: Due date in YYYY-MM-DD format
            notes: Additional notes about this task

        Returns:
            Creation result with note details
        """
        params = [f'"{self._escape_for_elisp(title)}"']

        if due_date:
            params.append(f'"{self._escape_for_elisp(due_date)}"')
        else:
            params.append('nil')

        if notes:
            params.append(f'"{self._escape_for_elisp(notes)}"')
        else:
            params.append('nil')

        expression = f'(my/api-create-admin {" ".join(params)})'
        return self.eval_elisp(expression)

    # =========================================================================
    # Proactive Surfacing (Phase 3)
    # =========================================================================

    def get_active_projects(self) -> Dict[str, Any]:
        """Get all active projects with their next actions.

        Returns:
            Active projects ordered by last modified date
        """
        expression = '(my/api-get-active-projects)'
        return self.eval_elisp(expression)

    def get_pending_followups(self) -> Dict[str, Any]:
        """Get all people with pending follow-up items.

        Returns:
            People with unchecked follow-ups, sorted by count
        """
        expression = '(my/api-get-pending-followups)'
        return self.eval_elisp(expression)

    def get_stale_projects(self, days_threshold: int = 5) -> Dict[str, Any]:
        """Get projects with no activity in X days.

        Args:
            days_threshold: Number of days of inactivity to consider stale

        Returns:
            Stale projects sorted by days since last modified
        """
        expression = f'(my/api-get-stale-projects {days_threshold})'
        return self.eval_elisp(expression)

    def get_weekly_inbox(self, days: int = 7) -> Dict[str, Any]:
        """Get inbox entries from the past N days.

        Args:
            days: Number of days to look back

        Returns:
            Inbox entries grouped by day
        """
        expression = f'(my/api-get-weekly-inbox {days})'
        return self.eval_elisp(expression)

    def get_digest_data(self) -> Dict[str, Any]:
        """Get all data needed for daily digest in a single call.

        Combines active projects, pending follow-ups, and stale projects
        for efficient digest generation.

        Returns:
            Combined digest data
        """
        expression = '(my/api-get-digest-data)'
        return self.eval_elisp(expression)

    def log_to_inbox(self, text: str) -> Dict[str, Any]:
        """Log text to inbox and auto-create person nodes for [[Name]] links.

        Args:
            text: Text to log, may contain [[Name]] backlinks

        Returns:
            Result with success status and list of created person nodes
        """
        expression = f'(my/api-log-to-inbox "{self._escape_for_elisp(text)}")'
        return self.eval_elisp(expression)

    def get_dangling_followups(self) -> Dict[str, Any]:
        """Find unchecked items with [[Name]] links where the person doesn't exist.

        Scans all org files for follow-ups mentioning untracked people.

        Returns:
            List of untracked people with their follow-up items
        """
        expression = '(my/api-get-dangling-followups)'
        return self.eval_elisp(expression)

    def change_task_state(self, file: str, heading: str, new_state: str):
        """Change the TODO state of a task using emacsclient.
        
        This triggers org-after-todo-state-change-hook, which logs to daily notes.
        """
        try:
            escaped_file = self._escape_for_elisp(file)
            escaped_heading = self._escape_for_elisp(heading)
            escaped_state = self._escape_for_elisp(new_state)
            
            elisp = f'(my/org-roam-change-task-state "{escaped_file}" "{escaped_heading}" "{escaped_state}")'
            result = self._execute_elisp(elisp)
            
            return {"success": True, "message": result}
        except EmacsClientError as e:
            return {"success": False, "error": str(e)}


    def read_note(self, identifier: str, section: str = None) -> Dict[str, Any]:
        """Read full content of a note by ID or path.

        Args:
            identifier: Org-roam ID or path relative to org-roam-directory
            section: Optional heading name to return only that section

        Returns:
            Note content, title, properties, and metadata
        """
        if section:
            expression = f'(my/api-read-note "{self._escape_for_elisp(identifier)}" "{self._escape_for_elisp(section)}")'
        else:
            expression = f'(my/api-read-note "{self._escape_for_elisp(identifier)}")'
        return self.eval_elisp(expression)

    def update_note(self, identifier: str, content: str, section: str = None, mode: str = "append") -> Dict[str, Any]:
        """Update content in a note by ID or path.

        Args:
            identifier: Org-roam ID or path relative to org-roam-directory
            content: Content to add
            section: Optional heading name to target
            mode: "append" (default), "prepend", or "replace"

        Returns:
            Success status and preview of updated content
        """
        escaped_id = self._escape_for_elisp(identifier)
        escaped_content = self._escape_for_elisp(content)
        escaped_mode = self._escape_for_elisp(mode)
        
        if section:
            escaped_section = self._escape_for_elisp(section)
            expression = f'(my/api-update-note "{escaped_id}" "{escaped_content}" "{escaped_section}" "{escaped_mode}")'
        else:
            expression = f'(my/api-update-note "{escaped_id}" "{escaped_content}" nil "{escaped_mode}")'
        return self.eval_elisp(expression)

    def list_notes(self, node_type: str = None, status: str = None, limit: int = 50, sort_by: str = "modified") -> Dict[str, Any]:
        """List org-roam notes with optional filters.

        Args:
            node_type: "project", "person", "idea", "admin", "blog", or None for all
            status: "active", "stale", "done", "cancelled", or None for all
            limit: Maximum number of results (default 50)
            sort_by: "created", "modified", or "title" (default "modified")

        Returns:
            List of notes matching filters
        """
        type_arg = f'"{node_type}"' if node_type else 'nil'
        status_arg = f'"{status}"' if status else 'nil'
        sort_arg = f'"{sort_by}"' if sort_by else '"modified"'
        
        expression = f'(my/api-list-notes {type_arg} {status_arg} {limit} {sort_arg})'
        return self.eval_elisp(expression)

    def get_note_properties(self, identifier: str) -> Dict[str, Any]:
        """Get metadata for a note without full content."""
        expression = f'(my/api-get-note-properties "{self._escape_for_elisp(identifier)}")'
        return self.eval_elisp(expression)

    def delete_note(self, identifier: str, archive: bool = False) -> Dict[str, Any]:
        """Delete or archive a note."""
        archive_arg = "t" if archive else "nil"
        expression = f'(my/api-delete-note "{self._escape_for_elisp(identifier)}" {archive_arg})'
        return self.eval_elisp(expression)

    def rename_note(self, identifier: str, new_title: str) -> Dict[str, Any]:
        """Rename a note and update its title."""
        expression = f'(my/api-rename-note "{self._escape_for_elisp(identifier)}" "{self._escape_for_elisp(new_title)}")'
        return self.eval_elisp(expression)

    def manage_tags(self, identifier: str, action: str, tag: str) -> Dict[str, Any]:
        """Add or remove a tag from a note."""
        expression = f'(my/api-manage-tags "{self._escape_for_elisp(identifier)}" "{action}" "{self._escape_for_elisp(tag)}")'
        return self.eval_elisp(expression)

    def add_link(self, from_id: str, to_id: str, section: str = None) -> Dict[str, Any]:
        """Add an org-roam link between two notes."""
        if section:
            expression = f'(my/api-add-link "{self._escape_for_elisp(from_id)}" "{self._escape_for_elisp(to_id)}" "{self._escape_for_elisp(section)}")'
        else:
            expression = f'(my/api-add-link "{self._escape_for_elisp(from_id)}" "{self._escape_for_elisp(to_id)}")'
        return self.eval_elisp(expression)

"""Tests for emacs_client module."""

import pytest
from unittest.mock import Mock, patch
from org_roam_mcp.emacs_client import EmacsClient, EmacsClientError


@pytest.fixture
def client():
    """Create EmacsClient instance for testing."""
    return EmacsClient("/test/server/file")


class TestEmacsClient:
    """Test EmacsClient functionality."""
    
    def test_init(self):
        """Test client initialization."""
        client = EmacsClient("/custom/server")
        assert client.server_file == "/custom/server"
        assert client.timeout == 30
    
    def test_escape_for_elisp(self, client):
        """Test elisp string escaping."""
        # Basic escaping
        assert client._escape_for_elisp('hello') == 'hello'
        assert client._escape_for_elisp('hello "world"') == 'hello \\"world\\"'
        assert client._escape_for_elisp('line1\nline2') == 'line1\\nline2'
        assert client._escape_for_elisp('tab\there') == 'tab\\there'
        assert client._escape_for_elisp('back\\slash') == 'back\\\\slash'
        
        # Combined escaping
        test_str = 'This has "quotes", \nnewlines, and \\backslashes'
        expected = 'This has \\"quotes\\", \\nnewlines, and \\\\backslashes'
        assert client._escape_for_elisp(test_str) == expected
    
    def test_build_elisp_list(self, client):
        """Test elisp list construction."""
        # Empty list
        assert client._build_elisp_list([]) == '(list )'
        
        # Single item
        assert client._build_elisp_list(['item1']) == '(list "item1")'
        
        # Multiple items
        result = client._build_elisp_list(['item1', 'item2', 'item "3"'])
        expected = '(list "item1" "item2" "item \\"3\\"")'
        assert result == expected
    
    @patch('subprocess.run')
    def test_execute_command_success(self, mock_run, client):
        """Test successful command execution."""
        mock_run.return_value = Mock(returncode=0, stdout='{"success": true}', stderr='')
        
        result = client._execute_command('test command')
        assert result == '{"success": true}'
        mock_run.assert_called_once()
    
    @patch('subprocess.run')
    def test_execute_command_failure(self, mock_run, client):
        """Test command execution failure."""
        mock_run.return_value = Mock(returncode=1, stdout='', stderr='Error message')
        
        with pytest.raises(EmacsClientError) as exc_info:
            client._execute_command('test command')
        
        assert 'emacsclient failed' in str(exc_info.value)
        assert 'Error message' in str(exc_info.value)
    
    @patch('subprocess.run')
    def test_execute_command_timeout(self, mock_run, client):
        """Test command execution timeout."""
        import subprocess
        mock_run.side_effect = subprocess.TimeoutExpired('cmd', 30)
        
        with pytest.raises(EmacsClientError) as exc_info:
            client._execute_command('test command')
        
        assert 'timed out' in str(exc_info.value)
    
    def test_parse_json_response_simple(self, client):
        """Test parsing simple JSON response."""
        response = '{"success": true, "data": "test"}'
        result = client._parse_json_response(response)
        assert result == {"success": True, "data": "test"}
    
    def test_parse_json_response_character_array(self, client):
        """Test parsing character array response."""
        # Simulate character array response
        char_array = {"0": "{", "1": "\"", "2": "s", "3": "u", "4": "c", "5": "c", 
                     "6": "e", "7": "s", "8": "s", "9": "\"", "10": ":", "11": " ", 
                     "12": "t", "13": "r", "14": "u", "15": "e", "16": "}"}
        
        with patch('json.loads') as mock_loads:
            # First call returns character array, second call returns actual JSON
            mock_loads.side_effect = [char_array, {"success": True}]
            
            result = client._parse_json_response('dummy')
            assert result == {"success": True}
    
    def test_parse_json_response_invalid(self, client):
        """Test parsing invalid JSON response."""
        with pytest.raises(EmacsClientError) as exc_info:
            client._parse_json_response('invalid json')

        assert 'Failed to parse JSON' in str(exc_info.value)

    def test_parse_json_response_with_control_characters(self, client):
        """Test parsing JSON with literal newlines and control characters.

        This simulates the issue where Emacs json-encode outputs strings
        with literal newlines instead of escaped \\n sequences.
        """
        # JSON with literal newlines in string value (invalid JSON)
        response_with_literal_newlines = '''{"success": true, "content": "line1
line2
line3", "data": "test"}'''

        result = client._parse_json_response(response_with_literal_newlines)
        assert result["success"] is True
        assert "line1\\nline2\\nline3" in result["content"] or "line1\nline2\nline3" in result["content"]
        assert result["data"] == "test"

    def test_parse_json_response_with_org_file_content(self, client):
        """Test parsing JSON with org file content containing properties and embeddings.

        This simulates the real-world case where semantic_search returns
        full_content with :PROPERTIES: blocks and literal newlines.
        """
        # Simulated response from semantic_search with org file content
        response_with_org_content = '''{"success": true, "notes": [{"id": "123", "full_content": ":PROPERTIES:
:ID: 123
:EMBEDDING: [0.1, 0.2, 0.3]
:END:
#+title: Test Note

Content here"}]}'''

        result = client._parse_json_response(response_with_org_content)
        assert result["success"] is True
        assert len(result["notes"]) == 1
        assert "PROPERTIES" in result["notes"][0]["full_content"]
        assert "EMBEDDING" in result["notes"][0]["full_content"]
        assert "Test Note" in result["notes"][0]["full_content"]
    
    @patch.object(EmacsClient, 'eval_elisp')
    def test_contextual_search(self, mock_eval, client):
        """Test contextual search functionality."""
        mock_eval.return_value = {"success": True, "notes": []}
        
        result = client.contextual_search("test query", 5)
        
        mock_eval.assert_called_once_with('(my/api-contextual-search "test query" 5)')
        assert result == {"success": True, "notes": []}
    
    @patch.object(EmacsClient, 'eval_elisp')
    def test_contextual_search_with_escaping(self, mock_eval, client):
        """Test contextual search with special characters."""
        mock_eval.return_value = {"success": True, "notes": []}
        
        client.contextual_search('query with "quotes"', 10)
        
        mock_eval.assert_called_once_with('(my/api-contextual-search "query with \\"quotes\\"" 10)')
    
    @patch.object(EmacsClient, 'eval_elisp')
    def test_create_note(self, mock_eval, client):
        """Test note creation."""
        mock_eval.return_value = {"success": True, "note": {"id": "123"}}
        
        result = client.create_note("Test Title", "Test Content", "reference", "high")
        
        expected_expr = ('(my/api-create-note "Test Title" "Test Content" '
                        '"reference" "high")')
        mock_eval.assert_called_once_with(expected_expr)
        assert result == {"success": True, "note": {"id": "123"}}
    
    @patch.object(EmacsClient, 'eval_elisp')
    def test_add_daily_entry_success(self, mock_eval, client):
        """Test successful daily entry addition."""
        mock_eval.return_value = None  # Function returns nil on success

        result = client.add_daily_entry(
            "14:30", "Test Entry", ["point1", "point2"],
            ["step1"], ["tag1"], "journal"
        )

        # Note: entry_type is NOT passed to elisp - it's used client-side for title formatting only
        expected_expr = ('(my/add-daily-entry-structured "14:30" "Test Entry" '
                        '(list "point1" "point2") (list "step1") (list "tag1"))')
        mock_eval.assert_called_once_with(expected_expr)
        assert result["success"] is True
    
    @patch.object(EmacsClient, 'eval_elisp')  
    def test_add_daily_entry_nil_response(self, mock_eval, client):
        """Test daily entry with nil response (success case)."""
        mock_eval.side_effect = EmacsClientError("nil")
        
        result = client.add_daily_entry("14:30", "Test", ["point1"])
        
        assert result["success"] is True
        assert "successfully" in result["message"]
    
    @patch.object(EmacsClient, '_execute_command')
    def test_get_daily_content_success(self, mock_execute, client):
        """Test successful daily content retrieval."""
        mock_execute.return_value = '"Daily note content\\nWith multiple lines"'
        
        result = client.get_daily_content("2024-01-01")
        
        assert result["success"] is True
        assert "Daily note content\nWith multiple lines" in result["content"]
        assert result["date"] == "2024-01-01"
    
    @patch.object(EmacsClient, '_execute_command')
    def test_get_daily_content_no_date(self, mock_execute, client):
        """Test daily content retrieval without date."""
        mock_execute.return_value = '"Today\'s content"'
        
        result = client.get_daily_content()
        
        assert result["success"] is True
        assert result["date"] == "today"
        
        # Verify correct elisp expression was used
        call_args = mock_execute.call_args[0][0]
        assert '(my/get-daily-note-content)' in call_args
    
    @patch.object(EmacsClient, 'eval_elisp')
    def test_sync_database_success(self, mock_eval, client):
        """Test successful database sync."""
        mock_eval.return_value = None
        
        result = client.sync_database(force=True)
        
        mock_eval.assert_called_once_with("(org-roam-db-sync \\'force)")
        assert result["success"] is True
    
    @patch.object(EmacsClient, 'eval_elisp')
    def test_sync_database_nil_response(self, mock_eval, client):
        """Test database sync with nil response (success case).""" 
        mock_eval.side_effect = EmacsClientError("nil")
        
        result = client.sync_database()
        
        assert result["success"] is True
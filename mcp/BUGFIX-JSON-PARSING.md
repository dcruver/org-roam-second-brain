# JSON Parsing Error Fix - Control Characters in Emacs Response

## Problem Summary

**Error**: `Invalid control character at: line 1 column 28742 (char 28741)`

**Root Cause**: Emacs `json-encode` function outputs JSON strings containing literal newline characters (`\n`) and other control characters instead of properly escaped sequences (`\\n`). This occurs when `semantic_search` returns `full_content` fields containing org-roam file content with `:PROPERTIES:` blocks, embeddings, and multi-line text.

Python's `json.loads()` strictly requires all control characters in JSON strings to be escaped according to RFC 8259, but Emacs `json-encode` sometimes outputs literal control characters in string values.

## Solution

Enhanced `EmacsClient._parse_json_response()` method in `/home/dcruver/Projects/org-roam-ai/mcp/src/org_roam_mcp/emacs_client.py` to automatically fix control characters when initial JSON parsing fails.

### Implementation Details

The fix uses a two-stage approach:

1. **First attempt**: Standard `json.loads()` (fast path for properly-formatted JSON)
2. **Fallback**: If parsing fails with control character error:
   - Use regex to identify all JSON string values: `"((?:[^"\\]|\\.)*)"`
   - For each string value, escape unescaped control characters:
     - `\n` (newline) → `\\n`
     - `\r` (carriage return) → `\\r`
     - `\t` (tab) → `\\t`
     - `\f` (form feed) → `\\f`
     - `\x08` (backspace) → `\\b`
   - Parse the cleaned JSON

### Code Changes

**File**: `mcp/src/org_roam_mcp/emacs_client.py`

**Modified method**: `_parse_json_response()` (lines 127-199)

Key implementation:
```python
def fix_string_content(match):
    """Fix control characters within a JSON string value."""
    content = match.group(1)
    fixed = content
    fixed = re.sub(r'(?<!\\)\n', r'\\n', fixed)  # newline
    fixed = re.sub(r'(?<!\\)\r', r'\\r', fixed)  # carriage return
    fixed = re.sub(r'(?<!\\)\t', r'\\t', fixed)  # tab
    fixed = re.sub(r'(?<!\\)\f', r'\\f', fixed)  # form feed
    fixed = re.sub(r'(?<!\\)[\x08]', r'\\b', fixed)  # backspace
    return f'"{fixed}"'

cleaned = re.sub(r'"((?:[^"\\]|\\.)*)"', fix_string_content, response, flags=re.DOTALL)
parsed = json.loads(cleaned)
```

**Important**: The regex `(?<!\\)\n` uses negative lookbehind to only match unescaped newlines, preserving already-escaped sequences.

## Testing

Added two comprehensive test cases in `tests/test_emacs_client.py`:

1. **`test_parse_json_response_with_control_characters`**: Tests basic newline handling
2. **`test_parse_json_response_with_org_file_content`**: Tests real-world scenario with org file content containing `:PROPERTIES:`, `:EMBEDDING:`, and multi-line content

All 20 existing tests continue to pass, confirming no regressions.

## Verification Steps

To verify the fix works in production:

```bash
# 1. Start MCP server
cd /home/dcruver/Projects/org-roam-ai/mcp
source .venv/bin/activate
python -m org_roam_mcp.server

# 2. In another terminal, test semantic_search
curl -X POST http://localhost:8000 \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "method": "tools/call",
    "params": {
      "name": "semantic_search",
      "arguments": {
        "query": "test query",
        "limit": 5,
        "similarity_cutoff": 0.7
      }
    },
    "id": 1
  }'
```

Expected: JSON response with `full_content` fields containing org file content, no parsing errors.

## Performance Impact

- **Minimal**: Fast path (standard `json.loads()`) is unchanged
- **Fallback overhead**: Regex processing only occurs when control characters are present
- **Typical case**: No performance impact for properly-formatted JSON

## Alternative Approaches Considered

1. **Fix in Elisp**: Modify `org-roam-api.el` to use `json-encode-string` directly
   - **Rejected**: Would require changes to external dependency (org-roam-semantic)
   - Python fix is more maintainable and doesn't require Emacs changes

2. **Use lenient JSON parser**: Libraries like `demjson` or `hjson`
   - **Rejected**: Adds external dependency, not needed for this specific issue

3. **Character-by-character escaping**: Process entire response string
   - **Rejected**: Would escape characters outside JSON strings (breaks structure)

## Related Files

- **Implementation**: `/home/dcruver/Projects/org-roam-ai/mcp/src/org_roam_mcp/emacs_client.py`
- **Tests**: `/home/dcruver/Projects/org-roam-ai/mcp/tests/test_emacs_client.py`
- **Elisp API**: `/home/dcruver/Projects/org-roam-ai/mcp/org-roam-api.el` (unchanged)

## Additional Fix

Fixed incorrect test expectation in `test_add_daily_entry_success`:
- Removed `"journal"` parameter from expected elisp expression
- `entry_type` is used client-side only for title formatting (per CLAUDE.md documentation)
- Elisp function `my/add-daily-entry-structured` takes 5 parameters, not 6

## Additional Fix - Version 0.1.6 (Client-Side JSON Encoding)

### Problem
After fixing the Emacs → Server JSON parsing, a **second issue** was discovered: The server successfully parses Emacs responses but then fails to properly encode them when sending back to clients via HTTP JSON-RPC.

**Symptom**: Client receives "Invalid control character at line 1 column 27689" error even though server logs show successful parsing (return code 0).

**Root Cause**: The fix in `_parse_json_response()` handles parsing FROM Emacs correctly, but the HTTP handler in `server.py` uses Starlette's `JSONResponse` which doesn't properly escape control characters when serializing nested strings back to the client.

### Solution (Version 0.1.6)

Changed HTTP response handlers in `server.py` to use explicit `json.dumps()` with proper escaping instead of relying on `JSONResponse` automatic serialization.

**Modified handlers**:
1. `semantic_search` (lines 799-827)
2. `contextual_search` (lines 787-806)
3. `search_notes` (lines 778-793)
4. `get_daily_content` (lines 896-913)

**Key change pattern**:
```python
# OLD (unreliable escaping)
return JSONResponse({
    "jsonrpc": "2.0",
    "id": rpc_request.get("id"),
    "result": result  # result contains control chars in nested strings
})

# NEW (explicit escaping)
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
```

**Why this works**:
- Python's `json.dumps()` properly escapes ALL control characters according to RFC 8259
- `ensure_ascii=False` preserves UTF-8 characters while still escaping control chars
- Eliminates reliance on Starlette's `JSONResponse` serialization behavior

### Testing Version 0.1.6

```bash
# 1. Rebuild and install
cd /home/dcruver/Projects/org-roam-ai/mcp
python -m build
pip install dist/org_roam_mcp-0.1.6-py3-none-any.whl

# 2. Start server
python -m org_roam_mcp.server

# 3. Test semantic_search (should now work)
curl -X POST http://localhost:8000 \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "method": "tools/call",
    "params": {
      "name": "semantic_search",
      "arguments": {
        "query": "container networking",
        "limit": 10,
        "cutoff": 0.7
      }
    },
    "id": 1
  }' | python -m json.tool
```

Expected: Valid JSON response with no parsing errors from client side.

## Next Steps

1. Deploy updated MCP server version 0.1.6 to production (192.168.20.136 and n8n-backend.cruver.network)
2. Test with real Codex/n8n client requests to verify client-side parsing succeeds
3. Monitor logs for "Successfully parsed after fixing control characters" messages
4. Verify semantic_search operations complete without errors on both server and client sides
5. Consider adding telemetry to track fallback usage frequency

## References

- **RFC 8259** (JSON specification): Control characters (U+0000 to U+001F) must be escaped
- **Emacs json.el**: `json-encode` documentation
- **Python json module**: Standard library documentation

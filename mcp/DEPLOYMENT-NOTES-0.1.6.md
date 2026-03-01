# Deployment Notes - Version 0.1.6

## Summary

Version 0.1.6 fixes a critical JSON encoding issue that prevented clients from parsing semantic_search responses. The server was successfully parsing Emacs responses but failing to properly escape control characters when sending responses back to HTTP clients.

## What Was Fixed

### Problem
- **Server-side**: Parser worked correctly (fixed in 0.1.5)
- **Client-side**: JSONResponse didn't properly escape control characters in nested strings
- **Symptom**: Client received "Invalid control character at line 1 column 27689"

### Root Cause
The HTTP response handlers used Starlette's `JSONResponse` which doesn't guarantee proper JSON escaping of control characters in deeply nested strings (like `full_content` fields containing org file content with newlines, tabs, etc.).

### Solution
Changed HTTP response handlers to use explicit `json.dumps()` with proper escaping:

**Modified handlers in `/home/dcruver/Projects/org-roam-ai/mcp/src/org_roam_mcp/server.py`**:
1. `semantic_search` (lines 799-827)
2. `contextual_search` (lines 787-806)
3. `search_notes` (lines 778-793)
4. `get_daily_content` (lines 896-913)

**Pattern applied**:
```python
# Instead of: JSONResponse({"result": result})
# Now using:
response_data = {"jsonrpc": "2.0", "id": id, "result": result}
json_content = json.dumps(response_data, ensure_ascii=False)
return Response(content=json_content, media_type="application/json")
```

## Testing Results

All 20 unit tests pass:
```bash
$ pytest -v
============================== 20 passed in 0.02s ===============================
```

## Deployment Instructions

### For org-roam-agent-backend (192.168.20.136)

```bash
# 1. Copy wheel file to server
scp /home/dcruver/Projects/org-roam-ai/mcp/dist/org_roam_mcp-0.1.6-py3-none-any.whl \
    root@192.168.20.136:/tmp/

# 2. Install on server
ssh root@192.168.20.136
systemctl stop org-roam-mcp
/opt/org-roam-mcp-venv/bin/pip install --upgrade /tmp/org_roam_mcp-0.1.6-py3-none-any.whl
systemctl start org-roam-mcp

# 3. Verify
systemctl status org-roam-mcp
journalctl -u org-roam-mcp -f
```

### For n8n-backend.cruver.network

```bash
# 1. Copy wheel file to server
scp /home/dcruver/Projects/org-roam-ai/mcp/dist/org_roam_mcp-0.1.6-py3-none-any.whl \
    root@n8n-backend.cruver.network:/tmp/

# 2. Install on server
ssh root@n8n-backend.cruver.network
systemctl stop org-roam-mcp
/opt/org-roam-mcp-venv/bin/pip install --upgrade /tmp/org_roam_mcp-0.1.6-py3-none-any.whl
systemctl start org-roam-mcp

# 3. Verify
systemctl status org-roam-mcp
journalctl -u org-roam-mcp -f
```

## Verification Tests

### Test 1: Basic Health Check
```bash
curl http://localhost:8000
# Expected: OK
```

### Test 2: Semantic Search (Critical Test)
```bash
curl -X POST http://localhost:8000 \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "method": "tools/call",
    "params": {
      "name": "semantic_search",
      "arguments": {
        "query": "container networking",
        "limit": 5,
        "cutoff": 0.7
      }
    },
    "id": 1
  }' | python -m json.tool
```

**Expected**: Valid JSON response with no parsing errors. Response should contain:
- `"jsonrpc": "2.0"`
- `"result"` object with `"success": true`
- `"notes"` array with semantic search results
- `full_content` fields properly escaped

### Test 3: Contextual Search
```bash
curl -X POST http://localhost:8000 \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "method": "tools/call",
    "params": {
      "name": "contextual_search",
      "arguments": {
        "query": "kubernetes",
        "limit": 5
      }
    },
    "id": 1
  }' | python -m json.tool
```

**Expected**: Valid JSON response with contextual search results.

## Rollback Procedure (if needed)

```bash
# Rollback to version 0.1.5
ssh root@SERVER_HOST
systemctl stop org-roam-mcp
/opt/org-roam-mcp-venv/bin/pip install org-roam-mcp==0.1.5
systemctl start org-roam-mcp
```

## What to Monitor

1. **Client-side errors**: Check Codex/n8n logs for JSON parsing errors
2. **Server logs**: `journalctl -u org-roam-mcp -f`
   - Look for successful requests (return code 200)
   - No Python exceptions
3. **Performance**: Response times should be unchanged (< 2 seconds for semantic search)

## Files Changed

1. `/home/dcruver/Projects/org-roam-ai/mcp/src/org_roam_mcp/server.py`
   - Modified 4 HTTP response handlers
2. `/home/dcruver/Projects/org-roam-ai/mcp/pyproject.toml`
   - Bumped version to 0.1.6
3. `/home/dcruver/Projects/org-roam-ai/mcp/BUGFIX-JSON-PARSING.md`
   - Documented the client-side fix

## Success Criteria

- [ ] MCP server starts without errors on both production servers
- [ ] `curl http://localhost:8000` returns "OK"
- [ ] Semantic search curl test returns valid JSON (no parsing errors)
- [ ] Codex can successfully call semantic_search via MCP
- [ ] No errors in server logs after 24 hours

## Timeline

- **Development**: 2025-10-31
- **Testing**: 2025-10-31 (all 20 tests passed)
- **Built**: 2025-10-31 (version 0.1.6)
- **Ready for deployment**: 2025-10-31

## Contact

For issues or questions, check:
- Documentation: `/home/dcruver/Projects/org-roam-ai/mcp/BUGFIX-JSON-PARSING.md`
- Server logs: `journalctl -u org-roam-mcp -f`
- Project CLAUDE.md: `/home/dcruver/Projects/org-roam-ai/mcp/CLAUDE.md`

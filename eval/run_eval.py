#!/usr/bin/env python3
"""Can a small local model keep an org-roam second brain?

For each chore in chores.py this script:

  1. copies the sample vault (eval/vault) to a scratch directory;
  2. starts Emacs in batch mode running the MCP server on that copy
     (eval/serve.el), plus a stand-in embeddings server unless you give
     --embed-url;
  3. gives the chore to a chat model through any OpenAI-compatible
     /chat/completions endpoint (llama.cpp's server, vLLM, Ollama, LM Studio,
     a hosted API ...), offering the MCP server's tools as functions, and
     carries out each tool call the model makes against the MCP server;
  4. stops Emacs and scores the vault files left on disk against the chore's
     expected end state (vaultcheck.py), and the model's final reply where
     the chore is a question.

It records, per chore: pass or fail, the share of checks passed, tool calls,
tool errors (the server answered ok:false), bad calls (unknown tool or
arguments that were not valid JSON), turns, time and tokens.  Results go to
--out as results.json, a results.md table, and one transcript per run.

Standard library only (Python 3.8+).  Examples:

  # a local llama.cpp or vLLM server
  python3 eval/run_eval.py --base-url http://feynman:8080/v1 --model qwen3.8-27b

  # check the harness itself, no model: known good tool calls, then nothing
  python3 eval/run_eval.py --scripted
  python3 eval/run_eval.py --null-model

See eval/README.md.
"""

import argparse
import datetime
import json
import os
import re
import shutil
import socket
import subprocess
import sys
import tempfile
import threading
import time
import urllib.error
import urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)

import vaultcheck  # noqa: E402
from chores import chores as load_chores  # noqa: E402

DEFAULT_SYSTEM = """You keep the user's personal notes (an org-roam second brain) using the tools you are given.
Today is {today}.
Do what the user asks with the tools, then reply in a sentence or two saying what you did or answering the question.
Never invent facts about the notes; look them up."""

# Local services must not go through an HTTP proxy from the environment.
NO_PROXY = urllib.request.build_opener(urllib.request.ProxyHandler({}))


def free_port():
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def post_json(url, payload, headers=None, timeout=600, opener=None):
    data = json.dumps(payload).encode()
    req = urllib.request.Request(url, data=data, method="POST")
    req.add_header("Content-Type", "application/json")
    req.add_header("Accept", "application/json")
    for k, v in (headers or {}).items():
        req.add_header(k, v)
    with (opener or NO_PROXY).open(req, timeout=timeout) as r:
        return json.loads(r.read().decode() or "{}"), dict(r.headers)


# --- the MCP side --------------------------------------------------------------

class McpServer:
    """One Emacs process serving the MCP tools on one vault copy."""

    def __init__(self, args, vault, state, port, embed_url):
        self.url = f"http://127.0.0.1:{port}/mcp"
        env = dict(os.environ,
                   ORSB_EVAL_VAULT=vault, ORSB_EVAL_STATE=state, ORSB_EVAL_PORT=str(port),
                   ORSB_EVAL_EMBED_URL=embed_url or "", ORSB_EVAL_EMBED_MODEL=args.embed_model,
                   ORSB_EVAL_EMBED_DIMS=str(args.embed_dims), ORSB_EVAL_PROFILE=args.profile)
        if args.deps is not None:
            env["ORSB_DEPS"] = args.deps
        self.log_path = os.path.join(state, "emacs.log")
        os.makedirs(state, exist_ok=True)
        self.log = open(self.log_path, "w")
        self.proc = subprocess.Popen([args.emacs, "-Q", "--batch", "-l", os.path.join(HERE, "serve.el")],
                                     env=env, stdout=self.log, stderr=subprocess.STDOUT)
        self.session = None
        self.next_id = 1
        deadline = time.time() + args.server_timeout
        while time.time() < deadline:
            if self.proc.poll() is not None:
                raise RuntimeError(f"Emacs exited early; see {self.log_path}:\n" + self.tail())
            with open(self.log_path) as f:
                if "ORSB-EVAL READY" in f.read():
                    break
            time.sleep(0.2)
        else:
            self.stop()
            raise RuntimeError(f"MCP server not ready after {args.server_timeout}s; see {self.log_path}")
        result = self.rpc("initialize", {"protocolVersion": "2025-03-26", "capabilities": {},
                                         "clientInfo": {"name": "orsb-eval", "version": "1"}})
        self.instructions = result.get("instructions") or ""
        self.tools = self.rpc("tools/list", {}).get("tools", [])

    def tail(self, n=30):
        with open(self.log_path) as f:
            return "".join(f.readlines()[-n:])

    def rpc(self, method, params):
        payload = {"jsonrpc": "2.0", "id": self.next_id, "method": method, "params": params}
        self.next_id += 1
        headers = {"Mcp-Session-Id": self.session} if self.session else {}
        body, hdrs = post_json(self.url, payload, headers, timeout=120)
        for k, v in hdrs.items():
            if k.lower() == "mcp-session-id":
                self.session = v
        if "error" in body:
            raise RuntimeError(f"MCP {method} error: {body['error']}")
        return body.get("result", {})

    def call(self, name, arguments):
        """Call a tool; return (text for the model, parsed envelope or None, ok)."""
        result = self.rpc("tools/call", {"name": name, "arguments": arguments})
        text = "".join(c.get("text", "") for c in result.get("content", []) if c.get("type") == "text")
        try:
            env = json.loads(text)
        except ValueError:
            env = None
        ok = not result.get("isError") and (env is None or env.get("ok", True))
        return text, env, ok

    def stop(self):
        if self.proc.poll() is None:
            self.proc.terminate()
            try:
                self.proc.wait(10)
            except subprocess.TimeoutExpired:
                self.proc.kill()
        self.log.close()


def openai_tools(mcp_tools):
    out = []
    for t in mcp_tools:
        out.append({"type": "function", "function": {
            "name": t["name"],
            "description": t.get("description", ""),
            "parameters": t.get("inputSchema") or {"type": "object", "properties": {}},
        }})
    return out


# --- the model side -------------------------------------------------------------

TEXT_CALL_RE = re.compile(r"<tool_call>\s*(\{.*?\})\s*</tool_call>", re.S)
THINK_RE = re.compile(r"<think>.*?</think>", re.S)


class Stats:
    def __init__(self):
        self.tool_calls = 0
        self.tool_errors = 0
        self.bad_calls = 0
        self.text_calls = 0
        self.turns = 0
        self.prompt_tokens = 0
        self.completion_tokens = 0
        self.ran_out = False
        self.model_error = None
        self.tools_used = {}


class Transcript:
    def __init__(self, path):
        self.f = open(path, "w")

    def write(self, kind, **data):
        self.f.write(json.dumps({"t": round(time.time(), 3), "kind": kind, **data}) + "\n")
        self.f.flush()

    def close(self):
        self.f.close()


def run_tool(mcp, name, args_text, known, stats, log):
    """Carry out one tool call from the model; return the text the model sees."""
    stats.tool_calls += 1
    stats.tools_used[name] = stats.tools_used.get(name, 0) + 1
    if name not in known:
        stats.bad_calls += 1
        msg = f"Unknown tool {name!r}. Available tools: {', '.join(sorted(known))}."
        log.write("bad_call", name=name, reason="unknown tool")
        return msg
    if isinstance(args_text, dict):
        args = args_text
    else:
        try:
            args = json.loads(args_text or "{}")
            if not isinstance(args, dict):
                raise ValueError("arguments are not an object")
        except ValueError as e:
            stats.bad_calls += 1
            log.write("bad_call", name=name, reason=f"bad JSON arguments: {e}", arguments=args_text)
            return f"Your arguments for {name} were not a valid JSON object ({e}). Call it again with valid JSON."
    try:
        text, env, ok = mcp.call(name, args)
    except Exception as e:  # server-level failure, counts as an error
        stats.tool_errors += 1
        log.write("tool_result", name=name, arguments=args, ok=False, text=str(e))
        return f"Tool call failed: {e}"
    if not ok:
        stats.tool_errors += 1
    log.write("tool_result", name=name, arguments=args, ok=ok, text=text)
    return text


def chat_agent(args, mcp, chore, today, stats, log):
    """Run the model on one chore; return its final reply."""
    system = (open(args.system_prompt).read() if args.system_prompt else DEFAULT_SYSTEM).format(today=today)
    if mcp.instructions and not args.no_server_instructions:
        system += "\n\nAbout the tools:\n" + mcp.instructions
    messages = [{"role": "system", "content": system}, {"role": "user", "content": chore["prompt"]}]
    tools = openai_tools(mcp.tools)
    known = {t["function"]["name"] for t in tools}
    headers = {"Authorization": f"Bearer {args.api_key}"} if args.api_key else {}
    opener = urllib.request.build_opener() if args.use_proxy else NO_PROXY
    url = args.base_url.rstrip("/") + "/chat/completions"
    final = ""
    for _turn in range(args.max_turns):
        stats.turns += 1
        payload = {"model": args.model, "messages": messages, "tools": tools, "tool_choice": "auto",
                   "temperature": args.temperature}
        if args.max_tokens:
            payload["max_tokens"] = args.max_tokens
        if args.extra_body:
            payload.update(json.loads(args.extra_body))
        try:
            resp, _ = post_json(url, payload, headers, timeout=args.request_timeout, opener=opener)
        except urllib.error.HTTPError as e:
            stats.model_error = f"HTTP {e.code}: {e.read()[:500].decode(errors='replace')}"
            log.write("model_error", error=stats.model_error)
            break
        except Exception as e:
            stats.model_error = str(e)
            log.write("model_error", error=stats.model_error)
            break
        usage = resp.get("usage") or {}
        stats.prompt_tokens += usage.get("prompt_tokens") or 0
        stats.completion_tokens += usage.get("completion_tokens") or 0
        msg = (resp.get("choices") or [{}])[0].get("message") or {}
        content = msg.get("content") or ""
        calls = msg.get("tool_calls") or []
        log.write("assistant", content=content, tool_calls=calls,
                  reasoning=msg.get("reasoning_content") or msg.get("reasoning"))
        if calls:
            messages.append({"role": "assistant", "content": content or None, "tool_calls": calls})
            for c in calls:
                fn = c.get("function") or {}
                result = run_tool(mcp, fn.get("name", ""), fn.get("arguments"), known, stats, log)
                messages.append({"role": "tool", "tool_call_id": c.get("id", ""), "content": result[:args.max_result_chars]})
            continue
        # Some servers leave tool calls in the text (<tool_call>{...}</tool_call>).
        text_calls = TEXT_CALL_RE.findall(THINK_RE.sub("", content))
        if text_calls and not args.no_text_calls:
            messages.append({"role": "assistant", "content": content})
            results = []
            for raw in text_calls:
                stats.text_calls += 1
                try:
                    obj = json.loads(raw)
                    name, a = obj.get("name", ""), obj.get("arguments", {})
                except ValueError:
                    name, a = "", raw
                result = run_tool(mcp, name, a if isinstance(a, (dict, str)) else {}, known, stats, log)
                results.append(f"Result of {name}:\n{result[:args.max_result_chars]}")
            messages.append({"role": "user", "content": "\n\n".join(results)})
            continue
        final = THINK_RE.sub("", content).strip()
        break
    else:
        stats.ran_out = True
    return final


def substitute(value, setup_results):
    """Replace "$setup.N.key" strings with values from the setup calls' results."""
    if isinstance(value, str) and value.startswith("$setup."):
        _, n, key = value.split(".", 2)
        return setup_results[int(n)].get("data", {}).get(key)
    if isinstance(value, dict):
        return {k: substitute(v, setup_results) for k, v in value.items()}
    if isinstance(value, list):
        return [substitute(v, setup_results) for v in value]
    return value


def scripted_agent(mcp, chore, setup_results, stats, log, null=False):
    known = {t["name"] for t in mcp.tools}
    if null:
        log.write("assistant", content="(null model: no tool calls)")
        return "I did nothing."
    for step in chore.get("reference", []):
        stats.turns += 1
        run_tool(mcp, step["tool"], substitute(step["args"], setup_results), known, stats, log)
    final = chore.get("reference_answer", "")
    log.write("assistant", content=final)
    return final


# --- one chore -----------------------------------------------------------------

def run_chore(args, chore, rep, today, embed_url, out_dir):
    work = tempfile.mkdtemp(prefix=f"orsb-eval-{chore['name']}-")
    vault = os.path.join(work, "vault")
    state = os.path.join(work, "state")
    # plain copies, so every note's modified time is now (staleness is time based)
    shutil.copytree(os.path.join(HERE, "vault"), vault, copy_function=shutil.copyfile)
    tag = f"{chore['name']}" + (f"-{rep + 1}" if args.repeat > 1 else "")
    log = Transcript(os.path.join(out_dir, "transcripts", tag + ".jsonl"))
    stats = Stats()
    record = {"chore": chore["name"], "rep": rep + 1, "prompt": chore["prompt"]}
    started = time.time()
    mcp = None
    try:
        mcp = McpServer(args, vault, state, free_port(), embed_url)
        log.write("start", prompt=chore["prompt"], tools=[t["name"] for t in mcp.tools])
        setup_results = []
        for step in chore.get("setup", []):
            _text, env, ok = mcp.call(step["tool"], step["args"])
            if not ok:
                raise RuntimeError(f"setup call {step['tool']} failed: {_text}")
            setup_results.append(env or {})
            log.write("setup", name=step["tool"], arguments=step["args"], result=env)
        before = vaultcheck.snapshot(vault)
        agent_started = time.time()
        if args.scripted or args.null_model:
            final = scripted_agent(mcp, chore, setup_results, stats, log, null=args.null_model)
        else:
            final = chat_agent(args, mcp, chore, today, stats, log)
        record["agent_seconds"] = round(time.time() - agent_started, 1)
    except Exception as e:
        record["harness_error"] = str(e)
        final, before = "", None
        log.write("harness_error", error=str(e))
    finally:
        if mcp:
            mcp.stop()
    # score what is on disk
    checks = []
    if "harness_error" not in record:
        vault_now = vaultcheck.Vault(vault)
        for c in chore["checks"]:
            ok, detail = vault_now.check(c, final)
            checks.append({"check": c["type"], "ok": ok, "detail": detail})
        bad = vaultcheck.collateral(before, vaultcheck.snapshot(vault), chore.get("may_touch", []))
        checks.append({"check": "no_collateral", "ok": not bad, "detail": "; ".join(bad) or "nothing else changed"})
    passed = sum(c["ok"] for c in checks)
    record.update({
        "pass": bool(checks) and passed == len(checks),
        "score": round(passed / len(checks), 3) if checks else 0.0,
        "checks": checks,
        "final_reply": final,
        "tool_calls": stats.tool_calls, "tool_errors": stats.tool_errors, "bad_calls": stats.bad_calls,
        "text_tool_calls": stats.text_calls, "turns": stats.turns, "ran_out_of_turns": stats.ran_out,
        "model_error": stats.model_error, "tools_used": stats.tools_used,
        "prompt_tokens": stats.prompt_tokens, "completion_tokens": stats.completion_tokens,
        "seconds": round(time.time() - started, 1),
    })
    log.write("score", **{k: record[k] for k in ("pass", "score", "checks")})
    log.close()
    if args.keep_vaults:
        record["vault"] = vault
    else:
        shutil.rmtree(work, ignore_errors=True)
    return record


# --- reporting -----------------------------------------------------------------

def summarize(records):
    n = len(records)
    return {
        "runs": n,
        "passed": sum(r["pass"] for r in records),
        "mean_score": round(sum(r["score"] for r in records) / n, 3) if n else 0,
        "tool_calls": sum(r.get("tool_calls", 0) for r in records),
        "tool_errors": sum(r.get("tool_errors", 0) for r in records),
        "bad_calls": sum(r.get("bad_calls", 0) for r in records),
        "harness_errors": sum("harness_error" in r for r in records),
        "model_errors": sum(bool(r.get("model_error")) for r in records),
        "ran_out_of_turns": sum(bool(r.get("ran_out_of_turns")) for r in records),
        "prompt_tokens": sum(r.get("prompt_tokens", 0) for r in records),
        "completion_tokens": sum(r.get("completion_tokens", 0) for r in records),
        "seconds": round(sum(r.get("seconds", 0) for r in records), 1),
    }


def markdown(meta, summary, records):
    lines = [f"# Second brain chores: {meta['model']}", "",
             f"Run {meta['started']} against {meta['endpoint']}; tool profile {meta['profile']}; "
             f"embeddings: {meta['embeddings']}.", "",
             f"**{summary['passed']} of {summary['runs']} runs passed**, mean check score {summary['mean_score']:.2f}. "
             f"{summary['tool_calls']} tool calls, {summary['tool_errors']} tool errors, "
             f"{summary['bad_calls']} bad calls, {summary['ran_out_of_turns']} runs out of turns, "
             f"{summary['model_errors']} model errors, {summary['harness_errors']} harness errors.", "",
             "| Chore | Run | Pass | Checks | Tool calls | Errors | Bad calls | Turns | Seconds | Failed checks |",
             "|---|---|---|---|---|---|---|---|---|---|"]
    for r in records:
        failed = "; ".join(f"{c['check']}: {c['detail']}" for c in r.get("checks", []) if not c["ok"])
        if r.get("harness_error"):
            failed = "harness error: " + r["harness_error"]
        elif r.get("model_error"):
            failed = ("model error: " + r["model_error"][:120] + "; " + failed).strip("; ")
        elif r.get("ran_out_of_turns"):
            failed = ("ran out of turns; " + failed).strip("; ")
        ok_checks = sum(c["ok"] for c in r.get("checks", []))
        lines.append(f"| {r['chore']} | {r['rep']} | {'yes' if r['pass'] else 'no'} | "
                     f"{ok_checks}/{len(r.get('checks', []))} | {r.get('tool_calls', 0)} | {r.get('tool_errors', 0)} | "
                     f"{r.get('bad_calls', 0)} | {r.get('turns', 0)} | {r.get('seconds', 0)} | "
                     f"{failed.replace('|', '/')} |")
    return "\n".join(lines) + "\n"


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    m = p.add_argument_group("model")
    m.add_argument("--base-url", default=os.environ.get("OPENAI_BASE_URL", "http://127.0.0.1:8080/v1"),
                   help="OpenAI-compatible base URL, ending in /v1 (default %(default)s)")
    m.add_argument("--model", default=os.environ.get("ORSB_EVAL_MODEL", "local"), help="model name to send")
    m.add_argument("--api-key", default=os.environ.get("OPENAI_API_KEY", ""), help="sent as a bearer token if set")
    m.add_argument("--temperature", type=float, default=0.2)
    m.add_argument("--max-tokens", type=int, default=0, help="per response; 0 leaves it to the server")
    m.add_argument("--max-turns", type=int, default=15, help="model responses allowed per chore")
    m.add_argument("--request-timeout", type=int, default=600, help="seconds per model response")
    m.add_argument("--max-result-chars", type=int, default=12000, help="tool results are cut to this for the model")
    m.add_argument("--system-prompt", help="file with your own system prompt ({today} is filled in)")
    m.add_argument("--no-server-instructions", action="store_true",
                   help="leave out the guidance the MCP server sends at initialize")
    m.add_argument("--extra-body", help='JSON merged into each request, e.g. \'{"top_p": 0.9}\'')
    m.add_argument("--no-text-calls", action="store_true",
                   help="do not run <tool_call>{...}</tool_call> found in plain text")
    m.add_argument("--use-proxy", action="store_true", help="send model requests through the environment's HTTP proxy")
    m.add_argument("--scripted", action="store_true", help="no model: run each chore's known good tool calls")
    m.add_argument("--null-model", action="store_true", help="no model: do nothing (every chore should fail)")
    r = p.add_argument_group("run")
    r.add_argument("--chores", help="comma-separated chore names (default all)")
    r.add_argument("--list", action="store_true", help="list the chores and exit")
    r.add_argument("--repeat", type=int, default=1, help="runs per chore (models vary run to run)")
    r.add_argument("--out", default=None, help="results directory (default eval/results/<model>-<time>)")
    r.add_argument("--keep-vaults", action="store_true", help="keep each run's vault copy for inspection")
    r.add_argument("--expect", choices=["pass", "fail"],
                   help="exit 1 unless every run passed (or every run failed); for self-checks")
    s = p.add_argument_group("server")
    s.add_argument("--emacs", default=os.environ.get("EMACS", "emacs"))
    s.add_argument("--deps", default=None, help="like ORSB_DEPS for make test: where org-roam is installed")
    s.add_argument("--profile", default="small", choices=["small", "full"], help="MCP tool profile")
    s.add_argument("--embed-url", help="your OpenAI-compatible embeddings base URL; default: start the stand-in")
    s.add_argument("--embed-model", default="stand-in")
    s.add_argument("--embed-dims", type=int, default=768)
    s.add_argument("--no-embeddings", action="store_true", help="no semantic search at all; skips chores that need it")
    s.add_argument("--server-timeout", type=int, default=180, help="seconds to wait for Emacs to start")
    args = p.parse_args()

    today = datetime.date.today().isoformat()
    all_chores = load_chores(today)
    if args.list:
        for c in all_chores:
            print(f"{c['name']:20} {c['prompt']}")
        return 0
    chosen = all_chores
    if args.chores:
        names = [n.strip() for n in args.chores.split(",")]
        unknown = set(names) - {c["name"] for c in all_chores}
        if unknown:
            p.error("unknown chores: " + ", ".join(sorted(unknown)))
        chosen = [c for c in all_chores if c["name"] in names]
    if args.no_embeddings:
        chosen = [c for c in chosen if c.get("needs") != "embeddings"]

    label = "scripted" if args.scripted else "null-model" if args.null_model else args.model
    stamp = datetime.datetime.now().strftime("%Y%m%d-%H%M%S")
    out_dir = args.out or os.path.join(HERE, "results", re.sub(r"[^\w.-]+", "_", label) + "-" + stamp)
    os.makedirs(os.path.join(out_dir, "transcripts"), exist_ok=True)

    embed_url, embedder = None, None
    if args.embed_url:
        embed_url = args.embed_url
    elif not args.no_embeddings:
        from stand_in_embedder import serve
        embedder = serve(0, args.embed_dims)
        threading.Thread(target=embedder.serve_forever, daemon=True).start()
        embed_url = f"http://127.0.0.1:{embedder.server_address[1]}/v1"

    meta = {"model": label, "endpoint": "none" if (args.scripted or args.null_model) else args.base_url,
            "profile": args.profile, "started": stamp, "today": today,
            "embeddings": "off" if args.no_embeddings else (args.embed_url or "stand-in (word overlap)")}
    records = []
    for chore in chosen:
        for rep in range(args.repeat):
            rec = run_chore(args, chore, rep, today, embed_url, out_dir)
            records.append(rec)
            status = "PASS" if rec["pass"] else "fail"
            extra = rec.get("harness_error") or rec.get("model_error") or ""
            print(f"{status}  {chore['name']:18} score {rec['score']:.2f}  calls {rec['tool_calls']:2}  "
                  f"errors {rec['tool_errors']}  bad {rec['bad_calls']}  {rec['seconds']:6.1f}s  {extra[:100]}",
                  flush=True)
            if not rec["pass"]:
                for c in rec["checks"]:
                    if not c["ok"]:
                        print(f"      x {c['check']}: {c['detail']}", flush=True)
    summary = summarize(records)
    with open(os.path.join(out_dir, "results.json"), "w") as f:
        json.dump({"meta": meta, "summary": summary, "runs": records}, f, indent=2)
    with open(os.path.join(out_dir, "results.md"), "w") as f:
        f.write(markdown(meta, summary, records))
    print(f"\n{summary['passed']}/{summary['runs']} passed, mean score {summary['mean_score']:.2f}, "
          f"{summary['tool_calls']} tool calls, {summary['tool_errors']} tool errors, {summary['bad_calls']} bad calls.")
    print(f"Results: {out_dir}")
    if embedder:
        embedder.shutdown()
    if args.expect == "pass" and summary["passed"] != summary["runs"]:
        print("expected every run to pass", file=sys.stderr)
        return 1
    if args.expect == "fail" and (summary["passed"] or summary["harness_errors"]):
        print("expected every run to fail (and no harness errors)", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())

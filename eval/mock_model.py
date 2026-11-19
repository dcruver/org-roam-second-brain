#!/usr/bin/env python3
"""A fake chat model for testing the harness's model path without a real model.

It answers POST /v1/chat/completions like an OpenAI-compatible server with
tool calling.  It recognises which chore it was given from the user prompt
and replays that chore's reference tool calls from chores.py, one per
response, then gives the reference answer.  Nothing here is smart; it only
proves that run_eval.py talks the protocol correctly (tool definitions,
tool_call ids, tool results, the final reply) and counts what it should.

  --style native   tool calls in the message's tool_calls field (default)
  --style text     tool calls written in the text as <tool_call>{...}</tool_call>,
                   the way some local servers return them
  --faults         first response holds three bad calls: broken JSON arguments,
                   a tool that does not exist, and a note that does not exist,
                   so the harness's counts can be checked (expect 2 bad calls
                   and 1 tool error per chore)

  python3 eval/mock_model.py --port 8091 &
  python3 eval/run_eval.py --base-url http://127.0.0.1:8091/v1 --model mock
"""

import argparse
import datetime
import json
import sys
import os
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from chores import chores as load_chores  # noqa: E402


class Handler(BaseHTTPRequestHandler):
    style = "native"
    faults = False

    def log_message(self, fmt, *args):
        pass

    def reply(self, message):
        body = json.dumps({"id": "mock", "object": "chat.completion", "model": "mock",
                           "choices": [{"index": 0, "message": message, "finish_reason": "stop"}],
                           "usage": {"prompt_tokens": 10, "completion_tokens": 5}}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_POST(self):
        req = json.loads(self.rfile.read(int(self.headers.get("Content-Length", 0))))
        msgs = req["messages"]
        prompt = next(m["content"] for m in msgs if m["role"] == "user")
        chore = next((c for c in load_chores(datetime.date.today().isoformat()) if c["prompt"] == prompt), None)
        if chore is None:
            return self.reply({"role": "assistant", "content": "I don't know this chore."})
        # how far along are we: one assistant message per step so far
        done = sum(1 for m in msgs if m["role"] == "assistant")
        steps = list(chore.get("reference", []))
        if self.faults:
            steps = [None] + steps
        if done >= len(steps):
            return self.reply({"role": "assistant", "content": chore.get("reference_answer", "Done.")})
        step = steps[done]
        if step is None:
            calls = [("get_node", '{"id": "Garden"'), ("make_coffee", "{}"), ("get_node", '{"id": "No such note"}')]
        else:
            args = dict(step["args"])
            for k, v in list(args.items()):
                if isinstance(v, str) and v.startswith("$setup."):
                    args[k] = self.latest_change_id(msgs)
            calls = [(step["tool"], json.dumps(args))]
        if self.style == "text":
            text = "".join(f'<tool_call>{{"name": "{n}", "arguments": {a}}}</tool_call>' for n, a in calls)
            return self.reply({"role": "assistant", "content": text})
        return self.reply({"role": "assistant", "content": None, "tool_calls": [
            {"id": f"call_{done}_{i}", "type": "function", "function": {"name": n, "arguments": a}}
            for i, (n, a) in enumerate(calls)]})

    @staticmethod
    def latest_change_id(msgs):
        """The newest change listed by an earlier restore call (stands in for the model reading it)."""
        for m in reversed(msgs):
            text = m.get("content") or ""
            if m["role"] in ("tool", "user") and '"changes"' in text:
                start = text.find("{")
                try:
                    return json.loads(text[start:])["data"]["changes"][0]["change_id"]
                except (ValueError, KeyError, IndexError):
                    pass
        return "unknown"


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--port", type=int, default=8091)
    p.add_argument("--style", choices=["native", "text"], default="native")
    p.add_argument("--faults", action="store_true")
    a = p.parse_args()
    Handler.style, Handler.faults = a.style, a.faults
    print(f"mock model on http://127.0.0.1:{a.port}/v1 ({a.style}{', faults' if a.faults else ''})", file=sys.stderr)
    ThreadingHTTPServer(("127.0.0.1", a.port), Handler).serve_forever()


if __name__ == "__main__":
    main()

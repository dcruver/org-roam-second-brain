#!/usr/bin/env python3
"""A stand-in embeddings server, so semantic search works without a real model.

It answers POST /v1/embeddings (and /embeddings) the way an OpenAI-compatible
embeddings service does, but the vectors are word counts hashed into a fixed
number of dimensions: two texts score close when they share words.  That is
enough for the chores, which test whether a chat model uses the tools well,
not how good an embedding model is.  To test with your real embedding model,
pass --embed-url to run_eval.py instead and this server is not started.

Scores are lifted a little (see --floor) so that notes sharing a handful of
words clear the tools' default similarity cutoff of 0.5, as they would with a
real embedding model; unrelated notes stay below it.

Standard library only.  Run on its own with:  python3 stand_in_embedder.py --port 8090
"""

import argparse
import hashlib
import json
import math
import re
import sys
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

STOP = set("""
a an and are as at be been but by can do for from had has have i if in into is it its
my no not of on or our so than that the their then there these this to was we were what
when which who will with you your me us id end properties title node type status
""".split())


def stem(word):
    for suffix in ("ing", "ies", "es", "ed", "s"):
        if len(word) > len(suffix) + 2 and word.endswith(suffix):
            return word[: -len(suffix)] + ("y" if suffix == "ies" else "")
    return word


def embed(text, dims, floor):
    vec = [0.0] * dims
    words = [stem(w) for w in re.findall(r"[a-z][a-z0-9]+", text.lower()) if w not in STOP]
    counts = {}
    for w in words:
        counts[w] = counts.get(w, 0) + 1
    for w, c in counts.items():
        h = int.from_bytes(hashlib.sha1(w.encode()).digest()[:8], "big")
        vec[h % (dims - 1)] += (1.0 if (h >> 32) & 1 else -1.0) * math.sqrt(c)
    norm = math.sqrt(sum(x * x for x in vec)) or 1.0
    vec = [x / norm for x in vec]
    # The last dimension is shared by every text: it lifts every cosine score
    # from s to (s + floor) / (1 + floor).
    scale = 1.0 / math.sqrt(1.0 + floor)
    vec = [x * scale for x in vec]
    vec[-1] = math.sqrt(floor) * scale
    return vec


class Handler(BaseHTTPRequestHandler):
    dims = 768
    floor = 0.6

    def log_message(self, fmt, *args):  # quiet
        pass

    def do_POST(self):
        if not self.path.rstrip("/").endswith("embeddings"):
            self.send_error(404)
            return
        length = int(self.headers.get("Content-Length", 0))
        try:
            req = json.loads(self.rfile.read(length) or b"{}")
        except ValueError:
            self.send_error(400, "bad JSON")
            return
        inputs = req.get("input", "")
        if isinstance(inputs, str):
            inputs = [inputs]
        data = [{"object": "embedding", "index": i, "embedding": embed(t, self.dims, self.floor)}
                for i, t in enumerate(inputs)]
        body = json.dumps({"object": "list", "data": data, "model": req.get("model", "stand-in"),
                           "usage": {"prompt_tokens": 0, "total_tokens": 0}}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)


def serve(port, dims=768, floor=0.6, host="127.0.0.1"):
    Handler.dims, Handler.floor = dims, floor
    server = ThreadingHTTPServer((host, port), Handler)
    return server


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--port", type=int, default=8090)
    p.add_argument("--dims", type=int, default=768)
    p.add_argument("--floor", type=float, default=0.6)
    a = p.parse_args()
    server = serve(a.port, a.dims, a.floor)
    print(f"stand-in embedder on http://127.0.0.1:{server.server_address[1]}/v1", file=sys.stderr)
    server.serve_forever()


if __name__ == "__main__":
    main()

#!/bin/sh
# The notes server the clips are recorded against: a fresh copy of the sample vault
# (eval/vault) in demo/work/vault, served by eval/serve.el on 127.0.0.1:8019 with the
# stand-in embedder, so nothing here touches a real vault or needs an embedding model.
#   demo/serve.sh         start (a running one is stopped first)
#   demo/serve.sh stop    stop
set -eu
cd "$(dirname "$0")"
work=$PWD/work
stop() {
  for f in "$work"/server.pid "$work"/embedder.pid; do
    [ -f "$f" ] && kill "$(cat "$f")" 2>/dev/null || true
    rm -f "$f"
  done
}
stop
[ "${1:-}" = stop ] && exit 0

rm -rf "$work/vault" "$work/state" "$work/state-emacs"
mkdir -p "$work/state"
cp -r ../eval/vault "$work/vault"

python3 ../eval/stand_in_embedder.py --port 8091 >"$work/embedder.log" 2>&1 &
echo $! >"$work/embedder.pid"

: "${ORSB_DEPS:=$(ls -d "$HOME"/.config/emacs/.local/straight/build-*/ 2>/dev/null | head -1)}"
export ORSB_DEPS
ORSB_EVAL_VAULT=$work/vault ORSB_EVAL_STATE=$work/state ORSB_EVAL_PORT=8019 \
ORSB_EVAL_EMBED_URL=http://127.0.0.1:8091/v1 \
  "${EMACS:-emacs}" -Q --batch -l ../eval/serve.el >"$work/server.log" 2>&1 &
echo $! >"$work/server.pid"

i=0
until grep -q "ORSB-EVAL READY" "$work/server.log" 2>/dev/null; do
  i=$((i + 1))
  [ "$i" -gt 120 ] && { echo "the server did not start; see $work/server.log" >&2; exit 1; }
  sleep 0.5
done
grep "ORSB-EVAL READY" "$work/server.log"

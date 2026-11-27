#!/usr/bin/env bash
# Record docs/images/demo.gif, the README's clip (demo.tape). Needs VHS with ttyd,
# ffmpeg, Emacs with org-roam (ORSB_DEPS as for `make test`), and Claude Code signed
# in: the two requests in the clip are real ones, so the wording of the answers
# differs between recordings. Run from anywhere.
set -euo pipefail
cd "$(dirname "$0")"

./serve.sh
trap './serve.sh stop' EXIT
# Emacs's own index of the vault, built now so that it starts at once in the clip.
emacs -Q --batch -l init.el >/dev/null 2>&1
vhs demo.tape

# Claude Code prints nothing while it works, so each request is a still of ten seconds
# or more. Stills longer than 7 s (no pause in the tape is that long) are cut to 1.5 s.
cuts=$(ffmpeg -hide_banner -i work/demo.mp4 -vf freezedetect=n=0.001:d=7 -an -f null - 2>&1 |
  awk '/freeze_start/ {s=$NF} /freeze_end/ {printf "%sbetween(t,%.2f,%.2f)", sep, s+1.5, $NF; sep="+"}')
# The palette is made from the whole clip, so Emacs's colours survive.
ffmpeg -v error -y -i work/demo.mp4 -filter_complex \
  "select='not(${cuts:-0})',setpts=N/FRAME_RATE/TB,fps=10,split[x][y];[x]palettegen=stats_mode=full[p];[y][p]paletteuse=dither=bayer:bayer_scale=5:diff_mode=rectangle" \
  ../docs/images/demo.gif
ls -l ../docs/images/demo.gif

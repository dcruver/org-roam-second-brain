# The README's clip: Claude Code, connected to the notes server, answers from the
# vault and is asked to record a decision; the project type's rules send that to the
# approval screen in Emacs, where it is previewed and approved. Run by demo/record.sh
# (from demo/, after demo/serve.sh), which also cuts the waits for the model short.
Output work/demo.mp4

Set Shell "bash"
Set FontSize 16
Set Width 1280
Set Height 720
Set Padding 20
Set Theme "Catppuccin Mocha"
Set TypingSpeed 40ms
Set CursorBlink false

# `claude` is Claude Code with the demo server as its only MCP server and its tools
# allowed, so nothing asks; `emacs` is a bare Emacs with demo/init.el.
Hide
Type "export PS1='$ ' TERM=xterm-256color COLORTERM=truecolor DEMO=$PWD" Enter
Type `claude() { command claude --mcp-config "$DEMO/mcp.json" --strict-mcp-config --allowedTools mcp__notes --model sonnet "$@" </dev/null | fold -s -w 118; }` Enter
Type `emacs() { command emacs -nw -Q -l "$DEMO/init.el"; }` Enter
Type "mkdir -p work/cwd && cd work/cwd && clear" Enter
Sleep 1s
Show

Type `claude -p "What is my well's pressure switch set to?"` Sleep 400ms Enter
Wait@120s /^\$\s*$/
Sleep 4s
Type `claude -p "Record a decision in my fence repair project: cedar posts, not pine."` Sleep 400ms Enter
Wait@120s /^\$\s*$/
Sleep 6s

Type "emacs" Sleep 400ms Enter
# Two idle seconds after it starts, orsb-mode shows the digest.
Wait+Screen@60s /Waiting for approval/
Sleep 4s
# C-c b A: changes waiting for approval
Ctrl+C
Type "bA"
Sleep 3s
Type "n" Sleep 800ms
Enter
Sleep 3500ms
Type "q" Sleep 1s
Type "a" Sleep 2s
Type "y" Sleep 3s
Ctrl+X
Ctrl+F
Sleep 500ms
Type "projects/fence-repair.org" Sleep 400ms Enter
Sleep 1500ms
Ctrl+S
Type "* Decisions" Enter
Ctrl+L
Ctrl+L
Sleep 6s

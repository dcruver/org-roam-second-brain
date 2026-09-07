EMACS ?= emacs
# Directory (or colon-separated list) whose subdirectories hold org-roam and
# its dependencies, e.g. a straight.el build dir or ~/.emacs.d/elpa.
ORSB_DEPS ?= $(firstword $(wildcard $(HOME)/.config/emacs/.local/straight/build-*/))
export ORSB_DEPS

SOURCES := orsb-core.el org-roam-api.el org-roam-mcp-http.el org-roam-vector-search.el org-roam-second-brain.el

.PHONY: test lint compile clean reload

test:
	$(EMACS) -Q --batch -l test/run.el

# Files held to warnings-as-errors. Legacy files join this list as they are
# migrated onto orsb-core (see docs: Phase 5 of the 2.0 plan).
STRICT := orsb-core.el

LOADPATH := --eval '(dolist (dir (split-string (or (getenv "ORSB_DEPS") "") ":" t)) (dolist (sub (directory-files (expand-file-name dir) t "\\`[^.]")) (when (and (file-directory-p sub) (not (string-match-p "org-roam-second-brain" sub))) (add-to-list (quote load-path) sub t))))' --eval '(require (quote org-roam))'

# Byte-compile every source (warnings shown, not fatal), same load-path rules
# as the tests: working copy first, then ORSB_DEPS.
compile:
	$(EMACS) -Q --batch -L . $(LOADPATH) -f batch-byte-compile $(SOURCES)
	rm -f *.elc

# Warnings are errors for $(STRICT).
lint:
	$(EMACS) -Q --batch -L . $(LOADPATH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(STRICT)
	rm -f *.elc

clean:
	rm -f *.elc test/*.elc

# Reload the package in a running Emacs server (SERVER is the socket path).
SERVER ?= $(HOME)/emacs-server/server
reload:
	emacsclient -s $(SERVER) --eval '(progn (dolist (f (list "orsb-core" "org-roam-api" "org-roam-mcp-http")) (load f)) (org-roam-mcp-http-stop) (org-roam-mcp-http-start org-roam-mcp-http--port) (hash-table-count org-roam-mcp-http--tools))'

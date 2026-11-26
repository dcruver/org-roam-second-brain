"""Read an org-roam vault from disk and test it against a chore's expected end state.

Plain Python, standard library only.  It understands just enough org syntax
for the checks: file and heading property drawers (:ID:, :STATUS: ...),
#+title, headings with TODO keywords, DEADLINE/SCHEDULED lines, checkboxes
and [[id:...]] links.  It never asks Emacs, so a check cannot be fooled by
the server's own view of the vault.

A check is a dict with a "type" and its arguments; check() returns
(passed, detail).  Notes are picked by a selector:

  {"id": "eval-coop"}                 the note (or heading) with that :ID:
  {"path": "projects/garden.org"}     a file, relative to the vault
  {"title_re": "dehydrat", "type": "idea"}
                                      a note whose #+title matches the regular
                                      expression (case-insensitive), optionally
                                      of a type (the type of the folder the
                                      note is in); exactly one must match

Check types:

  note_exists     {note}                       the selector finds one note
  file_absent     {path}                       no such file in the vault
  id_resolves     {id, in_path?}               some file holds :ID: id (in that file)
  property        {note, key, equals}          file-level property, case-insensitive
  in_dir          {note, dir}                  the note's file is under dir/
  contains        {note, text | regex}         the note's text contains it
  not_contains    {note, text | regex}
  heading         {note, title_re, todo?, deadline?, scheduled?, absent?}
  checkbox        {note, text, checked}        a "- [ ]" / "- [X]" item
  link            {from, to, either_way?}      [[id:...]] link from one note to another
  proposal        {text_re, status?, absent?}  a change filed for approval whose stored
                                               call matches (asked of the server, not read
                                               from the vault: nothing is written until approved)
  answer          {all_re: [...], none_re?: [...]}
                                               tests the model's final reply
"""

import os
import re
from dataclasses import dataclass, field

DEFAULT_TODO = ["TODO", "NEXT", "WAITING", "HOLD", "DONE", "CANCELLED", "CANCELED"]
IGNORED_PROPERTIES = ("EMBEDDING", "EMBEDDING_HASH", "EMBEDDING_MODEL")


@dataclass
class Heading:
    level: int
    todo: str
    title: str
    start: int          # line index of the heading
    end: int            # line index after its subtree
    props: dict = field(default_factory=dict)
    deadline: str = None
    scheduled: str = None


@dataclass
class Note:
    path: str           # relative to the vault
    text: str
    props: dict
    title: str
    ids: list
    headings: list

    node_type: str = None   # the type of the folder the note lives in (its _schema.org)


def _drawer(lines, i):
    """Parse a :PROPERTIES: drawer starting at lines[i]; return (dict, next index)."""
    props = {}
    if i < len(lines) and lines[i].strip().upper() == ":PROPERTIES:":
        i += 1
        while i < len(lines) and lines[i].strip().upper() != ":END:":
            m = re.match(r"\s*:([^:\s]+):\s*(.*?)\s*$", lines[i])
            if m:
                props[m.group(1).upper()] = m.group(2)
            i += 1
        i += 1
    return props, i


def parse_note(root, rel):
    with open(os.path.join(root, rel), encoding="utf-8") as f:
        text = f.read()
    lines = text.split("\n")
    props, _ = _drawer(lines, 0)
    title = None
    todo = list(DEFAULT_TODO)
    for line in lines:
        m = re.match(r"#\+title:\s*(.*?)\s*$", line, re.I)
        if m and title is None:
            title = m.group(1)
        m = re.match(r"#\+(?:todo|seq_todo|typ_todo):\s*(.*)$", line, re.I)
        if m:
            todo += [w.split("(")[0] for w in m.group(1).split() if w != "|"]
    head_re = re.compile(r"^(\*+)\s+(?:(%s)\s+)?(.*?)(?:\s+(:[\w@#%%:]+:))?\s*$" % "|".join(map(re.escape, todo)))
    headings = []
    for i, line in enumerate(lines):
        m = head_re.match(line)
        if not m:
            continue
        h = Heading(level=len(m.group(1)), todo=m.group(2) or "", title=m.group(3), start=i, end=len(lines))
        j = i + 1
        while j < len(lines) and re.match(r"\s*(DEADLINE|SCHEDULED|CLOSED):", lines[j]):
            for kw, date in re.findall(r"(DEADLINE|SCHEDULED):\s*[<\[](\d{4}-\d{2}-\d{2})", lines[j]):
                setattr(h, kw.lower(), date)
            j += 1
        h.props, _ = _drawer(lines, j)
        headings.append(h)
    for k, h in enumerate(headings):
        for later in headings[k + 1:]:
            if later.level <= h.level:
                h.end = later.start
                break
    ids = [m.group(1) for m in re.finditer(r"^\s*:ID:\s*(\S+)\s*$", text, re.M)]
    return Note(path=rel, text=text, props=props, title=title, ids=ids, headings=headings)


SCHEMA_FILE = "_schema.org"


def folder_type(root, rel_dir):
    """The note type of the folder REL_DIR: the TYPE its _schema.org sets, else
    that of the nearest folder above that has one."""
    d = rel_dir
    while True:
        f = os.path.join(root, d, SCHEMA_FILE)
        if os.path.exists(f):
            with open(f, encoding="utf-8") as fh:
                m = re.search(r"^\s*:TYPE:\s*(\S+)", fh.read(), re.M)
            if m:
                return m.group(1)
        if not d:
            return None
        d = os.path.dirname(d)


class Vault:
    def __init__(self, root):
        self.root = root
        self.notes = []
        for dirpath, dirnames, filenames in os.walk(root):
            dirnames[:] = [d for d in dirnames if not d.startswith(".")]
            for fn in filenames:
                if fn.endswith(".org") and not fn.startswith(".#") and fn != SCHEMA_FILE:
                    rel = os.path.relpath(os.path.join(dirpath, fn), root)
                    note = parse_note(root, rel)
                    note.node_type = folder_type(root, os.path.dirname(rel))
                    self.notes.append(note)
        self.proposals = []   # filled by the harness: changes filed for approval

    # --- selection -------------------------------------------------------

    def select(self, sel):
        """Return (note, heading_or_None, error)."""
        if "path" in sel:
            hits = [n for n in self.notes if n.path == sel["path"]]
            return (hits[0], None, None) if hits else (None, None, f"no file {sel['path']}")
        if "id" in sel:
            for n in self.notes:
                if n.props.get("ID") == sel["id"]:
                    return n, None, None
                for h in n.headings:
                    if h.props.get("ID") == sel["id"]:
                        return n, h, None
            return None, None, f"no note with id {sel['id']}"
        if "title_re" in sel:
            rx = re.compile(sel["title_re"], re.I)
            hits = [n for n in self.notes if n.title and rx.search(n.title)
                    and ("type" not in sel or (n.node_type or "").lower() == sel["type"].lower())]
            if len(hits) == 1:
                return hits[0], None, None
            what = f"title /{sel['title_re']}/" + (f" type {sel['type']}" if "type" in sel else "")
            if not hits:
                return None, None, f"no note with {what}"
            return None, None, f"{len(hits)} notes with {what}: " + ", ".join(n.path for n in hits)
        return None, None, f"bad selector {sel}"

    @staticmethod
    def scope_text(note, heading):
        if heading is None:
            return note.text
        return "\n".join(note.text.split("\n")[heading.start:heading.end])

    def ids_in(self, note, heading):
        if heading is None:
            return set(note.ids)
        lines = note.text.split("\n")[heading.start:heading.end]
        return {m.group(1) for m in re.finditer(r"^\s*:ID:\s*(\S+)\s*$", "\n".join(lines), re.M)}

    # --- checks ------------------------------------------------------------

    def check(self, c, answer=""):
        fn = getattr(self, "_check_" + c["type"], None)
        if fn is None:
            return False, f"unknown check type {c['type']}"
        return fn(c, answer)

    def _note(self, c, key="note"):
        note, heading, err = self.select(c[key])
        if err:
            raise LookupError(err)
        return note, heading

    def _wrap(fn):
        def inner(self, c, answer):
            try:
                return fn(self, c, answer)
            except LookupError as e:
                return False, str(e)
        return inner

    def _check_proposal(self, c, _):
        rx = re.compile(c["text_re"], re.I | re.S)
        hits = [p for p in self.proposals
                if p.get("status") == c.get("status", "pending") and rx.search(p.get("text", ""))]
        if c.get("absent"):
            return (not hits), ("none filed" if not hits else f"a proposal matches /{c['text_re']}/")
        return bool(hits), (f"proposal {hits[0].get('id')} is waiting" if hits
                            else f"no {c.get('status', 'pending')} proposal matches /{c['text_re']}/ ({len(self.proposals)} filed)")

    @_wrap
    def _check_note_exists(self, c, _):
        note, _h = self._note(c)
        return True, f"found {note.path}"

    def _check_file_absent(self, c, _):
        gone = not os.path.exists(os.path.join(self.root, c["path"]))
        return gone, ("gone" if gone else f"{c['path']} still exists")

    def _check_id_resolves(self, c, _):
        holders = [n.path for n in self.notes if c["id"] in n.ids]
        if not holders:
            return False, f"no file holds :ID: {c['id']} (links to it are broken)"
        if "in_path" in c and c["in_path"] not in holders:
            return False, f":ID: {c['id']} is in {holders}, expected {c['in_path']}"
        if len(holders) > 1:
            return False, f":ID: {c['id']} appears in several files: {holders}"
        return True, f"in {holders[0]}"

    @_wrap
    def _check_property(self, c, _):
        note, heading = self._note(c)
        props = heading.props if heading else note.props
        val = props.get(c["key"].upper())
        ok = val is not None and val.lower() == str(c["equals"]).lower()
        return ok, f"{c['key']} = {val!r}"

    @_wrap
    def _check_in_dir(self, c, _):
        note, _h = self._note(c)
        ok = note.path.startswith(c["dir"].rstrip("/") + "/")
        return ok, f"file is {note.path}"

    def _match(self, c, text):
        if "regex" in c:
            return re.search(c["regex"], text, re.I | re.M) is not None
        return c["text"].lower() in text.lower()

    @_wrap
    def _check_contains(self, c, _):
        note, heading = self._note(c)
        ok = self._match(c, self.scope_text(note, heading))
        return ok, ("found" if ok else f"{note.path} lacks {c.get('text') or c.get('regex')!r}")

    @_wrap
    def _check_not_contains(self, c, _):
        note, heading = self._note(c)
        ok = not self._match(c, self.scope_text(note, heading))
        return ok, ("absent" if ok else f"{note.path} still has {c.get('text') or c.get('regex')!r}")

    @_wrap
    def _check_heading(self, c, _):
        note, _h = self._note(c)
        rx = re.compile(c["title_re"], re.I)
        hits = [h for h in note.headings if rx.search(h.title)]
        if c.get("absent"):
            return (not hits), ("absent" if not hits else f"{note.path} still has heading {hits[0].title!r}")
        if not hits:
            return False, f"{note.path} has no heading /{c['title_re']}/"
        h = hits[0]
        problems = []
        if "todo" in c and h.todo.upper() != c["todo"].upper():
            problems.append(f"todo is {h.todo or 'none'!r}, want {c['todo']!r}")
        for key in ("deadline", "scheduled"):
            if key in c and getattr(h, key) != c[key]:
                problems.append(f"{key} is {getattr(h, key)!r}, want {c[key]!r}")
        return (not problems), ("; ".join(problems) or f"heading {h.title!r} ok")

    @_wrap
    def _check_checkbox(self, c, _):
        note, heading = self._note(c)
        text = self.scope_text(note, heading)
        rx = re.compile(r"^\s*[-+*]\s+\[([ Xx-])\]\s+%s\s*$" % re.escape(c["text"]), re.M)
        m = rx.search(text)
        if not m:
            return False, f"no checkbox {c['text']!r} in {note.path}"
        checked = m.group(1) in "Xx"
        return checked == c["checked"], f"{c['text']!r} is {'[X]' if checked else '[ ]'}"

    def _links(self, src_note, src_heading, dst_ids):
        text = self.scope_text(src_note, src_heading)
        found = set(re.findall(r"\[\[id:([^\]\[]+)\]", text))
        return bool(found & dst_ids)

    @_wrap
    def _check_link(self, c, _):
        a, ah = self._note(c, "from")
        b, bh = self._note(c, "to")
        b_ids = self.ids_in(b, bh) if bh else {b.props.get("ID")}
        a_ids = self.ids_in(a, ah) if ah else {a.props.get("ID")}
        if self._links(a, ah, b_ids):
            return True, f"{a.path} links to {b.path}"
        if c.get("either_way") and self._links(b, bh, a_ids):
            return True, f"{b.path} links to {a.path}"
        return False, f"no link {a.path} -> {b.path}" + (" (either way)" if c.get("either_way") else "")

    def _check_answer(self, c, answer):
        answer = answer or ""
        missing = [rx for rx in c.get("all_re", []) if not re.search(rx, answer, re.I)]
        present = [rx for rx in c.get("none_re", []) if re.search(rx, answer, re.I)]
        ok = not missing and not present
        detail = []
        if missing:
            detail.append("reply lacks " + ", ".join(f"/{m}/" for m in missing))
        if present:
            detail.append("reply wrongly has " + ", ".join(f"/{p}/" for p in present))
        return ok, "; ".join(detail) or "reply ok"


# --- collateral damage ---------------------------------------------------

def _normalized(path):
    """File text without the lines the embedding worker adds on its own."""
    with open(path, encoding="utf-8") as f:
        lines = f.read().split("\n")
    keep = [l for l in lines if not re.match(r"\s*:(%s):" % "|".join(IGNORED_PROPERTIES), l)]
    return "\n".join(keep).rstrip() + "\n"


def snapshot(root):
    snap = {}
    for dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = [d for d in dirnames if not d.startswith(".")]
        for fn in filenames:
            if fn.endswith(".org"):
                p = os.path.join(dirpath, fn)
                snap[os.path.relpath(p, root)] = _normalized(p)
    return snap


def collateral(before, after, may_touch):
    """Files changed, added or removed outside the chore's may_touch globs."""
    import fnmatch
    allowed = lambda rel: any(fnmatch.fnmatch(rel, g) for g in may_touch)
    bad = []
    for rel in sorted(set(before) | set(after)):
        if allowed(rel):
            continue
        if rel not in after:
            bad.append(f"removed {rel}")
        elif rel not in before:
            bad.append(f"added {rel}")
        elif before[rel] != after[rel]:
            bad.append(f"changed {rel}")
    return bad

#!/usr/bin/env python3
"""Builds the anonymized archive that the paper's double-blind review gets as supplementary
material, and checks it.

    artifact/build.py --out ZIP --docs DIR [--sim DIR] [--allow-stale-docs]
    artifact/build.py --check DIR

The archive holds, under determinize-artifact/, the tracked files that SHIPPED selects,
artifact/README.md as its README, and website/: the landing page, the built simulator under sim/
(--sim, default sim/dist, which `npm run build` in sim/ writes), the API documentation under docs/
(--docs, doc-gen4's output) and every shipped text file as a page with line anchors under source/.
The website opens from disk, without a server and without building anything.

On the way, links to the repository and to the published site become links into the archive: to
the pages of source/ from the website, to the files themselves from the other files. The landing
page loses the metadata and the footer that name the published site and the commit, the
simulator its source maps, recorded absolute paths their home directory, and the files of
SECTIONS the sections that discuss drafts of the paper. Each entry of the zip has the same
timestamp and no owner, so the zip says nothing about when or by whom it was built.

The documentation's source links name the commit it was rendered from. Unless the Lean sources
at that commit are those of the working tree, the build fails, or with --allow-stale-docs warns:
pull requests reuse the documentation rendered on main, whose line numbers may be off.

The checks, which the build runs on its tree before writing the zip and --check runs on an
unpacked archive, find problems with
- identity: a pattern of artifact/identifying.txt in a file's path or text (HTML also with its
  character references decoded, JavaScript and JSON with their \\u escapes decoded), a commit of
  this repository (seven or more hex digits that start one), or an absolute path into a home or
  temporary directory, anywhere in the archive;
- what the website loads: a script, stylesheet, frame or image from a host outside LOAD_HOSTS;
- links: a relative link of the website's own pages (the landing page, the simulator, the pages
  of source/ and the documentation of this repository's modules) to a file or fragment that the
  archive lacks, a link from them to a host outside LINK_HOSTS, a relative link ending in `/`
  anywhere in the website (a browser lists the directory instead of opening its index.html from
  disk), and a relative link of a Markdown file to a path outside the archive. Of the relative
  links of the documentation, which doc-gen4 writes, only those into source/ fail the build; the
  others are reported, as the published documentation has them too.
"""
from __future__ import annotations

import argparse
import html
import os
import posixpath
import re
import subprocess
import sys
import tempfile
import unicodedata
import zipfile
from functools import cache
from pathlib import Path
from urllib.parse import unquote

ROOT = Path(__file__).resolve().parents[1]
ARTIFACT = ROOT / "artifact"
NAME = "determinize-artifact"

# The tracked files that the archive ships, as filter patterns over `git ls-files`: `*` matches
# within a path segment, `**` across segments, and a later pattern starting with `!` excludes.
# Files that a new pattern adds are checked like the others, but only a pattern adds one.
SHIPPED = [
    "lean/Determinize.lean",
    "lean/Determinize/**/*.lean",
    "lean/Main.lean",
    "lean/Tests/**/*.lean",
    "lean/lakefile.toml",
    "lean/lake-manifest.json",
    "lean/lean-toolchain",
    "lean/.gitignore",
    "lean/test.sh",
    "lean/README.md",
    "lean/finite-model-contract.md",
    "lean/docbuild/lakefile.toml",
    "lean/docbuild/lake-manifest.json",
    "lean/docbuild/lean-toolchain",
    "tests/**",
    # It tests .github/workflows, which the archive leaves out.
    "!tests/test_workflows.py",
    "examples/**",
    "benchmarks/**",
    "tools/**",
    # check.sh's helper, and the script that writes the paper's figure data into tex/.
    "!tools/dev-shell.sh",
    "!tools/paper-plots.py",
    "results/**",
    "run.sh",
    "test.sh",
    "flake.nix",
    "flake.lock",
    "flake-modules/systems.nix",
    "flake-modules/devshells/lean.nix",
]

# Sections of shipped Markdown files that the archive leaves out, by heading: each runs to the
# next heading of its level. The build fails when one is missing, so that a renamed section is
# not shipped unnoticed.
SECTIONS = {
    # It compares the Lean development with drafts of the paper at named commits.
    "lean/README.md": ["## Deviations from the paper"],
}

# The published site and the repository, whose links the archive replaces.
PUBLISHED = re.compile(r"https?://julesjacobs\.(?:com|github\.io)/determinize(?![\w.-])/?([^\s\"'<>)`\]]*)")
REPOSITORY = re.compile(
    r"https?://github\.com/julesjacobs/determinize(?:\.git)?(?![\w.-])"
    r"(?:/(blob|tree)/([^/\s\"'<>)`\]]+)/?([^\s\"'<>)#`\]]*))?(#[^\s\"'<>)`\]]*)?"
)

# Hosts that the website may load from: doc-gen4's pages load MathJax and a polyfill.
LOAD_HOSTS = {"cdn.jsdelivr.net", "cdnjs.cloudflare.com"}
# Hosts that the website's own pages may link to.
LINK_HOSTS = {"github.com", "www.w3.org", "www.graphviz.org", "libexpat.github.io", "arxiv.org"}

# Absolute paths into home and temporary directories, which name the account that recorded them.
# Emscripten's virtual home directory, in the simulator's Graphviz, names no one.
LOCAL_PATH = r"/(?:Users|home)/(?!web_user\b)[\w.-]+|/private/var/folders/|\b[A-Za-z]:\\+Users\\+[\w.-]+"
LOCAL_MARKERS = ("/Users/", "/home/", "/private/var/", ":\\")
# A recorded path: what is left after the home directory, with the checkout's name, is relative
# to the archive's root.
HOME_CHECKOUT = re.compile(r"(?:/Users|/home)/[^/\s\"']+/determinize/")
MAC_TEMPORARY = re.compile(r"/private/var/folders/[^\s\"']+?/T/")

LINE_RANGE = re.compile(r"#L(\d+)(?:-L(\d+))?\Z")
TIMESTAMP = (2026, 1, 1, 0, 0, 0)
# doc-gen4's declarations/declaration-data.bmp is JSON, read as text.
BINARY = {".png", ".jpg", ".jpeg", ".gif", ".ico", ".pdf", ".woff", ".woff2", ".zip"}


class Failure(Exception):
    pass


@cache
def pattern(glob):
    """A filter pattern as a regular expression, as in tests/test_workflows.py."""
    regex, i = "", 0
    while i < len(glob):
        if glob.startswith("**/", i):
            regex, i = regex + "(?:.*/)?", i + 3
        elif glob.startswith("**", i):
            regex, i = regex + ".*", i + 2
        elif glob[i] == "*":
            regex, i = regex + "[^/]*", i + 1
        else:
            regex, i = regex + re.escape(glob[i]), i + 1
    return re.compile(regex + r"\Z")


def selects(patterns, path):
    selected = False
    for glob in patterns:
        if pattern(glob.removeprefix("!")).match(path):
            selected = not glob.startswith("!")
    return selected


def git(*args):
    return subprocess.run(["git", "-C", str(ROOT), *args], text=True, capture_output=True, check=True).stdout


def shipped():
    """The shipped tracked files and their modes."""
    files = {}
    for line in git("ls-files", "-s", "-z").split("\0"):
        if line:
            meta, path = line.split("\t", 1)
            if selects(SHIPPED, path) and (ROOT / path).is_file():
                files[path] = 0o755 if meta.split()[0] == "100755" else 0o644
    return files


def text_of(data: bytes, path: str):
    """The text of a file, or None for a binary one."""
    if posixpath.splitext(path)[1].lower() in BINARY or b"\0" in data[:8192]:
        return None
    try:
        return data.decode("utf-8")
    except UnicodeDecodeError:
        return None


class Tree:
    """The archive's files, by path below its top directory: each one's content, or the file it
    copies unchanged, and its mode."""

    def __init__(self):
        self.files: dict[str, bytes | Path] = {}
        self.modes: dict[str, int] = {}

    def add(self, path, content, mode=0o644):
        if path in self.files:
            raise Failure(f"{path} is added twice")
        self.files[path] = content
        self.modes[path] = mode

    def read(self, path) -> bytes:
        content = self.files[path]
        return content.read_bytes() if isinstance(content, Path) else content

    def text(self, path) -> str:
        return self.read(path).decode("utf-8")

    def write(self, path, text: str):
        self.files[path] = text.encode("utf-8")

    def __contains__(self, path):
        return path in self.files

    def paths(self, prefix=""):
        return sorted(path for path in self.files if path.startswith(prefix))


def relative(target, source):
    """The relative link from the file `source` to the file `target`, both archive paths."""
    return posixpath.relpath(target, posixpath.dirname(source) or ".")


class Links:
    """Rewrites links to the published site and to the repository into links within the archive,
    and records which line ranges of which files the website links to."""

    def __init__(self, tree: Tree, sources: set[str], stale: bool):
        self.tree, self.sources, self.stale = tree, sources, stale
        self.ranges: dict[str, set[tuple[int, int]]] = {}
        self.lines: dict[str, int] = {}
        self.dropped = 0

    def published(self, match, source):
        url = match.group(1)
        path, hash_, fragment = url.partition("#")
        path = path.partition("?")[0]
        if path == "" or path.endswith("/"):
            path += "index.html"
        target = f"website/{path}"
        if target not in self.tree:
            raise Failure(f"{source} links {match.group(0)}, but the archive has no {target}")
        return relative(target, source) + hash_ + fragment

    def repository(self, match, source):
        kind, _, path, fragment = match.groups()
        fragment = fragment or ""
        in_website = source.startswith("website/")
        if kind != "blob":
            target = "website/source/index.html" if in_website else "README.md"
            return relative(target, source)
        if path not in self.sources:
            if not (self.stale and in_website):
                raise Failure(f"{source} links {match.group(0)}, but the archive does not ship {path}")
            self.dropped += 1
            return relative("website/source/index.html", source)
        if not in_website:
            # The file itself, which has no line anchors.
            return relative(path, source) + ("" if LINE_RANGE.match(fragment) else fragment)
        lines = LINE_RANGE.match(fragment)
        if lines:
            first, last = int(lines[1]), int(lines[2] or lines[1])
            if path not in self.lines:
                self.lines[path] = self.tree.text(path).count("\n") + 1
            count = self.lines[path]
            if not 1 <= first <= last <= count:
                if not self.stale:
                    raise Failure(f"{source} links lines {first}-{last} of {path}, which has {count}")
                fragment, self.dropped = "", self.dropped + 1
            elif lines[2]:
                self.ranges.setdefault(path, set()).add((first, last))
        return relative(f"website/source/{path}.html", source) + fragment

    def rewrite(self, text, source):
        text = PUBLISHED.sub(lambda m: self.published(m, source), text)
        return REPOSITORY.sub(lambda m: self.repository(m, source), text)


DIRECTORY_LINK = re.compile(r"""\b(href|src)=(["'])((?![a-z][\w+.-]*:|[#/])[^"'#?]*/|\.{1,2})([?#][^"']*)?\2""")


def directory_links(text):
    """Links to directories, which a browser lists from disk, as links to their index.html."""
    return DIRECTORY_LINK.sub(
        lambda m: f"{m[1]}={m[2]}{m[3].rstrip('/')}/index.html{m[4] or ''}{m[2]}", text)


def landing(text):
    """The landing page without its comments (drafts of links and authors), the metadata that
    names the published site, and the footer that names the commit."""
    text = re.sub(r"^[ \t]*<!--.*?-->[ \t]*\n", "", text, flags=re.S | re.M)
    text = re.sub(r"<!--.*?-->", "", text, flags=re.S)
    text = re.sub(r'\s*<link rel="canonical"[^>]*>', "", text)
    text = re.sub(r'\s*<meta (?:property="og:|name="twitter:)[^>]*>', "", text)
    text = re.sub(r"\s*<footer\b.*?</footer>", "", text, flags=re.S)
    if "@commit@" in text or "@short-commit@" in text:
        raise Failure("site/index.html still names the commit")
    return text


def remove_sections(text, path):
    for heading in SECTIONS.get(path, []):
        level = heading.split(" ", 1)[0]
        section = re.compile(rf"^{re.escape(heading)}\n.*?(?=^#{{1,{len(level)}}} |\Z)", re.S | re.M)
        text, count = section.subn("", text)
        if count != 1:
            raise Failure(f"{path} has no section {heading!r}; update SECTIONS in artifact/build.py")
    return text


def documentation_revisions(tree: Tree):
    revisions = set()
    for path in tree.paths("website/docs/"):
        if path.endswith(".html"):
            data = tree.read(path)
            if b"julesjacobs" in data:
                revisions |= {m[2] for m in REPOSITORY.finditer(data.decode("utf-8")) if m[1] == "blob"}
    return {revision for revision in revisions if re.fullmatch(r"[0-9a-f]{40}", revision)}


def stale_documentation(revisions):
    """Why the documentation's Lean sources differ from the working tree's, or None."""
    if len(revisions) != 1:
        return f"its source links name {len(revisions)} commits"
    (revision,) = revisions
    try:
        git("cat-file", "-e", f"{revision}^{{commit}}")
    except subprocess.CalledProcessError:
        return f"it was rendered from {revision[:12]}, which this clone lacks"
    lean = ["lean/Determinize.lean", "lean/Determinize"]
    if subprocess.run(["git", "-C", str(ROOT), "diff", "--quiet", revision, "--", *lean]).returncode:
        return f"it was rendered from {revision[:12]}, whose Lean sources differ"
    return None


SOURCE_PAGE = """<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{title}</title>
<link rel="stylesheet" href="{root}source.css">
<script defer src="{root}source.js"></script>
</head>
<body>
<header><a href="{root}index.html">Source</a> <span>{crumbs}</span></header>
<main>
<pre class="source"><code>{lines}</code></pre>
</main>
</body>
</html>
"""

SOURCE_INDEX = """<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Source</title>
<link rel="stylesheet" href="source.css">
</head>
<body>
<header><a href="../index.html">Determinize</a> <span>Source</span></header>
<main class="index">
<p>The files of the archive, as pages with line numbers. Start with
<a href="lean/Determinize/Theorems.lean.html">lean/Determinize/Theorems.lean</a>, which states every
theorem, and the definitions in <code>lean/Determinize/Spec/</code> that its statements use.</p>
{sections}
</main>
</body>
</html>
"""


def source_page(path, text, ranges):
    starts = {}
    for first, last in sorted(ranges):
        starts.setdefault(first, []).append(f'<span class="range" id="L{first}-L{last}"></span>')
    lines = text.split("\n")
    if lines and lines[-1] == "":
        lines.pop()
    body = "".join(
        "".join(starts.get(number, [])) + f'<span class="line" id="L{number}">{html.escape(line, quote=False)}</span>'
        for number, line in enumerate(lines, 1))
    depth = path.count("/")
    root = "../" * depth
    return SOURCE_PAGE.format(title=html.escape(path), root=root, crumbs=html.escape(path), lines=body)


def source_index(paths):
    by_directory = {}
    for path in paths:
        by_directory.setdefault(posixpath.dirname(path) or ".", []).append(path)
    sections = []
    for directory in sorted(by_directory):
        items = "\n".join(
            f'<li><a href="{html.escape(path)}.html">{html.escape(posixpath.basename(path))}</a></li>'
            for path in by_directory[directory])
        heading = "(root)" if directory == "." else directory + "/"
        sections.append(f"<h2>{html.escape(heading)}</h2>\n<ul>\n{items}\n</ul>")
    return SOURCE_INDEX.format(sections="\n".join(sections))


def build(sim: Path, docs: Path, allow_stale: bool):
    tree = Tree()
    files = shipped()
    for path, mode in files.items():
        tree.add(path, ROOT / path, mode)
    tree.add("README.md", ROOT / "artifact/README.md")

    # The landing page, which the website opens with.
    site = ROOT / "site"
    tree.add("website/index.html", landing((site / "index.html").read_text()).encode())
    for file in sorted([*site.glob("*.css"), *(site / "fonts").glob("*.woff2"), *(site / "fonts").glob("*.txt")]):
        tree.add(f"website/{file.relative_to(site).as_posix()}", file)

    # The simulator, without its source maps.
    if not (sim / "index.html").is_file():
        raise Failure(f"{sim}/index.html is missing; run 'npm run build' in sim/ or pass --sim.")
    for file in sorted(path for path in sim.rglob("*") if path.is_file()):
        relative_path = file.relative_to(sim).as_posix()
        if relative_path.endswith(".map"):
            continue
        content = file.read_bytes()
        if file.suffix == ".js":
            content = re.sub(rb"\n?//# sourceMappingURL=[^\n]*", b"", content)
        elif file.suffix == ".css":
            content = re.sub(rb"\n?/\*# sourceMappingURL=[^*]*\*/", b"", content)
        tree.add(f"website/sim/{relative_path}", content)

    # The API documentation, whose index is a placeholder of doc-gen4's: as in site/assemble.sh,
    # it sends the reader to the root module's page.
    if not (docs / "Determinize.html").is_file():
        raise Failure(f"{docs} holds no Determinize.html; pass doc-gen4's output as --docs.")
    for file in sorted(path for path in docs.rglob("*") if path.is_file()):
        relative_path = file.relative_to(docs).as_posix()
        if relative_path != "index.html":
            tree.add(f"website/docs/{relative_path}", file)
    tree.add("website/docs/index.html", b"""<!doctype html>
<meta charset="utf-8">
<title>Determinize</title>
<meta http-equiv="refresh" content="0; url=Determinize.html">
<a href="Determinize.html">Determinize</a>
""")

    stale = stale_documentation(documentation_revisions(tree))
    if stale and not allow_stale:
        raise Failure(f"The documentation is not of the working tree's Lean sources: {stale}. "
                      "Render it again, or pass --allow-stale-docs.")
    if stale:
        print(f"warning: the documentation is not of the working tree's Lean sources ({stale}); "
              "its source links may point to the wrong lines.", file=sys.stderr)

    # The shipped text files: without what discusses the paper's drafts, recorded home
    # directories and links that leave the archive.
    texts = {}
    for path in files:
        text = text_of(tree.read(path), path)
        if text is not None:
            texts[path] = text
    links = Links(tree, set(texts), bool(stale))
    for path, text in texts.items():
        text = remove_sections(text, path)
        text = HOME_CHECKOUT.sub("", text)
        text = MAC_TEMPORARY.sub("$TMPDIR/", text)
        text = links.rewrite(text, path)
        if text != texts[path]:
            texts[path] = text
            tree.write(path, text)
    for path in SECTIONS:
        if path not in texts:
            raise Failure(f"SECTIONS names {path}, which the archive does not ship")

    # The website's pages: links into the archive, and to directories' index.html.
    for path in tree.paths("website/"):
        if not path.endswith((".html", ".js")):
            continue
        data = tree.read(path)
        if path.endswith(".js") and b"julesjacobs" not in data:
            continue
        text = data.decode("utf-8")
        new = links.rewrite(text, path) if "julesjacobs" in text else text
        if path.endswith(".html"):
            new = directory_links(new)
        if new != text:
            tree.write(path, new)
    if links.dropped:
        print(f"warning: {links.dropped} source links lead to the source index or lost their line ranges, "
              "as their files or lines are missing.",
              file=sys.stderr)

    # The shipped text files as pages, with the anchors of the ranges that the website links.
    for path in sorted(texts):
        tree.add(f"website/source/{path}.html",
                 source_page(path, texts[path], links.ranges.get(path, ())).encode())
    tree.add("website/source/index.html", source_index(sorted(texts)).encode())
    tree.add("website/source/source.css", ARTIFACT / "source.css")
    tree.add("website/source/source.js", ARTIFACT / "source.js")
    return tree


def unpacked(directory: Path):
    tree = Tree()
    for file in sorted(path for path in directory.rglob("*") if path.is_file()):
        tree.add(file.relative_to(directory).as_posix(), file,
                 0o755 if os.access(file, os.X_OK) else 0o644)
    return tree


def identifying():
    patterns = []
    for line in (ARTIFACT / "identifying.txt").read_text().splitlines():
        line = line.strip()
        if line and not line.startswith("#"):
            patterns.append(line)
    return patterns


def required(regex):
    """The literal runs, in lower case, that every match of `regex` contains: those outside its
    groups, of three or more letters, digits, `.`, `-` and `@`. Searching a text for them first is
    much faster than matching the expression everywhere."""
    if "|" in re.sub(r"\((?:[^()]|\([^()]*\))*\)", "", regex) or "[" in regex:
        return []
    outside = re.sub(r"\((?:[^()]|\([^()]*\))*\)[?*]?", " ", regex)
    outside = re.sub(r"\\[bBsSdDwW][*+?]?|.[*?]|\{[^}]*\}", " ", outside)
    outside = outside.replace("\\.", ".").replace("\\-", "-")
    return [run.lower() for run in re.findall(r"[\w.@-]{3,}", outside)]


class Identity:
    """The patterns of artifact/identifying.txt, each with the literals it requires."""

    def __init__(self):
        self.patterns = [(re.compile(p, re.I), required(p)) for p in identifying()]

    def finditer(self, text):
        lower = text.lower()
        for regex, literals in self.patterns:
            if all(literal in lower for literal in literals):
                yield from regex.finditer(text)


def commit_prefixes():
    prefixes = set()
    for commit in git("rev-list", "--all").split():
        prefixes.update(commit[:n] for n in range(7, len(commit) + 1))
    return prefixes


def views(text, path):
    """The text, and the text with its escapes decoded where the file type has some."""
    yield text
    if "\u0308" in text:
        # A decomposed ü.
        yield unicodedata.normalize("NFC", text)
    if path.endswith((".html", ".htm", ".xml", ".svg")) and "&" in text:
        yield html.unescape(text)
    if path.endswith((".js", ".json", ".mjs")) and "\\u" in text:
        yield re.sub(r"\\u([0-9a-fA-F]{4})", lambda m: chr(int(m[1], 16)), text)


def own_page(path):
    """The pages of the website that this repository writes, as opposed to doc-gen4's pages of
    Lean, Mathlib and the other dependencies."""
    if not path.startswith("website/"):
        return False
    if path.startswith("website/docs/"):
        return path in ("website/docs/index.html", "website/docs/Determinize.html") or path.startswith(
            "website/docs/Determinize/")
    return True


ATTRIBUTE = re.compile(r"""\b(href|src)=(["'])([^"']*)\2""")
LOADED = re.compile(
    r"""<(?:script|img|iframe|source|embed)\b[^>]*\bsrc=(["'])(https?:)?//([^/"']+)[^"']*\1"""
    r"""|<link\b[^>]*\bhref=(["'])(https?:)?//([^/"']+)[^"']*\4"""
    r"""|url\(\s*["']?(https?:)?//([^/"')]+)""",
    re.I)
IDS = re.compile(r"""\b(?:id|name)=(["'])([^"']+)\1""")


def check(tree: Tree, commits=None):
    """The problems of the archive, and the broken links of doc-gen4's pages, as messages."""
    problems = []
    word = Identity()
    hexes = re.compile(r"(?<![0-9A-Za-z])[0-9a-f]{7,40}(?![0-9A-Za-z])")
    local = re.compile(LOCAL_PATH)
    commits = commit_prefixes() if commits is None else commits

    def where(text, match):
        start = max(0, match.start() - 40)
        return text[start:match.end() + 40].replace("\n", " ")

    # Identity, in every file; what the website loads, and its links to directories.
    for path in tree.paths():
        for match in word.finditer(path):
            problems.append(f"{path}: its path contains {match[0]!r}")
        data = tree.read(path)
        text = text_of(data, path)
        if text is None:
            # Binary files: their metadata and other ASCII strings.
            for match in word.finditer(data.decode("latin-1")):
                problems.append(f"{path}: contains {match[0]!r}")
            continue
        for view in views(text, path):
            for match in word.finditer(view):
                problems.append(f"{path}: names {match[0]!r}: …{where(view, match)}…")
        for match in hexes.finditer(text):
            if match[0] in commits:
                problems.append(f"{path}: names the commit {match[0]}: …{where(text, match)}…")
        for match in local.finditer(text) if any(marker in text for marker in LOCAL_MARKERS) else ():
            problems.append(f"{path}: has an absolute local path: …{where(text, match)}…")
        if path.startswith("website/") and path.endswith((".html", ".css", ".js")):
            for match in LOADED.finditer(text):
                host = match[3] or match[6] or match[8]
                if host.lower() not in LOAD_HOSTS:
                    problems.append(f"{path}: loads from {host}")
            if path.endswith(".html"):
                for match in DIRECTORY_LINK.finditer(text):
                    problems.append(f"{path}: links the directory {match[3]!r}")

    ids_of = {}

    def ids(path):
        if path not in ids_of:
            ids_of[path] = {m[2] for m in IDS.finditer(tree.read(path).decode("utf-8", "replace"))}
        return ids_of[path]

    # The links of the website's own pages. The documentation's pages are doc-gen4's; of their
    # relative links, only those into source/, which the build writes, are checked.
    warnings = []
    for path in tree.paths("website/"):
        if not own_page(path) or not path.endswith((".html", ".js")):
            continue
        documentation = path.startswith("website/docs/")
        for match in ATTRIBUTE.finditer(tree.text(path)):
            url = html.unescape(match[3])
            if not url or url.startswith(("data:", "mailto:", "javascript:", "blob:")) or "${" in url:
                continue
            if path.endswith(".js") and not re.match(r"(?:\.\.?/|https?://)[\w./-]*(?:[?#][^\s<>]*)?\Z", url):
                continue
            if url == "#top":
                continue
            if re.match(r"[a-z][\w+.-]*:|//", url, re.I):
                host = re.sub(r"^[a-z][\w+.-]*:", "", url, flags=re.I).lstrip("/").split("/")[0].lower()
                if host not in LINK_HOSTS | LOAD_HOSTS:
                    problems.append(f"{path}: links {url}")
                continue
            target, _, fragment = url.partition("#")
            target = target.partition("?")[0]
            if path.endswith(".js"):
                # The simulator's script creates its links on its page, and the targets of its
                # fragments with them.
                if not target:
                    continue
                base = "website/sim/index.html"
            else:
                base = path
            target = posixpath.normpath(posixpath.join(posixpath.dirname(base), target)) if target else base
            found = problems if not documentation or target.startswith("website/source/") else warnings
            if target not in tree:
                found.append(f"{path}: links {url}, which is not in the archive")
            elif fragment and target.endswith(".html") and unquote(fragment) not in ids(target):
                found.append(f"{path}: links {url}, but {target} has no {unquote(fragment)!r}")

    # The relative links of the Markdown files.
    directories = {posixpath.dirname(path) for path in tree.paths()}
    directories |= {posixpath.dirname(d) for d in set(directories)}
    markdown_link = re.compile(r"\]\(([^)\s]+)\)")
    for path in tree.paths():
        if not path.endswith(".md"):
            continue
        prose = re.sub(r"^```.*?^```|`[^`\n]*`", "", tree.text(path), flags=re.S | re.M)
        for match in markdown_link.finditer(prose):
            url = match[1].partition("#")[0]
            if not url or re.match(r"[a-z][\w+.-]*:", url, re.I):
                continue
            target = posixpath.normpath(posixpath.join(posixpath.dirname(path), url))
            if target.startswith("../") or (target not in tree and target not in directories):
                problems.append(f"{path}: links {match[1]}, which is not in the archive")
    return problems, warnings


def write_zip(tree: Tree, out: Path):
    out.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(dir=out.parent, prefix=f".{out.name}.", delete=False) as temporary:
        partial = Path(temporary.name)
    try:
        with zipfile.ZipFile(partial, "w", zipfile.ZIP_DEFLATED, compresslevel=9) as archive:
            for path in tree.paths():
                info = zipfile.ZipInfo(f"{NAME}/{path}", date_time=TIMESTAMP)
                info.create_system = 3
                info.external_attr = (0o100000 | tree.modes[path]) << 16
                info.compress_type = zipfile.ZIP_DEFLATED
                archive.writestr(info, tree.read(path))
        partial.chmod(0o644)
        partial.replace(out)
    finally:
        partial.unlink(missing_ok=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--out", type=Path, help="the zip to write")
    parser.add_argument("--sim", type=Path, default=ROOT / "sim/dist", help="the built simulator")
    parser.add_argument("--docs", type=Path, help="doc-gen4's output")
    parser.add_argument("--allow-stale-docs", action="store_true",
                        help="build with documentation of other Lean sources")
    parser.add_argument("--check", type=Path, metavar="DIR", help="check an unpacked archive instead")
    args = parser.parse_args()
    try:
        if args.check:
            if args.out or args.docs:
                parser.error("--check takes no --out or --docs")
            tree = unpacked(args.check)
            if "README.md" not in tree or "website/index.html" not in tree:
                raise Failure(f"{args.check} is not an unpacked archive: it has no README.md and website/index.html")
        else:
            if not args.out or not args.docs:
                parser.error("--out and --docs are required")
            tree = build(args.sim, args.docs, args.allow_stale_docs)
        problems, warnings = check(tree)
    except Failure as failure:
        sys.exit(f"artifact/build.py: {failure}")
    if warnings:
        print(f"warning: {len(warnings)} broken links in doc-gen4's pages, which the published "
              "documentation has too:", file=sys.stderr)
        for warning in warnings[:20]:
            print(f"  {warning}", file=sys.stderr)
    if problems:
        for problem in problems[:200]:
            print(problem, file=sys.stderr)
        if len(problems) > 200:
            print(f"… and {len(problems) - 200} more", file=sys.stderr)
        sys.exit(f"artifact/build.py: {len(problems)} problems; the archive was not written."
                 if not args.check else f"artifact/build.py: {len(problems)} problems.")
    if args.check:
        print(f"{args.check}: {len(tree.files)} files, no problems.")
    else:
        write_zip(tree, args.out)
        print(f"{args.out}: {len(tree.files)} files, {args.out.stat().st_size / 1e6:.1f} MB.")


if __name__ == "__main__":
    main()

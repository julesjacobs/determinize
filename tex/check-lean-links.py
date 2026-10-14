#!/usr/bin/env python3
"""Checks that the Lean links of the paper name the lines that hold their declarations.

    tex/check-lean-links.py

Each \\DeclareLeanLink of tex/lean-links.tex gives full Lean names, a file and a range of its
lines. The anonymous paper lists these in an appendix for the anonymized archive, which holds the
working tree, so the check reads the files there. A range must lie in its file, start at a
declaration or at the doc comment and attributes before it, end neither on a blank line nor in
the doc comment or the header of a declaration it does not name, and hold every declaration it
names: all of it, or, for a theorem, its statement up to the `:=` before the proof.

The camera-ready paper links the same lines on GitHub at \\leanrevision. When that commit's copies
of the linked files differ from the working tree's, the check passes but says so: before a
camera-ready build, \\leanrevision must name a commit with these lines.
"""
from __future__ import annotations

import re
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
LINKS = ROOT / "tex" / "lean-links.tex"

LINK = re.compile(
    r"\\DeclareLeanLink\{(?P<key>[^{}]*)\}\{(?P<title>.*)\}\{(?P<names>[^{}]*)\}"
    r"\{(?P<file>[^{}]*)\}\{(?P<first>\d+)(?:-(?P<last>\d+))?\}")
REVISION = re.compile(r"\\newcommand\{\\leanrevision\}\{([0-9a-f]{40})\}")
# A declaration's header at the start of a line.
HEADER = re.compile(
    r"(?:@\[[^\]]*\]\s*)*(?:(?:private|protected|noncomputable|partial|unsafe|nonrec)\s+)*"
    r"(?P<keyword>def|abbrev|theorem|lemma|inductive|structure|class|instance|opaque|axiom)\s+"
    r"(?P<name>[^\s({\[:]+)")
# The starts of lines that continue the declaration above them instead of starting a command.
CONTINUATION = re.compile(r"\s|\||deriving\b(?!\s+instance\b)|where\b|termination_by\b|decreasing_by\b|[)\]}⟩]")
NAMESPACE = re.compile(r"namespace\s+(\S+)")
# The other commands that `end` closes.
SCOPE = re.compile(r"(?:@\[[^\]]*\]\s*)?(?:(?:public|noncomputable)\s+)*section\b|mutual\b")
END = re.compile(r"end\b")


@dataclass
class Link:
    line: int
    key: str
    names: list[str]
    file: str
    first: int
    last: int


@dataclass
class Declarations:
    headers: dict[str, int]  # full name -> line of its header
    starts: dict[int, int]  # line of a header -> first line of its doc comment and attributes
    ends: dict[int, int]  # line of a header -> last line of the declaration, or of a theorem's statement


def comment_depth(line: str, depth: int) -> int:
    """The nesting of block comments after `line`, which starts at `depth`."""
    i = 0
    while i < len(line) - 1:
        pair = line[i:i + 2]
        if depth == 0 and pair == "--":
            break
        if pair == "/-":
            depth += 1
            i += 2
        elif pair == "-/" and depth > 0:
            depth -= 1
            i += 2
        else:
            i += 1
    return depth


def statement_end(lines: list[str], header: int) -> int:
    """The line of the `:=` that ends the statement of the theorem whose header is on `header`: the
    first outside brackets."""
    depth = 0
    for number in range(header, len(lines) + 1):
        code = lines[number - 1].split("--")[0]
        for i, c in enumerate(code):
            if c in "([{⦃⟨":
                depth += 1
            elif c in ")]}⦄⟩":
                depth -= 1
            elif depth == 0 and code.startswith(":=", i):
                return number
    return header


def declarations(lines: list[str]) -> Declarations:
    scopes: list[list[str]] = []
    result = Declarations({}, {}, {})
    keywords = {}
    commands = []  # the lines that start a command, or the doc comment and attributes before one
    depth = 0
    preamble = None  # the first line of the doc comment and attributes before the current line
    for number, line in enumerate(lines, 1):
        commented = depth > 0
        depth = comment_depth(line, depth)
        if commented:
            continue
        if line.startswith(("/-", "--")) or (line.startswith("@[") and not HEADER.match(line)):
            if preamble is None:
                preamble = number
                commands.append(number)
        elif header := HEADER.match(line):
            name = header["name"].removeprefix("_root_.") if header["name"].startswith("_root_.") \
                else ".".join([*(part for scope in scopes for part in scope), header["name"]])
            result.headers[name] = number
            result.starts[number] = preamble or number
            keywords[number] = header["keyword"]
            if preamble is None:
                commands.append(number)
            preamble = None
        else:
            if line.strip() and not CONTINUATION.match(line):
                commands.append(number)
                if namespace := NAMESPACE.match(line):
                    scopes.append(namespace[1].split("."))
                elif SCOPE.match(line):
                    scopes.append([])
                elif END.match(line) and scopes:
                    scopes.pop()
            preamble = None
    for header, keyword in keywords.items():
        end = next((command for command in commands if command > header), len(lines) + 1) - 1
        while end > header and not lines[end - 1].strip():
            end -= 1
        result.ends[header] = min(end, statement_end(lines, header)) if keyword in ("theorem", "lemma") else end
    return result


def problems(link: Link, text: str):
    lines = text.split("\n")
    if lines[-1] == "":
        lines.pop()
    if not 1 <= link.first <= link.last <= len(lines):
        yield f"lines {link.first}-{link.last} are not lines of {link.file}, which has {len(lines)}"
        return
    found = declarations(lines)
    if link.first not in found.starts and link.first not in found.starts.values():
        yield f"line {link.first} does not start a declaration or its doc comment"
    for name in link.names:
        if name not in found.headers:
            yield f"{link.file} declares no {name}"
        elif not link.first <= found.headers[name] <= link.last:
            yield f"{name} is declared on line {found.headers[name]}, outside lines {link.first}-{link.last}"
        elif found.ends[found.headers[name]] > link.last:
            yield f"{name} continues to line {found.ends[found.headers[name]]}, past line {link.last}"
    named = {found.headers.get(name) for name in link.names}
    if not lines[link.last - 1].strip():
        yield f"line {link.last} is blank"
    for header, start in found.starts.items():
        if start <= link.last < header:
            yield f"line {link.last} is in the doc comment of the declaration on line {header}"
        elif header == link.last != link.first and header not in named:
            yield f"line {link.last} is the header of a declaration that the link does not name"


def main() -> int:
    source = LINKS.read_text()
    links = []
    failures = []
    for number, line in enumerate(source.split("\n"), 1):
        if line.startswith("\\DeclareLeanLink"):
            if match := LINK.fullmatch(line.strip()):
                links.append(Link(number, match["key"], [name.strip() for name in match["names"].split(",")],
                                  match["file"], int(match["first"]), int(match["last"] or match["first"])))
            else:
                failures.append(f"tex/lean-links.tex:{number}: cannot read this \\DeclareLeanLink")
    for link in links:
        path = ROOT / link.file
        if not path.is_file():
            failures.append(f"tex/lean-links.tex:{link.line}: {link.key}: {link.file} does not exist")
            continue
        failures += [f"tex/lean-links.tex:{link.line}: {link.key}: {problem}"
                     for problem in problems(link, path.read_text())]
    if failures:
        print("\n".join(failures))
        return 1
    print(f"{len(links)} Lean links name the lines of their declarations in the working tree.")

    revision = REVISION.search(source)
    if not revision:
        print("tex/lean-links.tex defines no \\leanrevision.")
        return 1
    files = sorted({link.file for link in links})
    diff = subprocess.run(["git", "-C", str(ROOT), "diff", "--name-only", revision[1], "--", *files],
                          capture_output=True, text=True)
    if diff.returncode != 0:
        print(f"Not compared with \\leanrevision {revision[1][:12]}, which git cannot read here.")
    elif changed := diff.stdout.split():
        print(f"These linked files differ at \\leanrevision {revision[1][:12]}: {', '.join(changed)}. "
              "Before a camera-ready build, set it to a commit with these lines.")
    return 0


if __name__ == "__main__":
    sys.exit(main())

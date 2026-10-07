"""Public script entry points use Lean and preserve working-directory arguments, and each GitHub
workflow starts for exactly the files that it builds, tests or runs."""
import ast
from functools import cache
from pathlib import Path
import posixpath
import re
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
WORKFLOWS = ROOT / ".github/workflows"


class WorkflowTests(unittest.TestCase):
    def test_run_from_another_directory(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            (directory / "input.det").write_text("2 + 3")
            result = subprocess.run([ROOT / "run.sh", "--result", "model", "input.det"],
                                    cwd=directory, text=True, capture_output=True, timeout=120)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn("Expected terminal reward (kernel-checkable certificate generated)", result.stdout)
            self.assertTrue((directory / "model.result.lean").is_file())
            self.assertFalse((directory / "input.det.dout").exists())
            (directory / "bad.det").write_text("let x =")
            result = subprocess.run([ROOT / "run.sh", "--check", "bad.det"],
                                    cwd=directory, text=True, capture_output=True, timeout=120)
            self.assertNotEqual(result.returncode, 0)

    def test_storm_entry_point(self):
        result = subprocess.run([ROOT / "run.sh", "--storm", "--help"],
                                text=True, capture_output=True, timeout=120)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("--prefix", result.stdout)
        self.assertIn("--subject", result.stdout)


def uncommented(line):
    """`line` without a trailing YAML comment: a `#` outside quotes that starts it or follows a space."""
    quote = None
    for i, c in enumerate(line):
        if quote:
            quote = None if c == quote else quote
        elif c in "\"'":
            quote = c
        elif c == "#" and (i == 0 or line[i - 1] in " \t"):
            return line[:i].rstrip()
    return line.rstrip()


def scalar(text):
    if text[:1] in ("'", '"'):
        if len(text) < 2 or text[-1] != text[0]:
            raise ValueError(f"unterminated quote: {text}")
        return text[1:-1]
    if text[:1] in ("[", "{", "|", ">", "&", "*", "!", "%") or ": " in text:
        raise ValueError(f"unsupported YAML: {text}")
    return text


def block(lines, start):
    """Parses the block of (indentation, text) lines that begins at `start`; returns its value and
    the index after it."""
    indent, i = lines[start][0], start
    if lines[start][1].startswith("- "):
        items = []
        while i < len(lines) and lines[i][0] == indent and lines[i][1].startswith("- "):
            items.append(scalar(lines[i][1][2:].strip()))
            i += 1
        return items, i
    mapping = {}
    while i < len(lines) and lines[i][0] == indent:
        key, colon, rest = lines[i][1].partition(":")
        if not colon or rest[:1] not in ("", " "):
            raise ValueError(f"expected a key: {lines[i][1]}")
        rest, i = rest.strip(), i + 1
        if rest.startswith("[") and rest.endswith("]"):
            mapping[key] = [scalar(item.strip()) for item in rest[1:-1].split(",")]
        elif rest:
            mapping[key] = scalar(rest)
        elif i < len(lines) and lines[i][0] > indent:
            mapping[key], i = block(lines, i)
        else:
            mapping[key] = None
    if i < len(lines) and lines[i][0] > indent:
        raise ValueError(f"unexpected indentation: {lines[i][1]}")
    return mapping, i


@cache
def triggers(name):
    """The `on:` section of `.github/workflows/<name>.yml`, as nested dicts, lists and strings, with
    None for an event without settings. It covers the YAML that the section uses: block mappings
    and sequences, flow sequences, plain and quoted scalars, and comments."""
    lines, inside = [], False
    for line in map(uncommented, (WORKFLOWS / f"{name}.yml").read_text().splitlines()):
        if not line.strip():
            continue
        if not line[0].isspace():
            inside = line == "on:"
        elif inside:
            lines.append((len(line) - len(line.lstrip()), line.strip()))
    if not lines:
        raise ValueError(f"{name}.yml has no block `on:` section")
    value, end = block(lines, 0)
    if end != len(lines):
        raise ValueError(f"{name}.yml: unparsed `on:` line: {lines[end][1]}")
    return value


@cache
def pattern(glob):
    """A filter pattern as a regular expression: `*` matches within a path segment, `**` across
    segments, and `**/` also matches no directory at all."""
    regex, i = "", 0
    while i < len(glob):
        if glob.startswith("**/", i):
            regex, i = regex + "(?:.*/)?", i + 3
        elif glob.startswith("**", i):
            regex, i = regex + ".*", i + 2
        elif glob[i] == "*":
            regex, i = regex + "[^/]*", i + 1
        elif glob[i] in "?+[]\\!":
            raise ValueError(f"unsupported filter pattern: {glob}")
        else:
            regex, i = regex + re.escape(glob[i]), i + 1
    return re.compile(regex + r"\Z")


def selects(patterns, path):
    """Whether a `paths` filter selects `path`: the last pattern that matches it decides, and a
    pattern starting with `!` excludes."""
    selected = False
    for glob in patterns:
        if pattern(glob.removeprefix("!")).match(path):
            selected = not glob.startswith("!")
    return selected


def starts(name, event, path):
    """Whether `event` (`pull_request`, or `push` to main) with a change to `path` alone starts the
    workflow `name`."""
    on = triggers(name)
    if event not in on:
        return False
    settings = on[event] or {}
    if set(settings) - {"branches", "paths"}:
        raise ValueError(f"{name}.yml: unsupported {event} settings: {sorted(settings)}")
    if event == "push" and "main" not in settings.get("branches", ["main"]):
        return False
    return "paths" not in settings or selects(settings["paths"], path)


def workflows():
    return sorted(path.stem for path in WORKFLOWS.glob("*.yml"))


def pull_request_starts(path):
    return {name for name in workflows() if starts(name, "pull_request", path)}


@cache
def tracked():
    return frozenset(subprocess.run(["git", "ls-files"], cwd=ROOT, text=True, capture_output=True,
                                    check=True).stdout.splitlines())


def named(source, reference):
    """The tracked files that a path written in `source` names, relative to the directory of
    `source` or to the repository root. `*` and `${…}` stand for part of a name, but the first
    directory and part of the file name must be written out."""
    reference = re.sub(r"\$\{[^}]*\}", "*", reference)
    name = reference.split("/")[-1].replace("*", "")
    if not re.fullmatch(r"[\w.*/-]+", reference) or not re.search(r"\w", name):
        return set()
    files = set()
    for base in (posixpath.dirname(source), ""):
        path = posixpath.normpath(posixpath.join(base, reference))
        if "*" in path.split("/")[0]:
            continue
        if "*" in path:
            files |= {file for file in tracked() if pattern(path).match(file)}
        elif path in tracked():
            files.add(path)
    return files


def python_paths(source):
    """The files that a Python module names as an operand of `/`, and the modules of tests/ and
    tools/ that it imports."""
    files = set()
    for node in ast.walk(ast.parse((ROOT / source).read_text())):
        if (isinstance(node, ast.BinOp) and isinstance(node.op, ast.Div)
                and isinstance(node.right, ast.Constant) and isinstance(node.right.value, str)):
            files |= named(source, node.right.value)
        elif isinstance(node, ast.Import):
            files |= {f"{d}/{a.name}.py" for a in node.names for d in ("tests", "tools")} & tracked()
        elif isinstance(node, ast.ImportFrom) and node.module:
            files |= {f"{d}/{node.module}.py" for d in ("tests", "tools")} & tracked()
    return files


def shell_paths(source):
    """The files that the words of a shell script name."""
    words = re.findall(r"[^\s\"'();|&<>]+", (ROOT / source).read_text())
    return set().union(*(named(source, word) for word in words))


def script_paths(source):
    """The files that the string literals of a TypeScript file name."""
    literals = re.findall(r'"([^"\\\n]*)"|\'([^\'\\\n]*)\'|`([^`]*)`', (ROOT / source).read_text())
    return set().union(*(named(source, text) for groups in literals for text in groups if text))


def import_paths(source):
    """The files outside sim/ that a TypeScript file imports."""
    specifiers = re.findall(r"""(?:from|import)\s+["'](\.\.?/[^"']+)["']""",
                            (ROOT / source).read_text())
    files = set().union(*(named(source, specifier) for specifier in specifiers))
    return {file for file in files if not file.startswith("sim/")}


def tex_paths(source):
    """The files that a LaTeX file includes; latexmk runs in tex/."""
    command = r"\\(?:input|include|includegraphics|bibliography)\s*(?:\[[^\]]*\])?\{([^}]+)\}"
    references = re.findall(command, (ROOT / source).read_text())
    return set().union(*(named("tex/main.tex", reference + suffix)
                         for reference in references for suffix in ("", ".*")))


# For each workflow with a pull-request filter: the code it runs, as filters over the tracked files,
# and how to read the repository paths that this code names.
GUARDED = {
    "lean": [(["tests/**/*.py"], python_paths), (["lean/test.sh"], shell_paths)],
    "sim": [(["sim/test/**", "sim/build.ts"], script_paths), (["sim/src/**"], import_paths)],
    "tex": [(["tex/**/*.tex", "!tex/archive/**"], tex_paths)],
    # check.sh site builds the simulator and assembles the site with site/assemble.sh.
    "site": [(["site/**/*.mts", "sim/e2e/**", "sim/playwright.config.ts", "sim/build.ts"], script_paths),
             (["site/**/*.sh"], shell_paths), (["sim/src/**"], import_paths)],
}


def named_paths(name):
    return {path for globs, find in GUARDED[name] for source in tracked() if selects(globs, source)
            for path in find(source)}


# Each change and the workflows that a pull request with only that change starts.
PULL_REQUEST_STARTS = {
    "README.md": set(),
    "check.sh": set(),
    "test.sh": set(),
    "tools/dev-shell.sh": set(),
    "tools/bench.py": set(),
    ".claude/settings.json": set(),
    "lean/README.md": set(),
    "tests/README.md": set(),
    "tex/reviews/section-6.md": set(),
    "tex/archive/1_introduction.tex": set(),
    "benchmarks/approx/branching.det": set(),
    "flake-modules/devshells/all.nix": set(),
    "lean/Determinize/Theorems.lean": {"lean", "site"},
    "lean/Determinize/Spec/Syntax.lean": {"lean", "site"},
    "lean/Determinize.lean": {"lean", "site"},
    "lean/Determinize/Proof/Traces/ConditionalLaw.lean": {"lean"},
    "lean/docbuild/lakefile.toml": {"lean"},
    "lean/test.sh": {"lean"},
    "run.sh": {"lean"},
    "tools/storm.py": {"lean"},
    "tools/storm_additive.py": {"lean"},
    "flake-modules/devshells/lean.nix": {"lean"},
    ".github/workflows/lean.yml": {"lean"},
    ".github/workflows/pages.yml": {"lean"},
    "sim/src/main.ts": {"sim", "site"},
    "sim/package-lock.json": {"sim", "site"},
    "sim/e2e/site.spec.ts": {"sim", "site"},
    "flake-modules/devshells/sim.nix": {"sim", "site"},
    "site/index.html": {"site"},
    "site/assemble.sh": {"site"},
    "flake-modules/devshells/site.nix": {"site"},
    ".github/workflows/site.yml": {"lean", "site"},
    "tests/cases.toml": {"lean", "sim"},
    "examples/paper/noisy-product.det": {"lean", "sim", "site"},
    "tests/statistical/nested.det": {"lean", "sim"},
    "tests/test_results.py": {"lean", "sim"},
    "examples/baselines/clickGraph.sgcl": {"lean", "sim", "site"},
    ".github/workflows/sim.yml": {"lean", "sim"},
    "tex/sections/1_introduction.tex": {"tex"},
    "results/estimator-variance/hmm.png": {"tex"},
    "results/approx-results.csv": {"tex"},
    "flake-modules/devshells/tex.nix": {"tex"},
    ".github/workflows/tex.yml": {"lean", "tex"},
    "flake.nix": {"lean", "sim", "tex", "site"},
    "flake.lock": {"lean", "sim", "tex", "site"},
    "flake-modules/systems.nix": {"lean", "sim", "tex", "site"},
}


def samples(glob):
    """Paths that a pattern selects, with `**` standing for no directory and for two."""
    glob = glob.removeprefix("!")
    for directories in ("", "a/b/"):
        yield glob.replace("**/", directories).replace("**", directories + "x").replace("*", "x")


class TriggerTests(unittest.TestCase):
    def test_pages_deploys_for_the_inputs_of_what_it_publishes(self):
        called = re.findall(r"^\s+uses: \S*/\.github/workflows/(\S+)\.yml$",
                            (WORKFLOWS / "pages.yml").read_text(), re.M)
        self.assertTrue(called)
        patterns = [glob for name in workflows() for settings in triggers(name).values()
                    for glob in (settings or {}).get("paths", [])]
        paths = {*tracked(), *PULL_REQUEST_STARTS, *(path for glob in patterns for path in samples(glob))}
        for path in sorted(paths):
            with self.subTest(path=path):
                self.assertEqual(starts("pages", "push", path),
                                 any(starts(name, "pull_request", path) for name in called))

    def test_pages_skips_the_lean_tests_by_the_files_that_start_them(self):
        hashed = re.search(r"lean=\$\{\{ hashFiles\((.*?)\) \}\}", (WORKFLOWS / "pages.yml").read_text())
        self.assertIsNotNone(hashed)
        self.assertEqual(re.findall(r"'([^']*)'", hashed[1]), triggers("lean")["pull_request"]["paths"])

    def test_every_workflow_has_expectations(self):
        for path in sorted(WORKFLOWS.iterdir()):
            with self.subTest(path=path.name):
                self.assertEqual(path.suffix, ".yml")
                self.assertIn(f".github/workflows/{path.name}", PULL_REQUEST_STARTS)
                if "pull_request" in triggers(path.stem):
                    self.assertIn(path.stem, GUARDED)
                    self.assertTrue(any(path.stem in started for started in PULL_REQUEST_STARTS.values()))

    def test_only_pages_runs_on_pushes_to_main(self):
        for name in workflows():
            with self.subTest(workflow=name):
                self.assertEqual("push" in triggers(name), name == "pages")


def check_pull_request(path, expected):
    def test(self):
        self.assertEqual(pull_request_starts(path), expected)
    return test


for change, workflows_started in PULL_REQUEST_STARTS.items():
    setattr(TriggerTests, "test_pull_request_changing_" + re.sub(r"\W", "_", change),
            check_pull_request(change, workflows_started))

def check_named_paths(name):
    def test(self):
        paths = named_paths(name)
        self.assertTrue(paths)
        for path in sorted(paths):
            with self.subTest(path=path):
                self.assertTrue(starts(name, "pull_request", path), f"{path} does not start {name}.yml")
    return test


for workflow in GUARDED:
    setattr(TriggerTests, f"test_paths_named_by_what_{workflow}_runs_start_it",
            check_named_paths(workflow))


if __name__ == "__main__":
    unittest.main()

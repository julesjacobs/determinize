#!/usr/bin/env bash
# PreToolUse (Bash): deny or escalate a few commands that are outward-facing,
# rewrite shared files wholesale, or change pinned dependencies.
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"
read_hook_input
cmd="$(jfield tool_input.command)"

python3 - "$cmd" <<'PY'
import json, re, sys
raw = sys.argv[1]

# Strip heredoc bodies and quoted strings so that text being written to files
# (docs, scripts, memory notes) is not mistaken for a command invocation.
def strip_heredocs(text):
    out, lines, i = [], text.split("\n"), 0
    while i < len(lines):
        line = lines[i]; out.append(line); i += 1
        for m in re.finditer(r"<<-?\s*['\"]?(\w+)['\"]?", line):
            term = m.group(1)
            while i < len(lines) and lines[i].strip() != term:
                i += 1
            i += 1  # skip the terminator line
    return "\n".join(out)

cmd = strip_heredocs(raw)
def deny(reason):
    print(reason, file=sys.stderr); sys.exit(2)
def ask(reason):
    print(json.dumps({"hookSpecificOutput": {"hookEventName": "PreToolUse",
          "permissionDecision": "ask", "permissionDecisionReason": reason}})); sys.exit(0)

if re.search(r"deploy-to-website", cmd):
    deny("sim/deploy-to-website.sh commits and pushes to an external website repository. "
         "Only the user runs it; tell them the bundle is ready instead.")
if re.search(r"\bdune\s+fmt\b|@fmt\b.*--auto-promote|ocamlformat\s+(-i\b|--inplace)", cmd):
    deny("The OCaml sources are not ocamlformat-formatted; a whole-file reformat would bury the real diff. "
         "Match the surrounding style by hand (see .claude/rules/ocaml.md).")
if re.search(r"\bgit\s+push\b.*(--force|-f\b|\+)", cmd):
    deny("Force-pushing a shared branch is not allowed.")
if re.search(r"\bnix\s+flake\s+(update|lock)\b", cmd):
    ask("This changes flake.lock (pinned toolchain for everyone).")
if re.search(r"\bnpm\s+(install|i|add|update|up|uninstall|rm|audit\s+fix)\b", cmd) and not re.search(r"\bnpm\s+ci\b", cmd):
    ask("This changes sim/package.json or sim/package-lock.json; the committed bundle must then be rebuilt.")
if re.search(r"\bgit\s+push\b", cmd):
    ask("Pushing to the shared remote.")
PY

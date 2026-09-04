#!/usr/bin/env python3
"""Heuristic scanner for prompt-injection / poisoning indicators in a skill or plugin tree.

Usage: scan.py DIR [--all-files]
Prints one line per finding: SEVERITY  path:line  rule  excerpt. Exit 0 = no HIGH findings,
1 = HIGH findings present, 2 = usage error. A clean run is necessary, not sufficient.
"""
import os, re, sys, unicodedata

TEXT_EXT = {".md", ".txt", ".json", ".yaml", ".yml", ".sh", ".bash", ".zsh", ".py", ".js", ".mjs", ".cjs",
            ".ts", ".rb", ".pl", ".toml", ".ini", ".cfg", ".html", ".xml", ".ps1", ".bat", ""}
RULES = [
    ("HIGH", "instruction-override", re.compile(r"ignore (all |any )?(previous|prior|above) (instructions|rules)|disregard (the )?(system|previous)|do not (tell|inform|mention to) the user|without (telling|informing) the user|(hide|conceal) (this|it) from the user|you are now (in )?(developer|god|unrestricted) mode|jailbreak", re.I)),
    ("HIGH", "exfiltration", re.compile(r"\b(curl|wget|Invoke-WebRequest|fetch\(|urllib\.request|requests\.(get|post)|http\.(get|post)|XMLHttpRequest|nc |ncat |socat )", re.I)),
    ("HIGH", "secrets-access", re.compile(r"~/\.ssh|id_rsa|\.aws/credentials|ANTHROPIC_API_KEY|OPENAI_API_KEY|AWS_(SECRET|ACCESS)|GITHUB_TOKEN|\.netrc|keychain|/etc/shadow|\.env\b", re.I)),
    ("HIGH", "destructive", re.compile(r"rm\s+-[a-zA-Z]*r[a-zA-Z]*f|rm\s+-[a-zA-Z]*f[a-zA-Z]*r|git\s+push\s+.*(--force|-f\b)|git\s+reset\s+--hard|chmod\s+-R?\s*777|mkfs|dd\s+if=|:\(\)\{|shutdown|reboot", re.I)),
    ("HIGH", "remote-exec", re.compile(r"(curl|wget)[^|\n]*\|\s*(ba|z|k)?sh\b|\beval\s*\(|\bexec\s*\(|base64\s+(-d|--decode)|\bpython[0-9.]*\s+-c\s|Function\(|new Function", re.I)),
    ("HIGH", "permission-escalation", re.compile(r"bypassPermissions|dontAsk|--dangerously-skip-permissions|allowManagedHooksOnly|disableAllHooks", re.I)),
    ("MED", "hidden-html-comment", re.compile(r"<!--.*?-->", re.S)),
    ("MED", "base64-blob", re.compile(r"[A-Za-z0-9+/]{80,}={0,2}")),
    ("MED", "hex-blob", re.compile(r"(\\x[0-9a-fA-F]{2}){8,}|[0-9a-fA-F]{64,}")),
    ("MED", "model-addressed", re.compile(r"\b(assistant|claude|model)\b[^.\n]{0,40}\b(must|should|always|never)\b[^.\n]{0,60}\b(silently|secretly|quietly)\b", re.I)),
    ("MED", "runtime-hook", re.compile(r"\"(PreToolUse|PostToolUse|Stop|SessionStart|UserPromptSubmit|Notification)\"|mcpServers|\"command\"\s*:", re.I)),
    ("LOW", "sudo", re.compile(r"\bsudo\b")),
]
INVISIBLE = {"​", "‌", "‍", "⁠", "﻿", "­", "‪", "‫", "‬", "‭", "‮", "⁦", "⁧", "⁨", "⁩", "᠎"}


def scan_file(path, findings):
    try:
        data = open(path, "rb").read()
    except OSError:
        return
    if b"\0" in data[:4096]:
        findings.append(("MED", path, 0, "binary-file", "binary content; inspect manually"))
        return
    text = data.decode("utf-8", errors="replace")
    for i, line in enumerate(text.splitlines(), 1):
        for ch in line:
            if ch in INVISIBLE or unicodedata.category(ch) == "Cf":
                findings.append(("HIGH", path, i, "invisible-unicode", f"U+{ord(ch):04X} {unicodedata.name(ch, '?')}"))
                break
        for sev, name, rx in RULES:
            if name == "hidden-html-comment":
                continue
            m = rx.search(line)
            if m:
                findings.append((sev, path, i, name, line.strip()[:160]))
    for m in RULES[6][2].finditer(text):  # multi-line HTML comments
        i = text.count("\n", 0, m.start()) + 1
        body = m.group(0).strip()[:160].replace("\n", " ")
        if re.search(r"[A-Za-z]{4,}", body):
            findings.append(("MED", path, i, "hidden-html-comment", body))


def main():
    if len(sys.argv) < 2 or not os.path.isdir(sys.argv[1]):
        print(__doc__); sys.exit(2)
    root, all_files = sys.argv[1], "--all-files" in sys.argv
    findings = []
    for d, dirs, files in os.walk(root):
        dirs[:] = [x for x in dirs if x not in {".git", "node_modules"}]
        for f in files:
            p = os.path.join(d, f)
            if all_files or os.path.splitext(f)[1].lower() in TEXT_EXT:
                scan_file(p, findings)
    order = {"HIGH": 0, "MED": 1, "LOW": 2}
    findings.sort(key=lambda x: (order[x[0]], x[1], x[2]))
    for sev, p, i, name, ex in findings:
        print(f"{sev:4} {os.path.relpath(p, root)}:{i}  {name}  {ex}")
    high = sum(1 for x in findings if x[0] == "HIGH")
    print(f"\n{len(findings)} finding(s), {high} HIGH. Scanned {root}. A clean scan does not replace reading the files.")
    sys.exit(1 if high else 0)


if __name__ == "__main__":
    main()

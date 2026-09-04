---
name: learn-tool
description: Learn the current best practices for a tool, library, language, or package before relying on it, and capture them as a path-scoped rule in .claude/rules/ so every future session follows them. Use whenever a new tool or dependency is added to the repo (new devshell package in flake-modules/, new npm dependency, new LaTeX package, new OCaml library, new CLI in a script), when a tool is upgraded to a new major version, or when you are about to use a tool whose conventions are not yet covered by .claude/rules/. Also invoked as /learn-tool <tool>.
argument-hint: <tool name> [paths it applies to]
allowed-tools: Read, Grep, Glob, Bash, WebSearch, WebFetch, Write, Edit, Agent
---

Learn how `$ARGUMENTS` is meant to be used today and encode it for this repository.

Why this exists: the owner wants every tool in this repo used the way its maintainers recommend, verified against reputable online sources rather than memory, and wants that learning repeated automatically whenever a new tool appears.

## Procedure

1. **Scope.** Identify what the tool is, where it is used in this repo (grep `flake-modules/`, `sim/package.json`, `ocaml/dune`, `tex/main.tex`, scripts), and the installed version (`flake.lock`, `package-lock.json`, or `TOOL --version` inside the right devshell). If `.claude/rules/` already has a file covering it, read it: you are updating, not duplicating.
2. **Research.** Delegate to the `tool-researcher` subagent with the tool name, installed version, how it is used here, and the list of neighbouring tools it must integrate with. Ask for canonical commands, config, conventions, LLM pitfalls, failure detection, and sources. Run two researchers in parallel only if the tool has clearly separate facets (e.g. a language and its build system).
3. **Distill.** Keep only what changes how one should act in this repo: commands, config files, conventions that differ from defaults, gotchas, failure signatures. Drop tutorials and generic advice.
4. **Write the rule.** Create or update `.claude/rules/<tool-or-area>.md` with `paths:` frontmatter limited to the files where the tool matters (see the existing files for the format). Target 30-70 lines. Cite the sources and the verification date in one line. If a canonical config file is missing (e.g. `.ocamlformat`), propose it to the user; do not add repo-level config files without their agreement (the owner prefers to keep the non-Claude surface of the repo untouched).
5. **Wire it in.** If the tool has a build/test/lint command, add it to `.claude/scripts/check.sh` (and, if fast, to `.claude/hooks/post-edit.sh`). Add read-only permission rules for its docs domain to `.claude/settings.json` `permissions.allow` (`WebFetch(domain:...)`). Add a one-line row to the toolchain table in `CLAUDE.md` if the tool is a first-class part of the workflow.
6. **Remember.** Add a `reference` memory entry (documentation URLs) and, if the user expressed a preference during the process, a `feedback` entry; update `MEMORY.md`.
7. **Report** what was learned in five bullets, and which files changed.

Reputable sources by ecosystem: OCaml - ocaml.org, dune.readthedocs.io, GitHub ocaml/* and ocaml-ppx/*; Nix - nix.dev, nixos.org manuals, flake.parts, wiki.nixos.org; JS - nodejs.org/api, esbuild.github.io, codemirror.net, docs.npmjs.com; LaTeX - ctan.org package docs, texdoc.org, the class author's GitHub; verification tools - the project's own docs site (e.g. stormchecker.org). Blog posts only to find claims to verify.

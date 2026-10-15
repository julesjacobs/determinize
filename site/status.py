#!/usr/bin/env python3
"""Writes the status chips of the README to the branch `status`: for each part of the published
site, whether it is up to date, outdated, failing or being built.

    NEEDS='<toJSON(needs) of its job>' site/status.py [--building]

pages.yml runs it on main, in a checkout whose credentials push the branch: with --building as soon
as its `changes` job has hashed the inputs, and without when every other job has finished. The
chips are shields.io endpoint badges, <chip>.json, which the README loads through
img.shields.io/endpoint. At the end of a run, a part is
- failing when a job that builds, checks or publishes it failed, timed out or was cancelled;
- up to date when the site was published, or when what the part is built from is the same as when
  the site was published last;
- outdated otherwise: it changed, but a failure elsewhere kept the site from being published, as
  each deployment to GitHub Pages replaces the whole site.
At the start of a run, a part that changed since the site was published last is building; the
others keep their chips.

What a part is built from is compared by the hashes that the `changes` job outputs. deployed.json
records those of the site that was published last, with its commit. The branch holds one commit,
which each run replaces, so that it does not grow; before the first run, it does not exist.
"""
from __future__ import annotations

import argparse
import json
import os
import subprocess

BRANCH = "status"
BOT = ["-c", "user.name=github-actions[bot]",
       "-c", "user.email=41898282+github-actions[bot]@users.noreply.github.com"]

# Each chip by its file's name: its label, the jobs of pages.yml whose failure fails it, and the
# outputs of `changes` that hash what it is built from. The site is assembled, its links checked
# and published by `build` and `deploy`. The Lean tests gate the publication of the documentation
# of the code they test, and the archive includes that documentation.
CHIPS = {
    "site": ("Project page", ["site", "build", "deploy"], ["site"]),
    "sim": ("Simulator", ["sim"], ["sim"]),
    "docs": ("API documentation", ["lean", "docs"], ["docs"]),
    "paper": ("Paper (PDF)", ["paper"], ["tex"]),
    "artifact": ("Anonymized artifact (ZIP)", ["artifact"], ["artifact", "docs"]),
}
INPUTS = sorted({name for _, _, inputs in CHIPS.values() for name in inputs})

# The results of a job that did not finish its work; to `needs`, a job that timed out was
# cancelled. A skipped job either reused an earlier result or waited for one that failed, which
# fails its own chip.
FAILED = {"failure", "cancelled"}
STATES = {
    "failing": "red",
    "outdated": "orange",
    "building": "blue",
    "up to date": "brightgreen",
}


def chips(needs, previous, commit, building=False):
    """The files of the branch, by name, given the `needs` context of the job, the files that the
    branch held before, parsed, and the run's commit; with `building`, at the start of the run."""
    current = needs["changes"]["outputs"]
    deployed = previous.get("deployed.json") or {"commit": None, "inputs": {}}
    if not building and needs["deploy"]["result"] == "success":
        deployed = {"commit": commit, "inputs": {name: current[name] for name in INPUTS}}
    files = {"deployed.json": deployed}
    for chip, (label, jobs, inputs) in CHIPS.items():
        published = all(deployed["inputs"].get(name) == current[name] for name in inputs)
        if building:
            if published and f"{chip}.json" in previous:
                files[f"{chip}.json"] = previous[f"{chip}.json"]
                continue
            message = "building"
        elif any(needs[job]["result"] in FAILED for job in jobs):
            message = "failing"
        else:
            message = "up to date" if published else "outdated"
        files[f"{chip}.json"] = {"schemaVersion": 1, "label": label, "message": message,
                                 "color": STATES[message]}
    return files


def git(*args, input=None):
    return subprocess.run(["git", *args], input=input, stdout=subprocess.PIPE, text=True, check=True).stdout


def fetch():
    """The files of the branch on origin, parsed, by name; none before the first run."""
    found = subprocess.run(["git", "ls-remote", "--exit-code", "--heads", "origin", BRANCH],
                           stdout=subprocess.DEVNULL)
    if found.returncode == 2:
        return {}
    found.check_returncode()
    git("fetch", "--depth=1", "origin", BRANCH)
    return {name: json.loads(git("show", f"FETCH_HEAD:{name}"))
            for name in git("ls-tree", "--name-only", "FETCH_HEAD").splitlines()}


def push(files, message):
    """Replaces the branch on origin with one commit, without parents, of `files`."""
    entries = ""
    for name, content in sorted(files.items()):
        blob = git("hash-object", "-w", "--stdin", input=json.dumps(content, indent=2) + "\n").strip()
        entries += f"100644 blob {blob}\t{name}\n"
    commit = git(*BOT, "commit-tree", git("mktree", input=entries).strip(), "-m", message).strip()
    git("push", "--force", "origin", f"{commit}:refs/heads/{BRANCH}")


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--building", action="store_true", help="mark the parts that the run builds anew")
    args = parser.parse_args()
    commit = os.environ["GITHUB_SHA"]
    files = chips(json.loads(os.environ["NEEDS"]), fetch(), commit, args.building)
    for chip, (label, _, _) in CHIPS.items():
        print(f"{label}: {files[f'{chip}.json']['message']}")
    push(files, f"{'Building' if args.building else 'Status after'} {commit}")


if __name__ == "__main__":
    main()

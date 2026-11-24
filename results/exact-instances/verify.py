#!/usr/bin/env python3
"""Check the recorded files and their correspondence; does not run Lean or Storm."""

import hashlib
import json
import re
from pathlib import Path

ARTIFACT = Path(__file__).resolve().parent
ROOT = ARTIFACT.parents[1]


def sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def require(condition, message):
    if not condition:
        raise SystemExit(message)


manifest = json.loads((ARTIFACT / "manifest.json").read_text())
for filename, expected in manifest["files_sha256"].items():
    require(sha256(ARTIFACT / filename) == expected, f"Changed artifact: {filename}")

frontend = (ARTIFACT / "CheckFrontend.lean").read_text()
blocks = re.findall(r"namespace Case\d+\n(.*?)\nend Case\d+", frontend, re.DOTALL)
checked = 0
for row in manifest["results"]:
    name = row["benchmark"]
    source = ROOT / row["source"]
    instance = ARTIFACT / row["instance"]
    require(sha256(source) == row["source_sha256"], f"Changed source: {source}")
    require(sha256(instance) == row["instance_sha256"], f"Changed instance: {name}")
    expected = source.read_text().rstrip()
    if row["parameter_n"] is not None:
        expected += " " + str(row["parameter_n"]) + "\n"
    diagnostic = expected + "\n"
    require(hashlib.sha256(diagnostic.encode()).hexdigest() == row["diagnostic_instance_sha256"],
            f"Wrong diagnostic input: {name}")
    normalized = "\n".join(line.rstrip() for line in diagnostic.splitlines()).rstrip("\n") + "\n"
    require(instance.read_text() == normalized, f"Wrong normalized instance: {name}")
    if row["status"] == "state_limit":
        require(row["solve"]["exit_code"] != 0, f"Missing failure: {name}")
        continue
    require(row["status"] == "native_checked", f"Unknown status: {name}")
    theorem = (ARTIFACT / row["theorem"]).read_text()
    literal = next(line for line in theorem.splitlines() if line.startswith("def checkedSource :"))
    block, = [block for block in blocks if f'/instances/{name}"' in block]
    require(literal in block.splitlines(), f"Frontend/theorem mismatch: {name}")
    stats = row["output_statistics"]
    triple = ", ".join(stats[key] for key in ("return_mass", "first_moment", "second_moment"))
    require(f"⟨{triple}⟩ : OutputStatistics" in theorem, f"Wrong theorem statistics: {name}")
    log = row["native_check"]
    require(log["exit_code"] == 0 and "certified._native.native_decide.ax_" in log["stdout"]
            and "sorryAx" not in log["stdout"], f"Missing native check: {name}")
    solution = json.loads((ARTIFACT / row["solver_solution"]).read_text())
    require(len(solution["dead"]) == row["states"], f"Wrong state count: {name}")
    checked += 1

require(checked == manifest["frontend_check"]["instances_checked"] == len(blocks),
        "Wrong frontend check count")
print(f"Verified {len(manifest['results'])} source/instance pairs, {checked} theorem inputs, "
      f"and {len(manifest['files_sha256'])} artifact hashes.")

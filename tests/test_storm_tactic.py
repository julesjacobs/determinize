"""Storm tactic integration and rejection of corrupted external certificates."""
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

from test_export import ROOT, LEAN, BIN


HEADER = """import Determinize.Tactic.Storm
open Determinize.Spec.Paper Determinize.Spec.FiniteModel
set_option maxHeartbeats 0
set_option Elab.async false
def program : Core := .bernoulli (.sample .G) (.real (1/3))
"""
CLAIM = """theorem certified : (⟨1, 1/3, 1/3⟩ : OutputStatistics).Matches
    (bigStepMeasure (Subject.source.program program)) := by storm
#print axioms certified
"""


@unittest.skipUnless(os.environ.get("STORM_PYTHON"), "set STORM_PYTHON for real Storm integration")
class StormTacticTests(unittest.TestCase):
    def check(self, path, *args, env=None):
        return subprocess.run(["lake", "env", "lean", *args, path], cwd=LEAN,
                              text=True, capture_output=True, timeout=180, env=env)

    def assert_checked(self, result, native=True):
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("Quot.sound", result.stdout)
        for forbidden in ("sorryAx", "ofReduceBool", "trustCompiler"):
            self.assertNotIn(forbidden, result.stdout + result.stderr)
        if native:
            self.assertIn("_native.native_decide.ax", result.stdout)
        else:
            self.assertNotIn("_native.native_decide.ax", result.stdout)

    def test_kernel_option(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "kernel.lean"
            path.write_text(HEADER + "set_option determinize.storm.native false\n" + CLAIM)
            self.assert_checked(self.check(path), native=False)

    def test_statistics_and_conditional_moments(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "moments.lean"
            path.write_text(HEADER + CLAIM + """
theorem mean : Determinize.Spec.returnedExpectation (Subject.source.program program) =
    ((1/3 : Rat) : ℝ) := by storm
theorem variance : Determinize.Spec.returnedVariance (Subject.source.program program) =
    ((2/9 : Rat) : ℝ) := by storm
#print axioms mean
#print axioms variance
""")
            self.assert_checked(self.check(path))

    def test_rejection_divergence_and_additive_recursion(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            text = "import Determinize.Tactic.Storm\n" \
                   "open Determinize.Spec.Paper Determinize.Spec.FiniteModel\n" \
                   "set_option maxHeartbeats 0\nset_option Elab.async false\n"
            for i, (program, statistics) in enumerate([
                ("let _ = observe(flip(0.5)) in 3", "1/2, 3/2, 9/2"),
                ("let f = rec f u => f u in f ()", "0, 0, 0"),
                ("let f = rec f u => if flip(0.5) then 0 else 1 + f u in f ()", "1, 1, 3"),
                ("if flip(0.5) then 8 else -4", "1, 2, 40"),
            ]):
                source, prefix = directory / "input.det", directory / "model"
                source.write_text(program)
                exported = subprocess.run([BIN, "--check", "--export", prefix,
                    "--subject", "source", source], cwd=LEAN,
                    text=True, capture_output=True, timeout=60)
                self.assertEqual(exported.returncode, 0, exported.stderr)
                literal = next(line for line in Path(str(prefix) + ".replay.lean").read_text().splitlines()
                               if line.startswith("def checkedSource :"))
                text += literal.replace("checkedSource", f"program{i}") + "\n"
                text += f"theorem case{i} : (⟨{statistics}⟩ : OutputStatistics).Matches\n" \
                        f"  (bigStepMeasure (Subject.source.program program{i})) := by storm\n" \
                        f"#print axioms case{i}\n"
            path = directory / "cases.lean"
            path.write_text(text)
            self.assert_checked(self.check(path))

    def test_wrong_goal(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "wrong.lean"
            path.write_text(HEADER + CLAIM.replace("⟨1, 1/3, 1/3⟩", "⟨1, 0, 1/3⟩"))
            result = self.check(path)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("certified result does not match", result.stdout + result.stderr)

    def test_corrupted_worker_output(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            worker = directory / "worker.py"
            for mutation in [
                'data["values"]["first"] = ["123"] * len(data["dead"])',
                'data["values"]["second"] = ["123"] * len(data["dead"])',
                'data["dead"] = [True] * len(data["dead"])',
                'data["rank"] = [0] * len(data["dead"])',
                'data["next"] = list(range(len(data["dead"])))',
                'data["values"]["mass"].pop()',
                'data["values"]["mass"][0] = "1/0"',
            ]:
                with self.subTest(mutation=mutation):
                    worker.write_text(f"""import json, subprocess, sys
from pathlib import Path
subprocess.run([sys.executable, {str(ROOT / 'tools/storm.py')!r},
                "--worker", sys.argv[2], "--additive"], check=True)
path = Path(sys.argv[2] + ".storm-values.json")
data = json.loads(path.read_text())
{mutation}
path.write_text(json.dumps(data))
""")
                    path = directory / "corrupted.lean"
                    path.write_text(HEADER + f"set_option determinize.storm.worker {json.dumps(str(worker))}\n" + CLAIM)
                    result = self.check(path)
                    self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_import_compiled_proof_without_storm(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            path = directory / "Certified.lean"
            path.write_text(HEADER + CLAIM)
            self.assert_checked(self.check(path, "--root", str(directory), "-o", str(directory / "Certified.olean")))
            lean_path = subprocess.check_output(["lake", "env", "printenv", "LEAN_PATH"],
                                               cwd=LEAN, text=True).strip()
            imported = directory / "Imported.lean"
            imported.write_text("import Certified\n#print axioms certified\n")
            env = {**os.environ, "STORM_PYTHON": "/does/not/exist"}
            result = subprocess.run(["lake", "env", "env", f"LEAN_PATH={directory}:{lean_path}",
                "lean", imported], cwd=LEAN, env=env, text=True, capture_output=True, timeout=60)
            self.assert_checked(result)


if __name__ == "__main__":
    unittest.main()

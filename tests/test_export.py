"""Integration tests for finite-model exports and paper correspondence certificates."""
from fractions import Fraction
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
LEAN = ROOT / "lean"
BIN = LEAN / ".lake/build/bin/determinize"


def solve_export(path):
    transitions = {}
    for line in path.with_suffix(".tra").read_text().splitlines()[1:]:
        source, target, probability = line.split()
        transitions.setdefault(int(source), []).append((int(target), Fraction(probability)))
    size = len(transitions)
    rewards = [Fraction(0)] * size
    for suffix, sign in [(".positive.state.rew", 1), (".negative.state.rew", -1)]:
        for line in Path(str(path) + suffix).read_text().splitlines():
            state, reward = line.split()
            rewards[int(state)] += sign * Fraction(reward)
    matrix = []
    for i in range(size):
        row = [Fraction(i == j) for j in range(size)]
        if i != size - 1:
            for target, probability in transitions[i]:
                row[target] -= probability
        matrix.append(row + [rewards[i]])
    for column in range(size):
        pivot = next(i for i in range(column, size) if matrix[i][column])
        matrix[column], matrix[pivot] = matrix[pivot], matrix[column]
        factor = matrix[column][column]
        matrix[column] = [x / factor for x in matrix[column]]
        for i in range(size):
            if i != column:
                factor = matrix[i][column]
                matrix[i] = [x - factor*y for x, y in zip(matrix[i], matrix[column])]
    return matrix[0][-1]


class ExportTests(unittest.TestCase):
    def run_export(self, directory, program, *options):
        source = directory / "input.det"
        source.write_text(program)
        output = directory / "model"
        result = subprocess.run(
            [str(BIN), "--check", "--export", str(output), *options, str(source)],
            cwd=LEAN, text=True, capture_output=True, timeout=60)
        return result, output

    def test_ground_truth(self):
        for program, subject, expected in [
            ("if flip(0.25) then 8 else -4", "source", -1),
            ("let _ = observe(flip(0.5)) in 3", "source", Fraction(3, 2)),
            ("let f = rec f x => if flip(0.5) then 3 else f x in f 0", "source", 3),
            ("bernoulli[E](0.3)", "source", Fraction(3, 10)),
            ("bernoulli[E](0.3)", "determinized", Fraction(3, 10)),
            ("discrete[G](0,0.25,0,0.75)", "source", Fraction(5, 2)),
            ("-3", "source", -3),
        ]:
            with self.subTest(program=program, subject=subject), tempfile.TemporaryDirectory() as tmp:
                result, output = self.run_export(Path(tmp), program, "--subject", subject)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn("paper correspondence checked", result.stdout)
                self.assertEqual(solve_export(output), expected)

    def test_generated_lean_data(self):
        for program in [
            "discrete[G](0,0.25,0,0.75)",
            "let x = 2 in (fun y => x + y) (bernoulli[G](0.3))",
            "let f = rec f x => if flip(0.5) then -3 else f x in f 0",
        ]:
            with self.subTest(program=program), tempfile.TemporaryDirectory() as tmp:
                result, output = self.run_export(Path(tmp), program, "--subject", "source")
                self.assertEqual(result.returncode, 0, result.stderr)
                candidate = Path(str(output) + ".replay.lean")
                with candidate.open("a") as stream:
                    stream.write("""
def checkRoundTrip : IO Unit := do
  match explore checkedSource checkedSubject with
  | .complete replay =>
      unless replay.states == candidate.states &&
          reprStr replay.rows == reprStr candidate.rows &&
          replay.initial == candidate.initial do
        throw (IO.userError "candidate round trip changed data")
  | _ => throw (IO.userError "candidate did not replay")
#eval checkRoundTrip
""")
                checked = subprocess.run(["lake", "env", "lean", str(candidate)],
                                         cwd=LEAN, text=True, capture_output=True, timeout=120)
                self.assertEqual(checked.returncode, 0, checked.stdout + checked.stderr)

    def test_kernel_model_replay(self):
        for program, subject in [
            ("discrete[G](0,0.25,0,0.75)", "source"),
            ("let x = 2 in (fun y => x + y) (bernoulli[G](0.3))", "source"),
            ("let f = rec f x => if flip(0.5) then -3 else f x in f 0", "source"),
            ("let p = (bernoulli[G](0.25), 3) in fst p + snd p", "source"),
            ("match 2::[] with [] => 0 | x::xs => x", "source"),
            ("let f = rec f x => f x in f 0", "source"),
            ("uniform[E](0,3)", "determinized"),
        ]:
            with self.subTest(program=program, subject=subject), tempfile.TemporaryDirectory() as tmp:
                result, output = self.run_export(Path(tmp), program, "--subject", subject)
                self.assertEqual(result.returncode, 0, result.stderr)
                certificate = Path(str(output) + ".replay.lean")
                checked = subprocess.run(["lake", "env", "lean", str(certificate)],
                                         cwd=LEAN, text=True, capture_output=True, timeout=120)
                self.assertEqual(checked.returncode, 0, checked.stdout + checked.stderr)
                self.assertIn("machineReplay", checked.stdout)
                self.assertIn("modelMatches", checked.stdout)
                for forbidden in ["sorryAx", "ofReduceBool", "trustCompiler"]:
                    self.assertNotIn(forbidden, checked.stdout)
                changed = certificate.read_text().replace("  initial := 0", "  initial := 1", 1)
                self.assertNotEqual(changed, certificate.read_text())
                certificate.write_text(changed)
                rejected = subprocess.run(["lake", "env", "lean", str(certificate)],
                                          cwd=LEAN, text=True, capture_output=True, timeout=120)
                self.assertNotEqual(rejected.returncode, 0)
                self.assertIn("error", rejected.stdout + rejected.stderr)


    def test_no_export_on_failure(self):
        for program, options, message in [
            ("3", ["--max-states", "0"], "Incomplete"),
            ("3", ["--max-edges", "0"], "Incomplete"),
            ("3", ["--max-state-bytes", "0"], "Incomplete"),
            ("uniform[G](0,1)", [], "unsupported"),
            ("true", [], "numeric"),
        ]:
            with self.subTest(program=program, options=options), tempfile.TemporaryDirectory() as tmp:
                result, _ = self.run_export(Path(tmp), program, *options)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn(message, result.stderr)
                self.assertEqual(sorted(p.name for p in Path(tmp).iterdir()), ["input.det"])


if __name__ == "__main__":
    unittest.main()

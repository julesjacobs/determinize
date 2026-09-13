"""Exact result certificates, independent kernel replay, and optional real Storm runs."""
from fractions import Fraction
import json
import importlib.util
from types import SimpleNamespace
from unittest.mock import patch
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

from test_export import ROOT, LEAN, BIN, solve_export

CASES = [
    ("let p = bernoulli[E](0.5) in discrete[E](p/2,0.25,*)", "source", Fraction(5, 4)),
    ("let p = bernoulli[E](0.5) in discrete[E](p/2,0.25,*)", "determinized", Fraction(5, 4)),
    ("let ps = if flip(0.5) then [] else 0::[] in discrete_list[E](ps)", "source", Fraction(1, 2)),
    ("if flip(0.25) then 8 else -4", "source", Fraction(-1)),
    ("let _ = observe(flip(0.5)) in 3", "source", Fraction(3, 2)),
    ("let f = rec f x => if flip(0.5) then -3 else f x in f 0", "source", Fraction(-3)),
    ("discrete[G](0,0.25,0,0.75)", "source", Fraction(5, 2)),
    ("uniform[E](0,3)", "determinized", Fraction(3, 2)),
    ("-7/3", "source", Fraction(-7, 3)),
    ("let _ = observe(false) in 3", "source", Fraction(0)),
    ("if flip(0.25) then 1 else 0", "source", Fraction(1, 4)),
]


def generate(directory, program, subject="source", *options):
    source = directory / "input.det"
    source.write_text(program)
    prefix = directory / "model"
    result = subprocess.run([BIN, "--check", "--result", prefix, "--subject", subject,
                             *options, source], text=True, capture_output=True, timeout=60)
    return result, prefix


def kernel(path):
    return subprocess.run(["lake", "env", "lean", path], cwd=LEAN, text=True,
                          capture_output=True, timeout=180)


class ResultTests(unittest.TestCase):
    def test_exact_ground_truth(self):
        for program, subject, expected in CASES:
            with self.subTest(program=program), tempfile.TemporaryDirectory() as tmp:
                result, prefix = generate(Path(tmp), program, subject)
                self.assertEqual(result.returncode, 0, result.stderr)
                data = json.loads(Path(str(prefix) + ".result.json").read_text())
                self.assertEqual(Fraction(data["answer"]), expected)
                self.assertEqual(solve_export(prefix), expected)
                self.assertEqual(data["subject"], subject)
                self.assertGreaterEqual(data["rank_bound"], 0)

    def test_kernel_results_and_tampering(self):
        for program, subject, _ in CASES[:8]:
            with self.subTest(program=program), tempfile.TemporaryDirectory() as tmp:
                result, prefix = generate(Path(tmp), program, subject)
                self.assertEqual(result.returncode, 0, result.stderr)
                certificate = Path(str(prefix) + ".result.lean")
                checked = kernel(certificate)
                self.assertEqual(checked.returncode, 0, checked.stdout + checked.stderr)
                self.assertIn("expectedReward", checked.stdout)
                for forbidden in ("sorryAx", "ofReduceBool", "trustCompiler"):
                    self.assertNotIn(forbidden, checked.stdout)
                text = certificate.read_text()
                start = text.index("  values :=", text.index("def result :"))
                end = text.index("\n\n", start)
                certificate.write_text(text[:start] + "  values := fun _ _ => 12345" + text[end:])
                self.assertNotEqual(kernel(certificate).returncode, 0)
                start = text.index("  rank :=", text.index("def result :"))
                end = text.index("\n", start)
                certificate.write_text(text[:start] + "  rank := fun _ => 0" + text[end:])
                self.assertNotEqual(kernel(certificate).returncode, 0)

    def test_divergence_and_conditional_moments(self):
        cases = [
            ("let f = rec f x => f x in f 0", ("0", "0", "0", None, None)),
            ("let f = rec f x => f x in if flip(0.5) then f 0 else if flip(0.5) then -2 else 4",
             ("1/2", "1/2", "5", "1", "9")),
            ("let _ = observe(flip(0.5)) in if flip(0.5) then -2 else 4",
             ("1/2", "1/2", "5", "1", "9")),
        ]
        for program, expected in cases:
            with self.subTest(program=program), tempfile.TemporaryDirectory() as tmp:
                result, prefix = generate(Path(tmp), program)
                self.assertEqual(result.returncode, 0, result.stderr)
                data = json.loads(Path(str(prefix) + ".result.json").read_text())
                self.assertEqual(tuple(data[key] for key in
                    ("return_mass", "answer", "second_moment", "conditional_mean", "conditional_variance")), expected)
                checked = kernel(Path(str(prefix) + ".result.lean"))
                self.assertEqual(checked.returncode, 0, checked.stdout + checked.stderr)

    def test_termination_probabilities(self):
        cases = [
            ("let f = rec f x => f x in f 0", ("0", "0", "1")),
            ("let _ = observe(false) in 3", ("0", "1", "0")),
            ("let f = rec f x => f x in if flip(0.5) then f 0 else let _ = observe(flip(0.5)) in 1",
             ("1/4", "1/4", "1/2")),
        ]
        for program, expected in cases:
            with self.subTest(program=program), tempfile.TemporaryDirectory() as tmp:
                result, prefix = generate(Path(tmp), program)
                self.assertEqual(result.returncode, 0, result.stderr)
                data = json.loads(Path(str(prefix) + ".result.json").read_text())
                self.assertEqual(tuple(data[key] for key in
                    ("return_mass", "rejection_probability", "divergence_probability")), expected)
                checked = kernel(Path(str(prefix) + ".result.lean"))
                self.assertEqual(checked.returncode, 0, checked.stdout + checked.stderr)

    def test_failure_preserves_outputs(self):
        for program, options, error in [
            ("1/0", [], "division"),
            ("if flip(0.5) then 2 else 3", ["--max-result-states", "1"], "state limit"),
            ("uniform[G](0,1)", [], "unsupported"),
        ]:
            with self.subTest(program=program), tempfile.TemporaryDirectory() as tmp:
                directory = Path(tmp)
                old = directory / "model.result.lean"
                old.write_text("existing certificate")
                result, prefix = generate(directory, program, "source", *options)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn(error, result.stderr.lower())
                self.assertEqual(old.read_text(), "existing certificate")
                self.assertFalse(Path(str(prefix) + ".result.json").exists())
                self.assertFalse(Path(str(prefix) + ".tra").exists())

    def adapter(self):
        spec = importlib.util.spec_from_file_location("storm_adapter", ROOT / "tools/storm.py")
        adapter = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(adapter)
        return adapter

    def test_storm_failure_reports(self):
        adapter = self.adapter()
        for failure in ("timeout", "storm", "kernel", "disagreement"):
            with self.subTest(failure=failure), tempfile.TemporaryDirectory() as tmp:
                prefix = Path(tmp) / "model"
                args = SimpleNamespace(prefix=prefix, subject="source", timeout=1,
                                       binary=BIN, compare=True, file=Path(tmp) / "input.det")
                calls = []

                def command(argv, **kwargs):
                    calls.append(argv)
                    if failure == "timeout":
                        raise subprocess.TimeoutExpired(argv, 1)
                    stage = len(calls)
                    if (stage == 2 and failure == "storm") or (stage == 3 and failure == "kernel"):
                        return subprocess.CompletedProcess(argv, 1, failure + " rejected", "")
                    if stage == 2:
                        Path(str(prefix) + ".storm-values.json").write_text(
                            '{"storm_version": "test", "storm_build_type": "test"}')
                    if stage == 4:
                        Path(str(prefix) + ".result.json").write_text('{"answer": "1/3"}')
                    output = "\n".join(f"'{name}' depends on axioms: [propext, Classical.choice, Quot.sound]"
                                       for name in ("outputStatistics", "terminationProbabilities", "conditionalVariance"))
                    return subprocess.CompletedProcess(argv, 0, output, "")

                with patch.object(adapter.importlib.metadata, "version", return_value="test"), \
                     patch.object(adapter, "run_command", side_effect=command), \
                     patch.object(adapter, "certificate_text", return_value=("proof", {
                         "mass": Fraction(1), "first": Fraction(1,2), "second": Fraction(1,4),
                         "rejection": Fraction(0)})), patch("builtins.print"):
                    self.assertEqual(adapter.run(args), 1)
                report = json.loads(Path(str(prefix) + ".storm.json").read_text())
                self.assertEqual(report["status"], "failed")
                self.assertEqual(len(calls), {"timeout": 1, "storm": 2, "kernel": 3, "disagreement": 4}[failure])
                if failure == "disagreement":
                    self.assertIn("disagrees", report["error"])
                    self.assertTrue(report["kernel_checked"])
                else:
                    self.assertNotIn("kernel_checked", report)

    def test_certificate_axioms(self):
        adapter = self.adapter()
        output = "\n".join(f"'{name}' depends on axioms: [propext, Classical.choice, Quot.sound]"
                           for name in ("outputStatistics", "terminationProbabilities", "conditionalVariance"))
        self.assertEqual(len(adapter.checked_axioms(output)), 3)
        for invalid in ("", output.replace("Quot.sound", "sorryAx"),
                        output.replace("Quot.sound", "Lean.ofReduceBool"),
                        output.replace("Quot.sound", "unprovedClaim")):
            with self.assertRaises(ValueError):
                adapter.checked_axioms(invalid)

    def test_storm_timeout_stops_children(self):
        adapter = self.adapter()
        with tempfile.TemporaryDirectory() as tmp:
            pidfile = Path(tmp) / "child.pid"
            script = ("import subprocess, sys, time; "
                      "from pathlib import Path; "
                      "p=subprocess.Popen([sys.executable, '-c', 'import time; time.sleep(30)']); "
                      f"Path({str(pidfile)!r}).write_text(str(p.pid)); time.sleep(30)")
            with self.assertRaises(subprocess.TimeoutExpired):
                adapter.run_command([sys.executable, "-c", script], cwd=ROOT, timeout=1)
            child = int(pidfile.read_text())
            state = subprocess.run(["ps", "-p", str(child), "-o", "stat="],
                                   text=True, capture_output=True).stdout.strip()
            self.assertTrue(not state or state.startswith("Z"), state)

    def test_storm_certificate_validation(self):
        adapter = self.adapter()
        with tempfile.TemporaryDirectory() as tmp:
            result, prefix = generate(Path(tmp), "if flip(0.5) then 1 else 2")
            self.assertEqual(result.returncode, 0, result.stderr)
            size, edges, labels, _ = adapter.read_model(prefix)
            dead, rank, following = adapter.boundary(size, edges, labels)
            valid = {"dead": [i in dead for i in range(size-1)], "rank": rank, "next": following,
                     "values": {name: ["0"]*(size-1) for name in ("mass", "first", "second", "rejection")}}
            for key, invalid in [("dead", [False]), ("rank", [-1]*(size-1)),
                                 ("next", [size-1]*(size-1)), ("values", {})]:
                with self.subTest(key=key), self.assertRaises(ValueError):
                    adapter.certificate_text(prefix, {**valid, key: invalid})
            for bad in ("inf", "nan", "1/0"):
                with self.subTest(bad=bad), self.assertRaises((ValueError, ZeroDivisionError)):
                    adapter.certificate_text(prefix, {**valid, "values": {
                        **valid["values"], "mass": [bad]*(size-1)}})
            certificate, _ = adapter.certificate_text(prefix, valid)
            path = Path(tmp) / "wrong.lean"
            path.write_text(certificate)
            self.assertNotEqual(kernel(path).returncode, 0)

    @unittest.skipUnless(os.environ.get("STORM_PYTHON"), "set STORM_PYTHON for real Storm integration")
    def test_storm_above_dense_limit(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            source = directory / "input.det"
            source.write_text("let f = rec f x => if x <= 0 then 7 else f (x-1) in f 9")
            prefix = directory / "model"
            result = subprocess.run([os.environ["STORM_PYTHON"], ROOT / "tools/storm.py",
                                     source, "--prefix", prefix, "--subject", "source", "--timeout", "300"],
                                    text=True, capture_output=True, timeout=360)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            report = json.loads(Path(str(prefix) + ".storm.json").read_text())
            self.assertTrue(report["kernel_checked"])
            self.assertEqual(Fraction(report["exact_answer"]), 7)
            self.assertGreater(self.adapter().read_model(prefix)[0]-1, 256)
            self.assertFalse(Path(str(prefix) + ".result.json").exists())
            self.assertTrue(all("--result" not in command for command in report["commands"]))

    @unittest.skipUnless(os.environ.get("STORM_PYTHON"), "set STORM_PYTHON for real Storm integration")
    def test_storm(self):
        for program, subject, expected in CASES + [
            ("let f = rec f x => f x in f 0", "source", Fraction(0)),
            ("let f = rec f x => f x in if flip(0.5) then f 0 else 2", "source", Fraction(1)),
        ]:
            with self.subTest(program=program), tempfile.TemporaryDirectory() as tmp:
                directory = Path(tmp)
                source = directory / "input.det"
                source.write_text(program)
                prefix = directory / "model"
                result = subprocess.run([os.environ["STORM_PYTHON"], ROOT / "tools/storm.py",
                                         source, "--prefix", prefix, "--subject", subject],
                                        text=True, capture_output=True, timeout=240)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                report = json.loads(Path(str(prefix) + ".storm.json").read_text())
                self.assertEqual(report["status"], "completed")
                self.assertTrue(report["kernel_checked"])
                self.assertEqual(Fraction(report["exact_answer"]), expected)
                self.assertTrue(report["stormpy_version"])
                self.assertTrue(report["storm_version"])


if __name__ == "__main__":
    unittest.main()

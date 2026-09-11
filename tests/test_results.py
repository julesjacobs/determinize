"""Exact result certificates, independent kernel replay, and optional real Storm runs."""
from fractions import Fraction
import json
import importlib.util
from types import SimpleNamespace
from unittest.mock import patch
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

from test_export import ROOT, LEAN, BIN, solve_export

CASES = [
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
                self.assertGreater(data["horizon"], 0)
                self.assertGreater(Fraction(data["escape"]), 0)

    def test_kernel_results_and_tampering(self):
        for program, subject, _ in CASES[:5]:
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
                end = text.index("\n", start)
                certificate.write_text(text[:start] + "  values := fun _ => 12345" + text[end:])
                self.assertNotEqual(kernel(certificate).returncode, 0)
                start = text.index("  escape :=", text.index("def result :"))
                end = text.index("\n", start)
                certificate.write_text(text[:start] + "  escape := 0" + text[end:])
                self.assertNotEqual(kernel(certificate).returncode, 0)

    def test_failure_preserves_outputs(self):
        for program, options, error in [
            ("let f = rec f x => f x in f 0", [], "singular"),
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

    def test_storm_failure_reports(self):
        spec = importlib.util.spec_from_file_location("storm_adapter", ROOT / "tools/storm.py")
        adapter = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(adapter)
        for failure in ("timeout", "kernel", "disagreement"):
            with self.subTest(failure=failure), tempfile.TemporaryDirectory() as tmp:
                prefix = Path(tmp) / "model"
                args = SimpleNamespace(prefix=prefix, subject="source", timeout=1,
                                       binary=BIN, max_result_states=256, file=Path(tmp) / "input.det")
                calls = []

                def command(argv, **kwargs):
                    calls.append(argv)
                    if failure == "timeout":
                        raise subprocess.TimeoutExpired(argv, 1)
                    if len(calls) == 2 and failure == "kernel":
                        return subprocess.CompletedProcess(argv, 1, "kernel rejected", "")
                    if len(calls) == 3:
                        Path(str(prefix) + ".result.json").write_text('{"answer": "1/3"}')
                        Path(str(prefix) + ".storm-values.json").write_text(
                            '{"positive": "1/2", "negative": "0"}')
                    return subprocess.CompletedProcess(argv, 0, "", "")

                with patch.object(adapter.importlib.metadata, "version", return_value="test"), \
                     patch.object(adapter.subprocess, "run", side_effect=command), \
                     patch("builtins.print"):
                    self.assertEqual(adapter.run(args), 1)
                report = json.loads(Path(str(prefix) + ".storm.json").read_text())
                self.assertEqual(report["status"], "failed")
                self.assertEqual(len(calls), {"timeout": 1, "kernel": 2, "disagreement": 3}[failure])
                if failure == "disagreement":
                    self.assertIn("disagrees", report["error"])
                else:
                    self.assertNotIn("kernel_checked", report)

    @unittest.skipUnless(os.environ.get("STORM_PYTHON"), "set STORM_PYTHON for real Storm integration")
    def test_storm(self):
        for program, subject, expected in CASES:
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

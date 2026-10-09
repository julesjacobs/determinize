"""Real Storm runs of tools/storm.py: its kernel-checked answers against the exact ones, above the
dense limit too, and a certificate whose initial state is tampered with. They run when
STORM_PYTHON names a Python with stormpy; test_results.py checks the adapter without Storm."""
from fractions import Fraction
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

from test_export import ROOT
from test_results import CASES, kernel, storm_adapter


class StormTests(unittest.TestCase):
    @unittest.skipUnless(os.environ.get("STORM_PYTHON"), "set STORM_PYTHON for real Storm integration")
    def test_storm_report_initial_label_tampering(self):
        adapter = storm_adapter()
        for additive in (False, True):
            with self.subTest(additive=additive), tempfile.TemporaryDirectory() as tmp:
                directory = Path(tmp)
                source = directory / "input.det"
                source.write_text("bernoulli[G](0.5)")
                prefix = directory / "model"
                mode = ["--additive"] if additive else []
                run = subprocess.run([os.environ["STORM_PYTHON"], ROOT / "tools/storm.py",
                    source, "--prefix", prefix, "--subject", "source", *mode],
                    text=True, capture_output=True, timeout=180)
                self.assertEqual(run.returncode, 0, run.stdout + run.stderr)
                result = json.loads(Path(str(prefix) + ".storm-values.json").read_text())
                with patch.object(sys, "path", [str(ROOT / "tools"), *sys.path]):
                    original, answer = adapter.certificate_text(prefix, result, additive)
                self.assertEqual(answer["first"], Fraction(1, 2))
                _, _, labels, rewards = adapter.read_model(prefix)
                changed_initial = next(i for i in labels["returned"] if rewards[i] == 1)
                path = Path(str(prefix) + ".lab")
                lines = path.read_text().splitlines()
                changed = lines[:3]
                for line in lines[3:]:
                    state, *names = line.split()
                    names = [name for name in names if name != "init"]
                    if int(state) == changed_initial:
                        names.append("init")
                    changed.append(" ".join([state, *names]))
                path.write_text("\n".join(changed) + "\n")
                with patch.object(sys, "path", [str(ROOT / "tools"), *sys.path]):
                    tampered, answer = adapter.certificate_text(prefix, result, additive)
                self.assertEqual(answer["first"], 1)
                self.assertNotEqual(original, tampered)
                certificate = directory / "tampered.lean"
                certificate.write_text(tampered)
                checked = kernel(certificate)
                self.assertNotEqual(checked.returncode, 0)
                self.assertIn("error", checked.stdout + checked.stderr)


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
            self.assertGreater(storm_adapter().read_model(prefix)[0]-1, 256)
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

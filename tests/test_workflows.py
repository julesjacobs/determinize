"""Public script entry points use Lean and preserve working-directory arguments."""
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]


class WorkflowTests(unittest.TestCase):
    def test_run_from_another_directory(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            (directory / "input.det").write_text("2 + 3")
            result = subprocess.run([ROOT / "run.sh", "--result", "model", "input.det"],
                                    cwd=directory, text=True, capture_output=True, timeout=120)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn("Certified expected terminal reward", result.stdout)
            self.assertTrue((directory / "model.result.lean").is_file())
            self.assertFalse((directory / "input.det.dout").exists())
            (directory / "bad.det").write_text("let x =")
            result = subprocess.run([ROOT / "run.sh", "--check", "bad.det"],
                                    cwd=directory, text=True, capture_output=True, timeout=120)
            self.assertNotEqual(result.returncode, 0)

    def test_storm_entry_point(self):
        result = subprocess.run([ROOT / "run.sh", "--storm", "--help"],
                                text=True, capture_output=True, timeout=120)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("--prefix", result.stdout)
        self.assertIn("--subject", result.stdout)


if __name__ == "__main__":
    unittest.main()

"""Sampling benchmarks reject incomplete executions without treating observations as failures."""
import argparse
from contextlib import redirect_stdout
import io
from pathlib import Path
import sys
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "tools"))
import bench


class SamplingBenchmarkTests(unittest.TestCase):
    def sample(self, source="", target="", run=None):
        args = argparse.Namespace(binary=Path("determinize"), samples=10, seed=0,
                                  fuel=100, timeout=30)
        output = io.StringIO()
        if run is None:
            run = bench.ToolRun(0, source + target, "", 1.25)
        with patch.object(bench, "count_sample_sites", return_value=bench.SampleSites(1, 1, 1, 0)), \
             patch.object(bench, "run_tool", return_value=run), redirect_stdout(output):
            result = bench.det_approx(Path("input.det"), args)
        return result, output.getvalue()

    def test_execution_failure(self):
        for label in ("Source", "Determinized"):
            with self.subTest(label=label):
                summaries = {
                    name: (f"{name}: {9 if name == label else 10}/10 runs returned a value\n"
                           "  empirical mean among returned values: 5; variance: 2\n"
                           + ("  first failure: step limit reached\n" if name == label else ""))
                    for name in ("Source", "Determinized")
                }
                result, output = self.sample(summaries["Source"], summaries["Determinized"])
                self.assertEqual(result["status"], "failed")
                self.assertEqual(result["stage"], "sampling")
                self.assertIn(f"{label}: 1/10 executions failed", result["error"])
                self.assertIn("first failure: step limit reached", result["error"])
                self.assertIsNone(result["variance_reduction_factor"])
                self.assertIn(f"ERROR: {label}: 1/10 executions failed", output)
                self.assertIn("Variance reduction: n/a (sampling failed)", output)
                failed = result["source" if label == "Source" else "determinized"]
                self.assertEqual(failed["returned"], 9)
                self.assertEqual(failed["requested"], 10)
                self.assertEqual(failed["failed"], 1)
                self.assertEqual(failed["first_failure"], "step limit reached")

    def test_sampling_timeout(self):
        result, output = self.sample(run=bench.ToolRun(None, "", "", 30))
        self.assertEqual(result["status"], "failed")
        self.assertEqual(result["stage"], "sampling")
        self.assertIsNone(result.get("variance_reduction_factor"))
        self.assertIn("ERROR: sampling timed out after 30s", output)

    def test_incomplete_counts(self):
        for count in ("9/10", "9/9"):
            with self.subTest(count=count):
                result, _ = self.sample(
                    f"Source: {count} runs returned a value\n"
                    "  empirical mean among returned values: 5; variance: 2\n",
                    "Determinized: 10/10 runs returned a value\n"
                    "  empirical mean among returned values: 5; variance: 1\n")
                self.assertEqual(result["status"], "failed")
                self.assertIsNone(result["variance_reduction_factor"])

    def test_rejected_observations(self):
        result, _ = self.sample(
            "Source: 7/10 runs returned a value\n"
            "  empirical mean among returned values: 5; variance: 2\n"
            "  rejected observations: 3\n",
            "Determinized: 8/10 runs returned a value\n"
            "  empirical mean among returned values: 5; variance: 1\n"
            "  rejected observations: 2\n")
        self.assertEqual(result["status"], "completed")
        self.assertEqual(result["source"]["rejected"], 3)
        self.assertEqual(result["determinized"]["rejected"], 2)
        self.assertEqual(result["variance_reduction_factor"], 2)

    def test_all_runs_return(self):
        result, output = self.sample(
            "Source: 10/10 runs returned a value\n"
            "  empirical mean among returned values: 5; variance: 2\n",
            "Determinized: 10/10 runs returned a value\n"
            "  empirical mean among returned values: 5; variance: 0\n")
        self.assertEqual(result["status"], "completed")
        self.assertEqual(result["variance_reduction_factor"], "infinite")
        self.assertIn("Variance reduction: infinite", output)
        self.assertEqual(result["sampling_seconds"], 1.25)


if __name__ == "__main__":
    unittest.main()

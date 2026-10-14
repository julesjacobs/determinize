import math
import json
import os
from pathlib import Path
import struct
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "tools"))
import benchmark_estimator_variance as bench


def bits(value):
    return int.from_bytes(struct.pack("!d", value), "big")


class EstimatorVarianceTests(unittest.TestCase):
    def test_seed_ranges_are_disjoint_and_subjects_paired(self):
        rows = list(bench.trials(["a", "b"], [2, 3], 2))
        previous_end = 0
        for a, b in zip(rows[::2], rows[1::2]):
            self.assertEqual(a["seed"], b["seed"])
            self.assertEqual(a["seed"], previous_end)
            self.assertNotEqual(a["subject"], b["subject"])
            previous_end += a["samples"]
        self.assertEqual(rows[0]["subject"], "source")
        self.assertEqual(rows[2]["subject"], "determinized")

    def test_exact_float_round_trip_and_rejections(self):
        measured = {"requested": 10, "returned": 7, "rejected": 3, "failed": 0,
                    "first_failure": "", "sampling_ns": 1234567,
                    "mean_bits": bits(math.pi), "m2_bits": bits(14.)}
        row = bench.validate_measurement(measured, 10)
        self.assertEqual(row["estimate"], math.pi)
        self.assertEqual(row["output_variance"], 2)
        for update in ({"failed": 1}, {"first_failure": "step limit reached"},
                       {"returned": 6}, {"requested": 9}, {"mean_bits": bits(math.inf)},
                       {"m2_bits": bits(-1.)}):
            with self.subTest(update=update), self.assertRaises(ValueError):
                bench.validate_measurement({**measured, **update}, 10)

    def test_estimator_variance_uses_independent_replicate_means(self):
        rows = [{"benchmark": "a", "samples": 10, "subject": "source",
                 "replicate": i, "status": "completed", "returned": 7, "rejected": 3,
                 "estimate": value, "output_variance": 12, "sampling_seconds": 2}
                for i, value in enumerate([1., 2., 3.])]
        result = bench.summarize(rows)[0]
        self.assertEqual(result["estimator_variance"], 1)
        self.assertEqual(result["plug_in_estimator_variance"], 2)
        self.assertEqual(result["mean_rejected"], 3)
        rows[0]["status"] = "failed"
        with self.assertRaises(ValueError):
            bench.summarize(rows)


class SamplingBenchTests(unittest.TestCase):
    def run_sample(self, program, subject="source", fuel=100000):
        binary = os.environ.get("SAMPLING_BENCH_BINARY",
                                str(ROOT / "lean/.lake/build/bin/sampling-bench"))
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "input.det"
            path.write_text(program)
            result = subprocess.run([binary, str(path), subject, "10", "0", str(fuel), "0"],
                                    text=True, capture_output=True, timeout=30)
        return result, json.loads(result.stdout)

    def test_determinized_mean(self):
        result, measured = self.run_sample("uniform(0, 10)", "determinized")
        self.assertEqual(result.returncode, 0, result.stderr)
        row = bench.validate_measurement(measured, 10)
        self.assertEqual(row["returned"], 10)
        self.assertEqual(row["estimate"], 5)
        self.assertEqual(row["output_variance"], 0)

    def test_rejection_counts(self):
        result, measured = self.run_sample("let _ = observe(flip(1/2)) in 5")
        self.assertEqual(result.returncode, 0, result.stderr)
        row = bench.validate_measurement(measured, 10)
        self.assertGreater(row["rejected"], 0)
        self.assertEqual(row["estimate"], 5)

    def test_step_limit_is_an_error(self):
        result, measured = self.run_sample("let f = rec f x => f x in f 0", fuel=1)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(measured["failed"], 10)
        self.assertIn("10/10 executions failed: step limit reached", result.stderr)
        with self.assertRaises(ValueError):
            bench.validate_measurement(measured, 10)


if __name__ == "__main__":
    unittest.main()

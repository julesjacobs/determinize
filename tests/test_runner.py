"""Check that broken expectations and corpus omissions actually fail the test tools."""
import json
from pathlib import Path
import subprocess
import tempfile
import unittest

from run import ROOT, load_manifest, runner_case, validate_observation


class ManifestTests(unittest.TestCase):
    def test_unregistered_program_fails(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / 'tests').mkdir()
            (root / 'tests/one.det').write_text('1')
            manifest = root / 'tests/cases.toml'
            manifest.write_text('version = 1\n[[case]]\nfile = "tests/one.det"\nsuite = "typing"\noutcome = "accept"\n')
            self.assertEqual(len(load_manifest(manifest, root)), 1)
            (root / 'tests/forgotten.det').write_text('2')
            with self.assertRaisesRegex(ValueError, 'unlisted='):
                load_manifest(manifest, root)

    def test_vacuous_or_misspelled_expectations_fail(self):
        for observation in ({'tolerance': 1}, {'numbre': 1}, {'moments': {'mean': 1}},
                            {'number': 1, 'error': 'failure'}, {'number': float('nan')}):
            with self.subTest(observation=observation), self.assertRaises(ValueError):
                validate_observation(observation)


class RunnerTests(unittest.TestCase):
    def run_probe(self, text, observation, statistical=False):
        with tempfile.TemporaryDirectory() as tmp:
            program = Path(tmp) / 'Probe.det'
            program.write_text(text)
            manifest = Path(tmp) / 'cases.json'
            case = runner_case({'file': str(program), 'suite': 'statistical' if 'moments' in observation else 'execution',
                                'outcome': 'accept', 'source': observation, 'samples': 2000})
            manifest.write_text(json.dumps([case]))
            return subprocess.run([ROOT / 'lean/.lake/build/bin/det-tests', '--corpus', manifest,
                                   'statistical' if statistical else 'fast'], text=True, capture_output=True)

    def test_wrong_exact_result_fails(self):
        result = self.run_probe('1+2', {'number': 4})
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('result: expected', result.stderr)

    def test_fast_checks_results_but_skips_sampling(self):
        moments = dict(mean=10, variance=10, mean_tolerance=0, variance_tolerance=0)
        result = self.run_probe('1', {'number': 1, 'moments': moments})
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn('n=2000', result.stdout)

    def test_wrong_moments_fail(self):
        for field, wrong in [('mean', 10), ('variance', 10)]:
            moments = dict(mean=0.5, variance=1/12, mean_tolerance=0.1, variance_tolerance=0.1)
            moments[field] = wrong
            result = self.run_probe('uniform[E](0,1)', {'moments': moments}, True)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn(f'{field}: expected', result.stderr)

    def test_statistical_runtime_failures_are_not_discarded(self):
        moments = dict(mean=0, variance=0, mean_tolerance=1, variance_tolerance=1)
        result = self.run_probe('uniform[E](2,1)', {'moments': moments}, True)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('uniform requires', result.stderr)


if __name__ == '__main__':
    unittest.main()

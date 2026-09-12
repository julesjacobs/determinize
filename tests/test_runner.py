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
    def run_probe(self, text, observation, statistical=False, affinities=None):
        with tempfile.TemporaryDirectory() as tmp:
            program = Path(tmp) / 'Probe.det'
            program.write_text(text)
            manifest = Path(tmp) / 'cases.json'
            case = runner_case({'file': str(program), 'suite': 'statistical' if 'moments' in observation else 'execution',
                                'outcome': 'accept', 'source': observation, 'samples': 2000})
            if affinities is not None:
                case['affinities'] = affinities
            manifest.write_text(json.dumps([case]))
            return subprocess.run([ROOT / 'lean/.lake/build/bin/det-tests', '--corpus', manifest,
                                   'statistical' if statistical else 'fast'], text=True, capture_output=True)

    def test_affinity_expectations_are_checked(self):
        for expected, success in [(['E'], True), (['G'], False)]:
            result = self.run_probe('uniform[E](0,1)', {}, affinities=expected)
            self.assertEqual(result.returncode == 0, success, result.stderr)
            if not success:
                self.assertIn('expected affinities [G], got [E]', result.stderr)

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


class CliStatisticsTests(unittest.TestCase):
    def summarize(self, text, samples):
        with tempfile.TemporaryDirectory() as tmp:
            program = Path(tmp) / 'Statistics.det'
            program.write_text(text)
            result = subprocess.run([ROOT / 'lean/.lake/build/bin/determinize',
                                     '--samples', str(samples), program], text=True, capture_output=True)
            self.assertEqual(result.returncode, 0, result.stderr)
            return result.stdout

    def test_constant_has_zero_variance(self):
        output = self.summarize('1000000001', 1000)
        self.assertEqual(output.count('mean among returned values: 1000000001.000000; variance: 0.000000'), 2)

    def test_population_variance_and_returned_value_denominator(self):
        output = self.summarize('bernoulli[G](0.5)', 20)
        self.assertEqual(output.count('mean among returned values: 0.450000; variance: 0.247500'), 2)
        output = self.summarize('let _ = observe(flip(0.5)) in 3', 20)
        self.assertEqual(output.count('mean among returned values: 3.000000; variance: 0.000000'), 2)
        self.assertIn('rejected observations:', output)

    def test_overflow_is_not_reported_as_zero_variance(self):
        output = self.summarize('1e200 + bernoulli[G](0.5) * 1e200', 20)
        self.assertEqual(output.count('variance: unavailable (floating-point overflow)'), 2)


if __name__ == '__main__':
    unittest.main()

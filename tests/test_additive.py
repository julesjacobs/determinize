"""Additive output moments, independent certificates, and Storm integration."""
from fractions import Fraction
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
import unittest

from test_results import generate, kernel
from test_export import ROOT

sys.path.insert(0, str(ROOT / 'tools'))
import storm_additive

GEOMETRIC = 'let f = rec f u => if flip(0.5) then 0 else 1 + f u in f ()'


class AdditiveTests(unittest.TestCase):
    def test_native_certificate_and_tampering(self):
        with tempfile.TemporaryDirectory() as tmp:
            result, prefix = generate(Path(tmp), GEOMETRIC, 'source', '--additive', '--max-states', '100')
            self.assertEqual(result.returncode, 0, result.stderr)
            data = json.loads(Path(str(prefix) + '.result.json').read_text())
            self.assertFalse(data['kernel_checked'])
            self.assertEqual(data['termination_statistics_scope'], 'graph')
            self.assertEqual((data['return_mass'], data['answer'], data['second_moment'],
                              data['conditional_variance']), ('1', '1', '3', '2'))
            self.assertLess(data['states'], 100)
            certificate = Path(str(prefix) + '.result.lean')
            checked = kernel(certificate)
            self.assertEqual(checked.returncode, 0, checked.stdout + checked.stderr)
            self.assertNotIn('sorryAx', checked.stdout)
            text = certificate.read_text()
            self.assertNotIn("boundFirstValues", text)
            self.assertNotIn("bounds :=", text)
            self.assertNotIn("momentsValid :=", text)
            for old, new in [
                ('first := fun i => firstValues[i]', 'first := fun _ => 12345'),
                ('mass := fun i => massValues[i]', 'mass := fun _ => 0'),
                ('second := fun i => secondValues[i]', 'second := fun _ => 0'),
                ('checkedSource : Expr Rat :=', 'checkedSource : Expr Rat := .real 999 --'),
            ]:
                self.assertIn(old, text)
                certificate.write_text(text.replace(old, new, 1))
                self.assertNotEqual(kernel(certificate).returncode, 0, new)
            changed, count = re.subn(
                r'(⟨\d+, \(\(1 : Rat\) / 1\), )\(\(1 : Rat\) / 1\)',
                r'\g<1>((9 : Rat) / 1)', text, count=1)
            self.assertEqual(count, 1)
            certificate.write_text(changed)
            self.assertNotEqual(kernel(certificate).returncode, 0)

    def test_divergence_certificates(self):
        cases = [
            ('let f = rec f u => 1 + f u in f ()', ('0', '0', '0', '1')),
            ('let d = rec d u => 1 + d u in 2 + (if flip(0.25) then 10 else d ())',
             ('1/4', '3', '36', '3/4')),
            ('let f = rec f u => if flip(0.5) then 0 else (-1) + f u in f ()',
             ('1', '-1', '3', '0')),
        ]
        for program, expected in cases:
            with self.subTest(program=program), tempfile.TemporaryDirectory() as tmp:
                result, prefix = generate(Path(tmp), program, 'source', '--additive')
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                data = json.loads(Path(str(prefix) + '.result.json').read_text())
                self.assertEqual(tuple(data[key] for key in
                    ('return_mass', 'answer', 'second_moment', 'divergence_probability')), expected)
                checked = kernel(Path(str(prefix) + '.result.lean'))
                self.assertEqual(checked.returncode, 0, checked.stdout + checked.stderr)
                for forbidden in ('sorryAx', 'ofReduceBool', 'trustCompiler'):
                    self.assertNotIn(forbidden, checked.stdout + checked.stderr)

    def test_edge_format_preserves_rewards(self):
        with tempfile.TemporaryDirectory() as tmp:
            prefix = Path(tmp) / 'model'
            sidecar = Path(str(prefix) + '.additive.edges')
            sidecar.write_text('additive-rewards 1\n0 1 1/2 0\n0 1 1/2 2\n1 1 1 0\n')
            labels = {'returned': {1}, 'rejected': set()}
            controller = [(0, 1, Fraction(1)), (1, 2, Fraction(1)), (2, 2, Fraction(1))]
            edges = storm_additive.read_edges(prefix, 3, controller, labels)
            def query(rhs):
                return [rhs[0] + rhs[1], rhs[1]]
            values = storm_additive.moments(query, 3, labels, [0, 0, 0], set(), edges)
            self.assertEqual(values['first'][0], 1)
            self.assertEqual(values['second'][0], 2)
            sidecar.write_text(sidecar.read_text().replace('0 1 1/2 2', '0 1 1/3 2'))
            with self.assertRaisesRegex(ValueError, 'disagree'):
                storm_additive.read_edges(prefix, 3, controller, labels)

    @unittest.skipUnless(os.environ.get('STORM_PYTHON'), 'STORM_PYTHON not configured')
    def test_real_storm(self):
        cases = [GEOMETRIC, '1 + (-2)',
                 'let f = rec f u => 1 + f u in f ()',
                 'let f = rec f u => if flip(0.5) then 1 + f u else if flip(0.5) then 0 else (let _ = observe(false) in 0) in f ()']
        for program in cases:
            with self.subTest(program=program), tempfile.TemporaryDirectory() as tmp:
                source = Path(tmp) / 'input.det'
                source.write_text(program)
                result = subprocess.run([os.environ['STORM_PYTHON'], ROOT / 'tools/storm.py',
                    source, '--prefix', Path(tmp) / 'model', '--subject', 'source',
                    '--additive', '--max-states', '100', '--compare'],
                    text=True, capture_output=True, timeout=180)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)


if __name__ == '__main__':
    unittest.main()

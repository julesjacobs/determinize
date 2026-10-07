"""The closing theorems of exact-result certificates, on every route that writes one."""
from fractions import Fraction
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
import unittest

from test_export import ROOT
from test_results import generate, kernel

# How to run each route, by the file that generates its certificate; `test_every_generator_has_a_route`
# requires an entry for every generator that `generators` finds.
ROUTES = {
    "lean/Determinize/Finite/Export.lean": ("built-in", []),
    "lean/Determinize/Finite/Reward/Export.lean": ("built-in", ["--additive"]),
    "tools/storm.py": ("Storm", []),
    "tools/storm_additive.py": ("Storm", ["--additive"]),
}
# The report, its key for the first moment, and the certificate.
OUTPUTS = {"built-in": (".result.json", "answer", ".result.lean"),
           "Storm": (".storm.json", "exact_answer", ".storm.lean")}
# Return mass, first and second moments, conditional mean and conditional variance.
PROGRAMS = [
    ("if flip(0.25) then 8 else -4", ("1", "-1", "28", "-1", "27")),
    ("let f = rec f x => f x in if flip(0.5) then f 0 else if flip(0.5) then -2 else 4",
     ("1/2", "1/2", "5", "1", "9")),
    ("let _ = observe(false) in 3", ("0", "0", "0", None, None)),
]
AXIOMS = {"propext", "Classical.choice", "Quot.sound"}
# Everything but literals that the closing statements may name: `Spec` definitions and the
# certificate's `checkedSource` and `checkedSubject`.
NAMES = {"Determinize.Spec.FiniteModel.OutputStatistics", "Matches", "Determinize.Spec.Paper.bigStepMeasure",
         "Determinize.Spec.returnedExpectation", "Determinize.Spec.returnedVariance",
         "checkedSubject.program", "checkedSource", "Rat", "ℝ"}


def generators():
    """Tracked sources that write the main claim of an exact-result certificate."""
    files = subprocess.run(["git", "ls-files", "-z", "--", "*.lean", "*.py", ":!tests"], cwd=ROOT,
                           text=True, capture_output=True, check=True).stdout.split("\0")
    return {path for path in files if path and "theorem outputStatistics" in (ROOT / path).read_text()}


def closing(text):
    """The statements of the theorems from `printedStatistics` on, and what else follows them."""
    start = text.index("\ntheorem printedStatistics :")
    statements = dict(re.findall(r"^theorem (\w+) :\n([\s\S]*?) :=\n", text[start:], re.M))
    rest = re.sub(r"^theorem \w+ :\n[\s\S]*? :=\n(  .*\n)+", "", text[start:], flags=re.M)
    return statements, rest


def axioms(output):
    return {name: {axiom.strip() for axiom in listed.split(",")}
            for name, listed in re.findall(r"'([^']+)' depends on axioms: \[([^]]*)\]", output)}


class PrintedStatisticsTests(unittest.TestCase):
    def certify(self, directory, generator, program):
        """The reported statistics and the certificate of one route."""
        backend, options = ROUTES[generator]
        report, first, certificate = OUTPUTS[backend]
        if backend == "built-in":
            result, prefix = generate(directory, program, "source", *options)
        else:
            if not os.environ.get("STORM_PYTHON"):
                self.skipTest("set STORM_PYTHON for real Storm integration")
            source = directory / "input.det"
            source.write_text(program)
            prefix = directory / "model"
            result = subprocess.run([os.environ["STORM_PYTHON"], ROOT / "tools/storm.py", source,
                                     "--prefix", prefix, "--subject", "source", *options, "--skip-certificate"],
                                    text=True, capture_output=True, timeout=180)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        data = json.loads(Path(str(prefix) + report).read_text())
        reported = tuple(data[key] for key in
                         ("return_mass", first, "second_moment", "conditional_mean", "conditional_variance"))
        return reported, Path(str(prefix) + certificate)

    def test_every_generator_has_a_route(self):
        self.assertEqual(generators(), set(ROUTES))

    def test_closing_theorems(self):
        for generator in sorted(ROUTES):
            for program, expected in PROGRAMS:
                with self.subTest(route=generator, program=program), tempfile.TemporaryDirectory() as tmp:
                    reported, certificate = self.certify(Path(tmp), generator, program)
                    self.assertEqual(reported, expected)
                    mass, first, second, mean, variance = reported
                    text = certificate.read_text()
                    statements, rest = closing(text)
                    self.assertLessEqual(set(rest.split()), {"#print", "axioms", *statements}, rest)
                    self.assertEqual(set(statements), {"printedStatistics"} |
                                     ({"printedConditionalMoments"} if mean else set()))
                    self.assertIn(f"(⟨{mass}, {first}, {second}⟩ : ", statements["printedStatistics"])
                    if mean:
                        self.assertIn(f"returnedExpectation (checkedSubject.program checkedSource) = "
                                      f"(({mean} : Rat) : ℝ) ∧", statements["printedConditionalMoments"])
                        self.assertIn(f"returnedVariance (checkedSubject.program checkedSource) = "
                                      f"(({variance} : Rat) : ℝ)", statements["printedConditionalMoments"])
                    for statement in statements.values():
                        self.assertLessEqual(set(re.findall(r"[^\W\d][\w.]*", statement)), NAMES, statement)
                    checked = kernel(certificate)
                    self.assertEqual(checked.returncode, 0, checked.stdout + checked.stderr)
                    reports = axioms(checked.stdout)
                    self.assertLessEqual(set(statements), set(reports))
                    for name, used in reports.items():
                        self.assertLessEqual(used, AXIOMS, name)

    def test_tampered_values(self):
        program, (mass, first, second, mean, variance) = PROGRAMS[0]
        for generator in sorted(ROUTES):
            with self.subTest(route=generator), tempfile.TemporaryDirectory() as tmp:
                _, certificate = self.certify(Path(tmp), generator, program)
                text = certificate.read_text()
                start = text.index("\ntheorem printedStatistics :")
                literal = f"⟨{mass}, {first}, {second}⟩"
                changed = f"⟨{mass}, {Fraction(first) + 1}, {second}⟩"
                self.assertEqual(text[start:].count(literal), 2)
                certificate.write_text(text[:start] + text[start:].replace(literal, changed))
                self.assertNotEqual(kernel(certificate).returncode, 0, changed)
                literal = f"(({variance} : Rat) : ℝ)"
                changed = f"(({Fraction(variance) + 1} : Rat) : ℝ)"
                self.assertEqual(text[start:].count(literal), 1)
                certificate.write_text(text[:start] + text[start:].replace(literal, changed))
                checked = kernel(certificate)
                self.assertNotEqual(checked.returncode, 0, changed)
                self.assertLessEqual(axioms(checked.stdout)["printedStatistics"], AXIOMS)


if __name__ == "__main__":
    unittest.main()

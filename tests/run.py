#!/usr/bin/env python3
"""Load the shared TOML corpus; all language evaluation and assertions run in Lean."""
import argparse
import json
import math
from pathlib import Path
import subprocess
import tempfile
import tomllib

ROOT = Path(__file__).resolve().parents[1]


def require(condition, message):
    if not condition:
        raise ValueError(message)


def keys(table, allowed):
    require(isinstance(table, dict), 'expected a table')
    require(not (table.keys() - allowed), f'unknown fields: {table.keys() - allowed}')


def finite(value):
    return type(value) in (int, float) and math.isfinite(value)


def validate_observation(o):
    keys(o, {'number', 'value', 'error', 'draws', 'tolerance', 'moments'})
    require(bool(o.keys() & {'number', 'value', 'error', 'draws', 'moments'}), 'observation needs an expectation')
    require(sum(k in o for k in ('number', 'value', 'error')) <= 1, 'conflicting result expectations')
    require('error' not in o or set(o) == {'error'}, 'runtime error cannot have value expectations')
    for k in ('number', 'tolerance'):
        if k in o:
            require(finite(o[k]), f'{k} must be finite')
    require(o.get('tolerance', 0) >= 0, 'negative tolerance')
    for k in ('value', 'error'):
        if k in o:
            require(isinstance(o[k], str) and bool(o[k]), f'{k} must be a nonempty string')
    if 'draws' in o:
        require(type(o['draws']) is int and o['draws'] >= 0, 'invalid draw count')
    if 'moments' in o:
        m = o['moments']
        required = {'mean', 'variance', 'mean_tolerance', 'variance_tolerance'}
        keys(m, required | {'lower', 'upper', 'integer'})
        require(required <= m.keys(), 'incomplete moments')
        for k in m.keys() - {'integer'}:
            require(finite(m[k]), f'{k} must be finite')
        require(m['variance'] >= 0 and m['mean_tolerance'] >= 0 and m['variance_tolerance'] >= 0,
                'negative variance or tolerance')
        require(m.get('lower', -math.inf) <= m.get('upper', math.inf), 'invalid support bounds')
        if 'integer' in m:
            require(type(m['integer']) is bool, 'integer must be Boolean')


def load_manifest(path=ROOT / 'tests/cases.toml', root=ROOT):
    data = tomllib.loads(path.read_text())
    keys(data, {'version', 'case', 'exclude'})
    require(data.get('version') == 1, 'unsupported manifest version')
    cases = data.get('case', [])
    require(bool(cases), 'empty corpus')
    seen = set()
    allowed = {'file', 'suite', 'outcome', 'stage', 'expected_type', 'modes', 'source', 'target',
               'samples', 'seed', 'fuel', 'kernel', 'derivation'}
    for c in cases:
        keys(c, allowed)
        file = c.get('file', '')
        require(file and not Path(file).is_absolute() and '..' not in Path(file).parts,
                f'invalid corpus path: {file}')
        require(file not in seen, f'duplicate case: {file}')
        require((root / file).is_file() and file.endswith('.det'), f'missing .det case: {file}')
        seen.add(file)
        require(c.get('suite') in {'typing', 'execution', 'statistical'}, f'{file}: invalid suite')
        require(c.get('outcome') in {'accept', 'reject'}, f'{file}: invalid outcome')
        for k in ('samples', 'seed', 'fuel'):
            if k in c:
                require(type(c[k]) is int and c[k] >= (2 if k == 'samples' else 1 if k == 'fuel' else 0),
                        f'{file}: invalid {k}')
        for k in ('kernel',):
            if k in c:
                require(type(c[k]) is bool, f'{file}: {k} must be Boolean')
        if 'expected_type' in c:
            require(isinstance(c['expected_type'], str) and c['expected_type'], 'invalid expected type')
        if 'modes' in c:
            require(isinstance(c['modes'], list) and all(m in ('E', 'G') for m in c['modes']), 'invalid modes')
        if c['outcome'] == 'reject':
            require(c.get('stage') in {'parse', 'elaboration', 'inference', 'certificate'}, 'missing rejection stage')
            require(c['suite'] == 'typing' and not any(k in c for k in ('source', 'target', 'kernel', 'modes', 'expected_type')),
                    'rejected case has acceptance expectations')
        else:
            require('stage' not in c, 'accepted case has rejection stage')
        for k in ('source', 'target'):
            if k in c:
                validate_observation(c[k])
                require('moments' not in c[k] or c['suite'] == 'statistical', 'moments outside statistical suite')
        if c['suite'] == 'execution':
            require('source' in c or 'target' in c, f'{file}: execution needs an expected result')
        if c['suite'] == 'statistical':
            require(all('moments' in c.get(k, {}) for k in ('source', 'target')), 'missing source/target moments')
            require('samples' in c and 'seed' in c and c.get('derivation'), 'statistics need sample count, seed, and derivation')
    excluded = set()
    for e in data.get('exclude', []):
        keys(e, {'file', 'reason'})
        require(e.get('file') and e.get('reason'), 'exclusion needs file and reason')
        require(e['file'] not in seen | excluded, 'duplicate exclusion or tested exclusion')
        excluded.add(e['file'])
    inventory = {str(p.relative_to(root)) for folder in ('tests', 'examples') for p in (root / folder).rglob('*.det')}
    require(inventory == seen | excluded, f'corpus coverage mismatch: unlisted={inventory - seen - excluded}, stale={(seen | excluded) - inventory}')
    return cases


def runner_case(case):
    # Lean's derived JSON decoder requires explicit scalar defaults.
    c = {'stage': '', 'samples': 20000, 'seed': 20260911, 'fuel': 100000, **case}
    for key in ('source', 'target'):
        if key in c:
            c[key] = {'tolerance': 1e-10, **c[key]}
            if 'moments' in c[key]:
                c[key]['moments'] = {'integer': False, **c[key]['moments']}
    return c


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    suites = parser.add_mutually_exclusive_group()
    suites.add_argument('--statistical', action='store_true', help='run only the statistical corpus')
    suites.add_argument('--all', action='store_true', help='run fast checks, statistics, and kernel certificates')
    args = parser.parse_args()
    cases = load_manifest()
    selected = [c for c in cases if c['suite'] == 'statistical'] if args.statistical else cases
    with tempfile.TemporaryDirectory(prefix='determinize-corpus-') as tmp:
        manifest = Path(tmp) / 'cases.json'
        manifest.write_text(json.dumps([runner_case(c) for c in selected], allow_nan=False))
        exe = ROOT / 'lean/.lake/build/bin/det-tests'
        subprocess.run([exe, '--corpus', manifest, 'statistical' if args.statistical or args.all else 'fast'], cwd=ROOT, check=True)
        if not args.statistical:
            for c in cases:
                if not c.get('kernel'):
                    continue
                cert = Path(tmp) / 'Certificate.lean'
                subprocess.run([ROOT / 'lean/.lake/build/bin/determinize', '--check', '--certificate', cert, ROOT / c['file']], check=True)
                result = subprocess.run(['lake', 'env', 'lean', cert], cwd=ROOT / 'lean', text=True, capture_output=True, check=True)
                require(not any(s in result.stdout + result.stderr for s in ('sorryAx', 'ofReduceBool', 'trustCompiler', 'lean4Lean')), 'unexpected certificate axiom')
            print('Independent kernel certificate checks passed.')


if __name__ == '__main__':
    main()

import type { Expr, ExprOf } from "../compiler/ast.ts";

/**
 * constant + Σ terms[name] · name, over symbols named v1, v2, … `errors` bounds, to first order,
 * how far the constant and each coefficient are from what exact arithmetic gives on the same
 * literals and draws; a missing bound is 0. A coefficient that cancels to exactly 0 is dropped
 * with its bound.
 */
export interface Affine {
  constant: number;
  terms: Record<string, number>;
  errors?: { constant: number; terms: Record<string, number> };
}

/** The unit roundoff: an operation's result is within `unitRoundoff` times its size of the exact
 * result of the same operands. */
const unitRoundoff = 2 ** -53;

/** The rounding error of an operation that gave `result`, unless the operation was exact. */
function rounding(result: number, exact: boolean) {
  return exact ? 0 : unitRoundoff * Math.abs(result);
}

export function affineConst(value: number, error = 0): Affine {
  return normalize({ constant: value, terms: {}, errors: { constant: error, terms: {} } });
}

export function constantError(a: Affine) {
  return a.errors?.constant ?? 0;
}

function termError(a: Affine, name: string) {
  return a.errors?.terms[name] ?? 0;
}

export function affineVar(name: string): Affine {
  return { constant: 0, terms: { [name]: 1 } };
}

export function isAffine(value: Expr | null | undefined) {
  return value?.kind === "SymFloat";
}

export function symFloat(affine: Affine, from = 0, to = from): ExprOf<"SymFloat"> {
  return { kind: "SymFloat", affine: normalize(affine), from, to };
}

export function valueToAffine(value: Expr): Affine {
  if (value.kind === "Const") return affineConst(value.value, value.error);
  if (value.kind === "SymFloat") return value.affine;
  throw new Error(`expected float value, got ${value.kind}`);
}

export function affineAdd(a: Affine, b: Affine): Affine {
  const terms = { ...a.terms };
  const errors: Record<string, number> = { ...a.errors?.terms };
  for (const [name, coeff] of Object.entries(b.terms)) {
    const sum = (terms[name] ?? 0) + coeff;
    errors[name] = termError(a, name) + termError(b, name) + rounding(sum, !(name in terms));
    terms[name] = sum;
  }
  const constant = a.constant + b.constant;
  const constantBound =
    constantError(a) + constantError(b) + rounding(constant, a.constant === 0 || b.constant === 0);
  return normalize({ constant, terms, errors: { constant: constantBound, terms: errors } });
}

export function affineNeg(a: Affine): Affine {
  return affineScale(a, -1);
}

export function affineSub(a: Affine, b: Affine): Affine {
  return affineAdd(a, affineNeg(b));
}

/** `a` times `scalar`, whose own error is bounded by `scalarError`. */
export function affineScale(a: Affine, scalar: number, scalarError = 0): Affine {
  const scaledError = (value: number, error: number) =>
    error * Math.abs(scalar) +
    Math.abs(value) * scalarError +
    rounding(value * scalar, Math.abs(scalar) === 1 || value === 0);
  const terms: Record<string, number> = {};
  const errors: Record<string, number> = {};
  for (const [name, coeff] of Object.entries(a.terms)) {
    terms[name] = coeff * scalar;
    errors[name] = scaledError(coeff, termError(a, name));
  }
  return normalize({
    constant: a.constant * scalar,
    terms,
    errors: { constant: scaledError(a.constant, constantError(a)), terms: errors },
  });
}

export function affineMul(a: Affine, b: Affine): Affine {
  if (isConcreteAffine(a)) return affineScale(b, a.constant, constantError(a));
  if (isConcreteAffine(b)) return affineScale(a, b.constant, constantError(b));
  throw new Error("symbolic multiplication is only affine when one side is concrete");
}

export function affineDiv(a: Affine, b: Affine): Affine {
  if (!isConcreteAffine(b))
    throw new Error("symbolic division is only affine with a concrete denominator");
  const inverse = 1 / b.constant;
  const inverseError =
    constantError(b) / (b.constant * b.constant) + rounding(inverse, Math.abs(b.constant) === 1);
  return affineScale(a, inverse, inverseError);
}

/**
 * The first-order bound on the error of `x op y`, a number the runtime computes as Lean's does,
 * from the bounds on the errors of `x` and `y`.
 */
export function operationError(
  kind: "Add" | "Sub" | "Mul" | "Div",
  x: number,
  xError: number,
  y: number,
  yError: number,
  result: number,
) {
  switch (kind) {
    case "Add":
    case "Sub":
      return xError + yError + rounding(result, x === 0 || y === 0);
    case "Mul":
      return (
        xError * Math.abs(y) +
        Math.abs(x) * yError +
        rounding(result, Math.abs(x) === 1 || Math.abs(y) === 1 || result === 0)
      );
    case "Div":
      return (
        xError / Math.abs(y) +
        (Math.abs(x) * yError) / (y * y) +
        rounding(result, Math.abs(y) === 1)
      );
  }
}

export function isConcreteAffine(a: Affine) {
  return Object.keys(a.terms).length === 0;
}

export function affineToNumber(a: Affine): number {
  if (!isConcreteAffine(a))
    throw new Error(`expected concrete affine value, got ${prettyAffine(a)}`);
  return a.constant;
}

export function evalAffine(a: Affine, env: Map<string, number>): number {
  let value = a.constant;
  for (const [name, coeff] of Object.entries(a.terms)) {
    if (!env.has(name)) throw new Error(`missing symbolic value ${name}`);
    value += coeff * (env.get(name) as number);
  }
  return value;
}

/**
 * `evalAffine`, with the first-order bound on the error of its result: from the form's bounds,
 * the bounds on the symbols' values in `errors`, and the evaluation's own rounding.
 */
export function evalAffineWithError(
  a: Affine,
  env: Map<string, number>,
  errors: Map<string, number>,
): { value: number; error: number } {
  let value = a.constant;
  let error = constantError(a);
  for (const [name, coeff] of Object.entries(a.terms)) {
    if (!env.has(name)) throw new Error(`missing symbolic value ${name}`);
    const x = env.get(name) as number;
    const product = coeff * x;
    error +=
      termError(a, name) * Math.abs(x) +
      Math.abs(coeff) * (errors.get(name) ?? 0) +
      rounding(product, Math.abs(coeff) === 1) +
      rounding(value + product, value === 0);
    value += product;
  }
  return { value, error };
}

export function normalize(a: {
  constant?: number;
  terms?: Record<string, number>;
  errors?: Affine["errors"];
}): Affine {
  const terms: Record<string, number> = {};
  const errors: Record<string, number> = {};
  for (const [name, coeff] of Object.entries(a.terms ?? {})) {
    if (coeff === 0) continue;
    terms[name] = coeff;
    const error = a.errors?.terms[name] ?? 0;
    if (error > 0) errors[name] = error;
  }
  const constant = a.constant === 0 ? 0 : (a.constant ?? 0);
  const constantBound = a.errors?.constant ?? 0;
  if (constantBound === 0 && Object.keys(errors).length === 0) return { constant, terms };
  return { constant, terms, errors: { constant: constantBound, terms: errors } };
}

export function prettyAffine(a: Affine) {
  const parts: string[] = [];
  if (a.constant !== 0 || Object.keys(a.terms).length === 0) parts.push(formatNumber(a.constant));
  for (const [name, coeff] of Object.entries(a.terms)) {
    if (coeff === 1) parts.push(name);
    else if (coeff === -1) parts.push(`-${name}`);
    else parts.push(`${formatNumber(coeff)}*${name}`);
  }
  return parts.join(" + ").replace(/\+ -/g, "- ");
}

function formatNumber(value: number) {
  if (value === 0) return "0";
  if (Number.isInteger(value)) return String(value);
  return Number(value.toPrecision(13)).toString();
}

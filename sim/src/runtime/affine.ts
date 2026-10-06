import type { Expr, ExprOf } from "../compiler/ast.ts";

/** constant + Σ terms[name] · name, over symbols named v1, v2, … */
export interface Affine {
  constant: number;
  terms: Record<string, number>;
}

export function affineConst(value: number): Affine {
  return normalize({ constant: value, terms: {} });
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
  if (value.kind === "Const") return affineConst(value.value);
  if (value.kind === "SymFloat") return value.affine;
  throw new Error(`expected float value, got ${value.kind}`);
}

export function affineAdd(a: Affine, b: Affine): Affine {
  const terms = { ...a.terms };
  for (const [name, coeff] of Object.entries(b.terms)) {
    terms[name] = (terms[name] ?? 0) + coeff;
  }
  return normalize({ constant: a.constant + b.constant, terms });
}

export function affineNeg(a: Affine): Affine {
  return affineScale(a, -1);
}

export function affineSub(a: Affine, b: Affine): Affine {
  return affineAdd(a, affineNeg(b));
}

export function affineScale(a: Affine, scalar: number): Affine {
  return normalize({
    constant: a.constant * scalar,
    terms: Object.fromEntries(
      Object.entries(a.terms).map(([name, coeff]) => [name, coeff * scalar]),
    ),
  });
}

export function affineMul(a: Affine, b: Affine): Affine {
  if (isConcreteAffine(a)) return affineScale(b, a.constant);
  if (isConcreteAffine(b)) return affineScale(a, b.constant);
  throw new Error("symbolic multiplication is only affine when one side is concrete");
}

export function affineDiv(a: Affine, b: Affine): Affine {
  if (!isConcreteAffine(b))
    throw new Error("symbolic division is only affine with a concrete denominator");
  return affineScale(a, 1 / b.constant);
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

export function normalize(a: { constant?: number; terms?: Record<string, number> }): Affine {
  const terms: Record<string, number> = {};
  for (const [name, coeff] of Object.entries(a.terms ?? {})) {
    if (coeff !== 0) terms[name] = coeff;
  }
  return {
    constant: a.constant === 0 ? 0 : (a.constant ?? 0),
    terms,
  };
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

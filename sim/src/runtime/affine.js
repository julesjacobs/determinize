const EPS = 1e-12;

export function affineConst(value) {
  return normalize({ constant: value, terms: {} });
}

export function affineVar(name) {
  return { constant: 0, terms: { [name]: 1 } };
}

export function isAffine(value) {
  return value?.kind === "SymFloat";
}

export function symFloat(affine, from = 0, to = from) {
  return { kind: "SymFloat", affine: normalize(affine), from, to };
}

export function valueToAffine(value) {
  if (value.kind === "Const") return affineConst(value.value);
  if (value.kind === "SymFloat") return value.affine;
  throw new Error(`expected float value, got ${value.kind}`);
}

export function affineAdd(a, b) {
  const terms = { ...a.terms };
  for (const [name, coeff] of Object.entries(b.terms)) {
    terms[name] = (terms[name] ?? 0) + coeff;
  }
  return normalize({ constant: a.constant + b.constant, terms });
}

export function affineNeg(a) {
  return affineScale(a, -1);
}

export function affineSub(a, b) {
  return affineAdd(a, affineNeg(b));
}

export function affineScale(a, scalar) {
  return normalize({
    constant: a.constant * scalar,
    terms: Object.fromEntries(Object.entries(a.terms).map(([name, coeff]) => [name, coeff * scalar])),
  });
}

export function affineMul(a, b) {
  if (isConcreteAffine(a)) return affineScale(b, a.constant);
  if (isConcreteAffine(b)) return affineScale(a, b.constant);
  throw new Error("symbolic multiplication is only affine when one side is concrete");
}

export function affineDiv(a, b) {
  if (!isConcreteAffine(b)) throw new Error("symbolic division is only affine with a concrete denominator");
  return affineScale(a, 1 / b.constant);
}

export function isConcreteAffine(a) {
  return Object.keys(a.terms).length === 0;
}

export function affineToNumber(a) {
  if (!isConcreteAffine(a)) throw new Error(`expected concrete affine value, got ${prettyAffine(a)}`);
  return a.constant;
}

export function evalAffine(a, env) {
  let value = a.constant;
  for (const [name, coeff] of Object.entries(a.terms)) {
    if (!env.has(name)) throw new Error(`missing symbolic value ${name}`);
    value += coeff * env.get(name);
  }
  return value;
}

export function normalize(a) {
  const terms = {};
  for (const [name, coeff] of Object.entries(a.terms ?? {})) {
    if (Math.abs(coeff) > EPS) terms[name] = coeff;
  }
  return {
    constant: Math.abs(a.constant ?? 0) <= EPS ? 0 : a.constant,
    terms,
  };
}

export function prettyAffine(a) {
  const parts = [];
  if (Math.abs(a.constant) > EPS || Object.keys(a.terms).length === 0) parts.push(formatNumber(a.constant));
  for (const [name, coeff] of Object.entries(a.terms)) {
    if (Math.abs(coeff - 1) <= EPS) parts.push(name);
    else if (Math.abs(coeff + 1) <= EPS) parts.push(`-${name}`);
    else parts.push(`${formatNumber(coeff)}*${name}`);
  }
  return parts.join(" + ").replace(/\+ -/g, "- ");
}

function formatNumber(value) {
  if (Object.is(value, -0) || Math.abs(value) <= EPS) return "0";
  if (Number.isInteger(value)) return String(value);
  return Number(value.toFixed(12)).toString();
}

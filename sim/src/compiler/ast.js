export function node(kind, props, from, to) {
  return { kind, ...props, from, to };
}

export const distributions = new Set([
  "uniform",
  "gauss",
  "exponential",
  "gamma",
  "beta",
  "flip",
  "bernoulli",
  "poisson",
  "discrete",
]);

export function stripSpans(expr) {
  if (!expr || typeof expr !== "object") return expr;
  const out = { kind: expr.kind };
  for (const [key, value] of Object.entries(expr)) {
    if (key === "kind" || key === "from" || key === "to") continue;
    if (Array.isArray(value)) {
      out[key] = value.map((item) => stripSpans(item));
    } else if (value && typeof value === "object" && "kind" in value) {
      out[key] = stripSpans(value);
    } else {
      out[key] = value;
    }
  }
  return out;
}

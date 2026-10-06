import type { Mode } from "./ast.ts";

/** Lean's `Ty` (`Spec/Types.lean`): `float[G]` is a subtype of `float[E]`. */
export type Ty =
  | { tag: "unit" | "bool" }
  | { tag: "float"; mode: Mode }
  | { tag: "prod" | "sum" | "arr"; a: Ty; b: Ty }
  | { tag: "list"; a: Ty };

/** The type as Lean's `prettyType` prints it. */
export function prettyType(ty: Ty): string {
  switch (ty.tag) {
    case "unit":
    case "bool":
      return ty.tag;
    case "float":
      return `float[${ty.mode}]`;
    case "prod":
      return `(${prettyType(ty.a)} * ${prettyType(ty.b)})`;
    case "sum":
      return `(${prettyType(ty.a)} + ${prettyType(ty.b)})`;
    case "list":
      return `[${prettyType(ty.a)}]`;
    case "arr":
      return `(${prettyType(ty.a)} -> ${prettyType(ty.b)})`;
  }
}

/** The type with only the parentheses that products, sums and arrows need. */
export function formatType(ty: Ty, prec = 0): string {
  const wrap = (text: string, level: number) => (prec > level ? `(${text})` : text);
  switch (ty.tag) {
    case "unit":
    case "bool":
      return ty.tag;
    case "float":
      return `float[${ty.mode}]`;
    case "prod":
      return wrap(`${formatType(ty.a, 3)} * ${formatType(ty.b, 3)}`, 2);
    case "sum":
      return wrap(`${formatType(ty.a, 3)} + ${formatType(ty.b, 3)}`, 2);
    case "list":
      return `[${formatType(ty.a)}]`;
    case "arr":
      return wrap(`${formatType(ty.a, 1)} -> ${formatType(ty.b, 0)}`, 0);
  }
}

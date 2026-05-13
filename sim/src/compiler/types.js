import { CompileError } from "./errors.js";

let modeCounter = 0;
let tyCounter = 0;

export function resetTypeState() {
  modeCounter = 0;
  tyCounter = 0;
}

export function freshModeMeta() {
  modeCounter += 1;
  return { tag: "ModeMeta", id: modeCounter, mode: null, constraints: [] };
}

export function freshMeta() {
  tyCounter += 1;
  return { tag: "Meta", id: tyCounter, value: null };
}

export const TUnit = { tag: "Unit" };
export const TBool = { tag: "Bool" };
export const TNat = { tag: "Nat" };
export const TFloat = (mode = freshModeMeta()) => ({ tag: "Float", mode });
export const TPair = (left, right) => ({ tag: "Pair", left, right });
export const TSum = (left, right) => ({ tag: "Sum", left, right });
export const TList = (elem) => ({ tag: "List", elem });
export const TArrow = (arg, result) => ({ tag: "Arrow", arg, result });
export const TMeta = (meta = freshMeta()) => ({ tag: "MetaType", meta });

export function setMode(mvar, mode, source = undefined) {
  if (mvar.mode == null) {
    mvar.mode = mode;
    for (const c of [...mvar.constraints]) propagateSubmode(c.lhs, c.rhs, source);
    return;
  }
  if (mvar.mode !== mode) {
    throw new CompileError(`mode mismatch: expected ${mvar.mode}-mode sample, found ${mode}-mode sample`, source?.from, source?.to);
  }
}

function propagateSubmode(lhs, rhs, source = undefined) {
  if (lhs.mode === "E" && rhs.mode == null) setMode(rhs, "E", source);
  else if (lhs.mode == null && rhs.mode === "G") setMode(lhs, "G", source);
  else if (lhs.mode === "E" && rhs.mode === "G") {
    throw new CompileError("mode mismatch: E-mode value cannot be used where G-mode sampling is required", source?.from, source?.to);
  }
}

export function submode(lhs, rhs, source = undefined) {
  const c = { lhs, rhs };
  lhs.constraints.push(c);
  rhs.constraints.push(c);
  propagateSubmode(lhs, rhs, source);
}

export function zonk(type, seen = new Set()) {
  if (type.tag !== "MetaType") return type;
  const meta = type.meta;
  if (seen.has(meta.id)) return type;
  if (!meta.value) return type;
  seen.add(meta.id);
  const value = zonk(meta.value, seen);
  meta.value = value;
  return value;
}

export function setType(meta, type, source = undefined) {
  const value = zonk(type);
  if (!meta.value) {
    if (value.tag === "MetaType" && value.meta.id === meta.id) return;
    meta.value = value;
    return;
  }
  assertSubtype(value, zonk(meta.value), source);
}

export function assertSubtype(left, right, source = undefined) {
  const a = zonk(left);
  const b = zonk(right);
  if (a.tag === "Float" && b.tag === "Float") return submode(a.mode, b.mode, source);
  if ((a.tag === "Pair" && b.tag === "Pair") || (a.tag === "Sum" && b.tag === "Sum")) {
    assertSubtype(a.left, b.left, source);
    assertSubtype(a.right, b.right, source);
    return;
  }
  if (a.tag === "List" && b.tag === "List") return assertSubtype(a.elem, b.elem, source);
  if (a.tag === "Arrow" && b.tag === "Arrow") {
    assertSubtype(b.arg, a.arg, source);
    assertSubtype(a.result, b.result, source);
    return;
  }
  if (a.tag === "Unit" && b.tag === "Unit") return;
  if (a.tag === "Bool" && b.tag === "Bool") return;
  if (a.tag === "Nat" && b.tag === "Nat") return;
  if (a.tag === "MetaType" && b.tag === "MetaType" && a.meta.id === b.meta.id) return;
  if (a.tag === "MetaType") return setType(a.meta, b, source);
  if (b.tag === "MetaType") return setType(b.meta, a, source);
  throw new CompileError(`type mismatch: expected ${formatTypeForError(b)}, found ${formatTypeForError(a)}`, source?.from, source?.to);
}

export function ensureFloat(expected, source = undefined) {
  const ty = zonk(expected);
  if (ty.tag === "Float") return ty;
  if (ty.tag === "MetaType") {
    const mode = freshModeMeta();
    const floatTy = TFloat(mode);
    setType(ty.meta, floatTy, source);
    return floatTy;
  }
  throw new CompileError(`expected float, found ${formatTypeForError(ty)}`, source?.from, source?.to);
}

function formatTypeForError(type) {
  return formatType(type)
    .replace(/float\[\?m\d+\]/g, "float")
    .replace(/\?t\d+/g, "unknown");
}

export function freshFloat() {
  return TFloat(freshModeMeta());
}

export function defaultModesType(type) {
  const ty = zonk(type);
  switch (ty.tag) {
    case "Float":
      if (ty.mode.mode == null) setMode(ty.mode, "E");
      break;
    case "Pair":
    case "Sum":
      defaultModesType(ty.left);
      defaultModesType(ty.right);
      break;
    case "List":
      defaultModesType(ty.elem);
      break;
    case "Arrow":
      defaultModesType(ty.arg);
      defaultModesType(ty.result);
      break;
    case "MetaType":
      if (ty.meta.value) defaultModesType(ty.meta.value);
      break;
  }
}

export function formatType(type) {
  const seen = new Set();
  const go = (ty, prec = 0) => {
    ty = zonk(ty);
    switch (ty.tag) {
      case "Unit":
        return "unit";
      case "Bool":
        return "bool";
      case "Nat":
        return "nat";
      case "Float":
        return `float[${ty.mode.mode ?? `?m${ty.mode.id}`}]`;
      case "Pair": {
        const s = `${go(ty.left, 2)} * ${go(ty.right, 2)}`;
        return prec > 2 ? `(${s})` : s;
      }
      case "Sum": {
        const s = `${go(ty.left, 2)} + ${go(ty.right, 2)}`;
        return prec > 2 ? `(${s})` : s;
      }
      case "List":
        return `[${go(ty.elem, 0)}]`;
      case "Arrow": {
        const s = `${go(ty.arg, 1)} -> ${go(ty.result, 0)}`;
        return prec > 1 ? `(${s})` : s;
      }
      case "MetaType":
        if (ty.meta.value && !seen.has(ty.meta.id)) {
          seen.add(ty.meta.id);
          return go(ty.meta.value, prec);
        }
        return `?t${ty.meta.id}`;
      default:
        return ty.tag;
    }
  };
  return go(type);
}

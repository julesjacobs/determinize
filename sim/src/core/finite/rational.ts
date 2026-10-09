// Exact rational arithmetic for the finite models, as Lean's `Rat`: BigInt numerator and
// denominator in lowest terms, with a positive denominator. The type and its construction are the
// compiler's, which reads literals into it.
import type { Rational } from "../compiler/rational.ts";
import { add, compare, rational, sub } from "../compiler/rational.ts";

export type { Rational };
export { add, compare, rational, sub };

export const zero: Rational = { num: 0n, den: 1n };
export const one: Rational = { num: 1n, den: 1n };

export function integer(n: number | bigint): Rational {
  return { num: BigInt(n), den: 1n };
}

export function mul(a: Rational, b: Rational): Rational {
  if (a.num === 0n || b.num === 0n) return zero;
  return rational(a.num * b.num, a.den * b.den);
}

/** `a / b`; Lean's `Rat` division by 0 is 0. */
export function div(a: Rational, b: Rational): Rational {
  if (b.num === 0n) return zero;
  return rational(a.num * b.den, a.den * b.num);
}

export function neg(a: Rational): Rational {
  return a.num === 0n ? a : { num: -a.num, den: a.den };
}

export function equal(a: Rational, b: Rational): boolean {
  return a.num === b.num && a.den === b.den;
}

export function isZero(a: Rational): boolean {
  return a.num === 0n;
}

export function lt(a: Rational, b: Rational): boolean {
  return compare(a, b) < 0;
}

export function le(a: Rational, b: Rational): boolean {
  return compare(a, b) <= 0;
}

export function sum(values: Iterable<Rational>): Rational {
  let total = zero;
  for (const value of values) total = add(total, value);
  return total;
}

/** A fraction as `Finite.Export` writes it into `.result.json`: the numerator, and `/den` unless
 * the denominator is 1. */
export function fraction(q: Rational): string {
  return q.den === 1n ? String(q.num) : `${q.num}/${q.den}`;
}

/** About the double nearest to `q`, also where its numerator or denominator exceeds a double's
 * range. */
export function toNumber(q: Rational): number {
  const n = Number(q.num);
  const d = Number(q.den);
  if (Number.isFinite(n) && Number.isFinite(d)) return n / d;
  // Each scaled down to its 64 leading bits, and the quotient scaled back by the difference of the
  // shifts, in two halves, since the power alone may be out of range where the value isn't.
  const sn = Math.max(0, bits(q.num) - 64);
  const sd = Math.max(0, bits(q.den) - 64);
  const quotient = Number(q.num >> BigInt(sn)) / Number(q.den >> BigInt(sd));
  const half = Math.trunc((sn - sd) / 2);
  return quotient * 2 ** half * 2 ** (sn - sd - half);
}

function bits(n: bigint): number {
  return (n < 0n ? -n : n).toString(2).length;
}

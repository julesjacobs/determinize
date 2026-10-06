/** An exact rational number in lowest terms with a positive denominator, as Lean's `Rat`. */
export interface Rational {
  num: bigint;
  den: bigint;
}

function gcd(a: bigint, b: bigint): bigint {
  let x = a < 0n ? -a : a;
  let y = b < 0n ? -b : b;
  while (y !== 0n) [x, y] = [y, x % y];
  return x;
}

export function rational(num: bigint, den = 1n): Rational {
  if (den === 0n) throw new Error("rational with zero denominator");
  const sign = den < 0n ? -1n : 1n;
  const divisor = gcd(num, den) || 1n;
  return { num: (sign * num) / divisor, den: (sign * den) / divisor };
}

export function add(a: Rational, b: Rational): Rational {
  return rational(a.num * b.den + b.num * a.den, a.den * b.den);
}

export function sub(a: Rational, b: Rational): Rational {
  return rational(a.num * b.den - b.num * a.den, a.den * b.den);
}

export function compare(a: Rational, b: Rational): number {
  const difference = a.num * b.den - b.num * a.den;
  return difference < 0n ? -1 : difference > 0n ? 1 : 0;
}

/** The double that Lean's runtime computes for a literal: the numerator divided by the
 * denominator, each converted to a double. */
export function toNumber(q: Rational): number {
  return Number(q.num) / Number(q.den);
}

export function format(q: Rational): string {
  return q.den === 1n ? String(q.num) : `${q.num}/${q.den}`;
}

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

/** A terminating decimal when there is one, as Lean's `Pretty` prints literals, else `num/den`. */
export function formatDecimal(q: Rational): string {
  let places = 0;
  let den = q.den;
  while (den % 10n === 0n || den % 2n === 0n || den % 5n === 0n) {
    den /= den % 10n === 0n ? 10n : den % 2n === 0n ? 2n : 5n;
    places++;
  }
  if (den !== 1n) return `${q.num}/${q.den}`;
  if (q.den === 1n) return String(q.num);
  const scaled = (q.num < 0n ? -q.num : q.num) * (10n ** BigInt(places) / q.den);
  const digits = scaled.toString().padStart(places + 1, "0");
  const sign = q.num < 0n ? "-" : "";
  return `${sign}${digits.slice(0, -places)}.${digits.slice(-places)}`;
}

/** A number as the simulator shows it: integers as they are, others with about four digits. */
export function formatNumber(value: number) {
  if (value === Infinity) return "∞";
  if (value === -Infinity) return "-∞";
  if (!Number.isFinite(value)) return "n/a";
  if (Number.isInteger(value) && Math.abs(value) < 100000) return String(value);
  if (Math.abs(value) >= 1000 || Math.abs(value) < 0.001) return value.toExponential(2);
  return Number(value.toFixed(4)).toString();
}

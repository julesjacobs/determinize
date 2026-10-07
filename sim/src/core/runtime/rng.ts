// The random streams of a run: Lean's SplitMix64 generator (`Runtime/Sampling.lean`), one stream
// for E draws and one for G draws, seeded as Lean's `runOutcome` (`Runtime/Eval.lean`) seeds them.
import { SplitMix64, uint64 } from "./sampling.ts";

export type Rng = SplitMix64;

/** The random streams of E draws and of G draws. */
export interface Streams {
  rngE: Rng;
  rngG: Rng;
}

/** The E stream's seed is the run's seed with these bits flipped. */
export const eSeedMask = 0x517cc1b727220a95n;

/** The streams of the run at `seed`, as a UInt64: the E stream starts at `seed xor
 * 0x517cc1b727220a95`, the G stream at `seed`. */
export function makeStreams(seed: number | bigint = 1): Streams {
  const state = uint64(BigInt(seed));
  return { rngE: new SplitMix64(state ^ eSeedMask), rngG: new SplitMix64(state) };
}

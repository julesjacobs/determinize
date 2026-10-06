export class Rng {
  declare seed: number;

  constructor(seed: number) {
    this.seed = seed >>> 0;
  }

  clone(): Rng {
    return new Rng(this.seed);
  }

  next(): number {
    this.seed += 0x6d2b79f5;
    let t = this.seed;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  }

  positive(): number {
    return Math.max(this.next(), 1e-12);
  }
}

/** The random streams of E draws and of G draws. */
export interface Streams {
  rngE: Rng;
  rngG: Rng;
}

export function makeStreams(seed = 1): Streams {
  return {
    rngE: new Rng((seed ^ 0x9e3779b9) >>> 0),
    rngG: new Rng((seed ^ 0x85ebca6b) >>> 0),
  };
}

export function splitSeeds(seed = 1) {
  return {
    eSeed: (seed ^ 0x9e3779b9) >>> 0,
    gSeed: (seed ^ 0x85ebca6b) >>> 0,
  };
}

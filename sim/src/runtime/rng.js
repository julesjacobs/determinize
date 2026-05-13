export class Rng {
  constructor(seed) {
    this.seed = seed >>> 0;
  }

  clone() {
    return new Rng(this.seed);
  }

  next() {
    let t = (this.seed += 0x6d2b79f5);
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  }

  positive() {
    return Math.max(this.next(), 1e-12);
  }
}

export function makeStreams(seed = 1) {
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

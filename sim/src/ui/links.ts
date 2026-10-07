// The Lean definitions that the simulator's outputs link to, by absolute canonical URLs, which also
// work from file:// and from copies of the simulator elsewhere. The URLs are written out, so that
// the deploy's link check finds them in the bundle, and sim/index.html links each of them too.

export const leanLinks = {
  returnProbability:
    "https://julesjacobs.com/determinize/docs/Determinize/Spec/Expectation.html#Determinize.Spec.returnProbability",
  returnedExpectation:
    "https://julesjacobs.com/determinize/docs/Determinize/Spec/Expectation.html#Determinize.Spec.returnedExpectation",
  returnedVariance:
    "https://julesjacobs.com/determinize/docs/Determinize/Spec/Expectation.html#Determinize.Spec.returnedVariance",
};

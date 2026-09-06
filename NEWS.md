# nlFit 0.2.0

Corrections to the memory window, the equivalence bands and the handling of
reference points, plus a symmetric equivalence rule and an unanchored-placement
reading of `firstEstimate`. Version 0.1.0 remains available under the git tag
`v0.1.0` for anyone who needs the earlier predictions.

**Design rule going forward:** extend the model by adding arguments whose
defaults reproduce the current behaviour exactly; never change the meaning of an
existing parameter.

## Changes that move the predictions

* `memoryLength` now keeps exactly the last `memoryLength` estimates. The window
  was one trial too wide. Fits that held `memoryLength` at the number of targets
  are unaffected; any fit that searched `memoryLength` must be re-run.
* The equivalence-band test is inclusive (`>=` / `<=`) rather than strict. A
  repeated target now matches its own earlier estimate.
* `visibleReferencePoints` is required. The two fallbacks that applied when it
  was `NULL` (reference points defaulting to the bounds, and band widths scaled
  by distance from zero) are gone. Passing `c(lowerBound, upperBound)` is not
  numerically identical to the old `NULL` behaviour at intermediate
  `numberSensitivity`, so a fit that relied on the default is a re-fit.
* `firstEstimate` now applies to every unanchored placement, that is, whenever
  both of a target's anchors are reference points, rather than to trial 1 only.
  Bounded fits whose only reference points are the bounds are unchanged.
  Universal fits with more than two labelled points change: within a region that
  holds no remembered estimate every target is placed at
  `eLow + firstEstimate * (eHigh - eLow)` regardless of its value, so with a
  memory window shorter than the target set the fitted function becomes a
  staircase whose step heights are set by `firstEstimate`.
* The equivalence test is symmetric among remembered estimates: two numbers are
  confusable when either falls in the other's range, so confusability no longer
  depends on which was seen first. A target is still compared to a reference
  point in one direction only, which keeps labels perfectly discriminable.

## Changes that do not move the predictions

* `rangeLength` is removed. The anchors are the immediate rank neighbours on
  each side, which is what `rangeLength = 1` did; the offset may become a
  parameter again in the future.
* `firstEstimate = NULL` is documented as the midpoint of the region, which is
  what the code has always computed. The documentation said "random draw".
* Loops are no longer collapsed to 1 for a fixed or once-drawn target order when
  conceptual points are drawn in, so `pIncludeConceptualPoints` is estimable in
  a fixed order. `fillNumberlineParList()` fills `pIncludeConceptualPoints = 0`.
* `pars.n` is required in `getOrdinalNumberlineFit()` and `outputNLfitStats()`.
  It previously fell back to the length of the parameter list, which counted
  bounds and reference vectors as free parameters.
* Hygiene: the bound-order check in `validateNumberlineParameters()` now runs; a
  missing `lowerBound` stops with a message. The out-of-bounds relabel writes the
  value column it matches on. Debugging prints are gone. The parallel back end no
  longer writes `log.txt` into the working directory and stops its cluster on
  exit. `outputNLfitStats()` opens a pdf device only when given a file name and a
  sink only when given a sink file name. The dplyr dependency is replaced with
  `aggregate()`.
* `outputNLfitStats()` records the package version, the run time, the number of
  loops and, if the caller supplies one, the random seed. It gains a `seed`
  argument for that purpose; it does not set the seed itself.
* Package documentation: `DESCRIPTION` is filled in, the license is GPL-3, and
  the examples run.

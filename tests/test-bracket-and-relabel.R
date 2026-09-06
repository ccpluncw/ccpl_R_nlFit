## The anchors are the remembered values on either side of the target, and three
## rules decide which rows those are: the nearest value by absolute distance, the
## earlier of two rows that hold the same value, and, for a target equal to a
## remembered value, the item below it.
library(nlFit)

bounded <- function(targets, ...)
  ordinalNumberLine(targets, upperBound = 100, lowerBound = 0,
                    firstEstimate = NULL, pIncludeConceptualPoints = 0,
                    conceptualReferencePoints = NULL, targetOrder = "fixed", ...)$fEst

## the earlier of two equal remembered values is the anchor. The third and fourth
## 30s both take the FIRST 30's estimate of 40, so both are reported as 35. Taking
## the later duplicate would carry the drift forward and give 33.75 on trial 4,
## and inserting a repeat ahead of its earlier self would give 32.5.
stopifnot(isTRUE(all.equal(
  bounded(c(30, 70, 30, 30), memoryLength = 4, numberSensitivity = 1, accuracyPercent = 0.5,
          visibleReferencePoints = c(0, 100)),
  c(40, 70, 35, 35))))

## a target equal to a remembered value brackets with the item BELOW it, so the
## equal row is the upper anchor and the row beneath it is the lower one. Both
## are matched here, and the response is the mean of their values; bracketing
## above instead would put the equal row and the line's end together and give 80.
stopifnot(isTRUE(all.equal(
  bounded(c(80, 60, 80), memoryLength = 5, numberSensitivity = 0, accuracyPercent = 1,
          visibleReferencePoints = c(0, 25, 100)),
  c(80, 80, 70))))

## the same rule at the top of the line: the target equals the upper bound, so the
## bound is the upper anchor and the remembered 80 the lower one
stopifnot(isTRUE(all.equal(
  bounded(c(80, 60, 100), memoryLength = 5, numberSensitivity = 0, accuracyPercent = 1,
          visibleReferencePoints = c(0, 25, 100)),
  c(80, 80, 90))))

## A target past the end of the line relabels the bound, and the bound keeps the
## new value as well as the new position: 102 is what the fourth trial's accuracy
## component aims at, alongside the remembered 90, so the response is 96. Moving
## the position without the value would leave the bound calling itself 100 and
## give 95.
stopifnot(isTRUE(all.equal(
  bounded(c(102, 80, 90, 100), memoryLength = 2, numberSensitivity = 0.5,
          accuracyPercent = 1, visibleReferencePoints = 50),
  c(100, 100, 91, 96))))

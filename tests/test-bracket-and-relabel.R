#the anchor rules: nearest value, earlier duplicate, and below an equal target
library(nlFit)

bounded <- function(targets, ...)
  ordinalNumberLine(targets, upperBound = 100, lowerBound = 0,
                    firstEstimate = NULL, pConceptual = 0,
                    conceptualReferencePoints = NULL, targetOrder = "fixed", ...)$fEst

#the earlier duplicate anchors: the later one would give 33.75 on trial 4
stopifnot(isTRUE(all.equal(
  bounded(c(30, 70, 30, 30), memoryLength = 4, numberSensitivity = 1, accuracyPercent = 0.5,
          visibleReferencePoints = c(0, 100)),
  c(40, 70, 35, 35))))

#an equal target brackets below: bracketing above would give 80
stopifnot(isTRUE(all.equal(
  bounded(c(80, 60, 80), memoryLength = 5, numberSensitivity = 1e-9, accuracyPercent = 1,
          visibleReferencePoints = c(0, 25, 100)),
  c(80, 80, 70))))

#at the upper bound the bound is the upper anchor, the remembered 80 the lower
stopifnot(isTRUE(all.equal(
  bounded(c(80, 60, 100), memoryLength = 5, numberSensitivity = 1e-9, accuracyPercent = 1,
          visibleReferencePoints = c(0, 25)),
  c(80, 80, 90))))

#a relabelled bound keeps the new value: 102 with the remembered 90 gives 96
stopifnot(isTRUE(all.equal(
  bounded(c(102, 80, 90, 100), memoryLength = 2, numberSensitivity = 0.5,
          accuracyPercent = 1, visibleReferencePoints = 50),
  c(100, 100, 91, 96))))

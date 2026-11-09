#ascending targets give ascending estimates only at ns = 1 and no accuracy
library(nlFit)

targets <- seq(5, 95, by = 5)
r <- ordinalNumberLine(targets, upperBound = 100, lowerBound = 0,
                       firstEstimate = NULL, memoryLength = length(targets),
                       numberSensitivity = 1, accuracyPercent = 0,
                       pConceptual = 0,
                       visibleReferencePoints = c(0, 100),
                       conceptualReferencePoints = NULL, targetOrder = "fixed")

stopifnot(identical(r$target, targets))
stopifnot(all(diff(r$fEst) >= 0))

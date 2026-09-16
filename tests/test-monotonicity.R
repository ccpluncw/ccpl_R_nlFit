## At perfect number sensitivity and no weight on accuracy the model places each
## target between its two rank neighbours, so an ascending presentation order can
## never produce a descending estimate. Monotonicity holds only in this corner:
## below numberSensitivity = 1 the anchors can bracket in value while inverting in
## estimate, and the fitted function is not monotone in general.
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

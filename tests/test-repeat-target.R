## A repeated target falls inside its own earlier equivalence range, so at perfect
## number sensitivity it matches its earlier estimate exactly rather than being
## bracketed with the item below it.
library(nlFit)

r <- ordinalNumberLine(c(30, 70, 30, 30), upperBound = 100, lowerBound = 0,
                       firstEstimate = 0.4, memoryLength = 4,
                       numberSensitivity = 1, accuracyPercent = 0,
                       pIncludeConceptualPoints = 0,
                       visibleReferencePoints = c(0, 100),
                       conceptualReferencePoints = NULL, targetOrder = "fixed")

stopifnot(isTRUE(all.equal(r$fEst, c(40, 70, 40, 40))))

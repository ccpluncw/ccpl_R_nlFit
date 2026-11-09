#firstEstimate places every target in a region with no remembered estimate
library(nlFit)

targets <- c(10, 30, 50, 70, 90, 25, 55, 85)
r <- ordinalNumberLine(targets, upperBound = 100, lowerBound = 0,
                       firstEstimate = 0.9, memoryLength = 2,
                       numberSensitivity = 1, accuracyPercent = 0,
                       pConceptual = 0,
                       visibleReferencePoints = c(20, 40, 60, 80),
                       conceptualReferencePoints = NULL, targetOrder = "fixed")

stopifnot(identical(r$target, targets))
stopifnot(isTRUE(all.equal(r$fEst, c(18, 38, 58, 78, 98, 38, 58, 98))))

#30 and 25 share a region, as do 50 and 55, and 90 and 85
stopifnot(r$fEst[2] == r$fEst[6], r$fEst[3] == r$fEst[7], r$fEst[5] == r$fEst[8])

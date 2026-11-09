#a number is inside its own range, so a repeat matches its earlier estimate
library(nlFit)

r <- ordinalNumberLine(c(30, 70, 30, 30), upperBound = 100, lowerBound = 0,
                       firstEstimate = 0.4, memoryLength = 4,
                       numberSensitivity = 1, accuracyPercent = 0,
                       pConceptual = 0,
                       visibleReferencePoints = c(0, 100),
                       conceptualReferencePoints = NULL, targetOrder = "fixed")

stopifnot(isTRUE(all.equal(r$fEst, c(40, 70, 40, 40))))

#range ends count: a target on a visible point lands on it, not midway at 37.5
r2 <- ordinalNumberLine(c(20, 50), upperBound = 100, lowerBound = 0,
                        firstEstimate = NULL, memoryLength = 2,
                        numberSensitivity = 1, accuracyPercent = 0,
                        pConceptual = 0,
                        visibleReferencePoints = c(0, 50, 100),
                        conceptualReferencePoints = NULL, targetOrder = "fixed")

stopifnot(isTRUE(all.equal(r2$fEst, c(25, 50))))

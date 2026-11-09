#either number in the other's range matches, a double hit uses both anchors
library(nlFit)

run <- function(targets, accuracyPercent)
  ordinalNumberLine(targets, upperBound = 100, lowerBound = 0,
                    firstEstimate = NULL, memoryLength = length(targets),
                    numberSensitivity = 0.2, accuracyPercent = accuracyPercent,
                    pConceptual = 0,
                    visibleReferencePoints = c(0, 100),
                    conceptualReferencePoints = NULL, targetOrder = "fixed")$fEst

#50 (half width 40) reaches 15 and 75, accuracy target is their mean, 45
stopifnot(isTRUE(all.equal(run(c(15, 75, 50), 1), c(15, 75, 45))))
stopifnot(isTRUE(all.equal(run(c(20, 85, 50), 1), c(20, 85, 52.5))))

#the same double hit with symmetric anchors returns the target's own value
stopifnot(isTRUE(all.equal(run(c(15, 85, 50), 1), c(15, 85, 50))))

#the placement is unaffected: the anchors are the same two rows either way
stopifnot(isTRUE(all.equal(run(c(15, 75, 50), 0), c(50, 75, 62.5))))

#a reference point matches one way only, or near-zero ns snaps to the label
snapCheck <- function(targets)
  ordinalNumberLine(targets, upperBound = 100, lowerBound = 0,
                    firstEstimate = NULL, memoryLength = length(targets),
                    numberSensitivity = 1e-9, accuracyPercent = 1,
                    pConceptual = 0,
                    visibleReferencePoints = c(0, 50, 100),
                    conceptualReferencePoints = NULL, targetOrder = "fixed")$fEst

stopifnot(isTRUE(all.equal(snapCheck(c(30, 55, 80)), c(30, 55, 80))))

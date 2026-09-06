## A number falls inside its own range of equivalent numbers: the ends of the
## range count, so a target that sits exactly on an anchor is the same number as
## that anchor. That is what makes a repeated target match its earlier estimate,
## and what makes a target sitting on a reference point take the reference point's
## position.
library(nlFit)

r <- ordinalNumberLine(c(30, 70, 30, 30), upperBound = 100, lowerBound = 0,
                       firstEstimate = 0.4, memoryLength = 4,
                       numberSensitivity = 1, accuracyPercent = 0,
                       pIncludeConceptualPoints = 0,
                       visibleReferencePoints = c(0, 100),
                       conceptualReferencePoints = NULL, targetOrder = "fixed")

stopifnot(isTRUE(all.equal(r$fEst, c(40, 70, 40, 40))))

## A reference point is compared to the target in one direction only, so the only
## way a target can be the same number as one is to fall inside its range - which,
## at perfect sensitivity, means to equal it exactly. The second target sits on the
## visible 50 and is placed there. A range that excluded its own ends would match
## nothing here and would place the target midway between the remembered 20 and
## the 50, at 37.5.
r2 <- ordinalNumberLine(c(20, 50), upperBound = 100, lowerBound = 0,
                        firstEstimate = NULL, memoryLength = 2,
                        numberSensitivity = 1, accuracyPercent = 0,
                        pIncludeConceptualPoints = 0,
                        visibleReferencePoints = c(0, 50, 100),
                        conceptualReferencePoints = NULL, targetOrder = "fixed")

stopifnot(isTRUE(all.equal(r2$fEst, c(25, 50))))

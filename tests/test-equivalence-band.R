## Two numbers are treated as the same when either one falls in the other's range
## of equivalent numbers. A target near the middle of the line has a wide range
## because it is far from every reference point, so it can reach anchors whose own
## ranges are narrow and which are not confusable with each other. When it reaches
## both anchors the estimate is the midpoint of their estimates and the target of
## the accuracy component is the mean of their values.
library(nlFit)

run <- function(targets, accuracyPercent)
  ordinalNumberLine(targets, upperBound = 100, lowerBound = 0,
                    firstEstimate = NULL, memoryLength = length(targets),
                    numberSensitivity = 0.2, accuracyPercent = accuracyPercent,
                    pIncludeConceptualPoints = 0,
                    visibleReferencePoints = c(0, 100),
                    conceptualReferencePoints = NULL, targetOrder = "fixed")$fEst

## 50 has a half width of 40 and reaches both 15 and 75, whose half widths are 12
## and 20 and which do not reach each other. Accuracy target = mean(15, 75) = 45.
stopifnot(isTRUE(all.equal(run(c(15, 75, 50), 1), c(15, 75, 45))))
stopifnot(isTRUE(all.equal(run(c(20, 85, 50), 1), c(20, 85, 52.5))))

## the same double hit with symmetric anchors returns the target's own value
stopifnot(isTRUE(all.equal(run(c(15, 85, 50), 1), c(15, 85, 50))))

## the placement is unaffected: the anchors are the same two rows either way
stopifnot(isTRUE(all.equal(run(c(15, 75, 50), 0), c(50, 75, 62.5))))

## The reverse half of the test applies only to remembered estimates: a reference
## point is always discriminable from the target, however coarse the target's own
## resolution. Without that restriction, at numberSensitivity = 0 every target
## would be treated as the same number as its nearest label and, with all the
## weight on accuracy, would be reported as that label's value.
snapCheck <- function(targets)
  ordinalNumberLine(targets, upperBound = 100, lowerBound = 0,
                    firstEstimate = NULL, memoryLength = length(targets),
                    numberSensitivity = 0, accuracyPercent = 1,
                    pIncludeConceptualPoints = 0,
                    visibleReferencePoints = c(0, 50, 100),
                    conceptualReferencePoints = NULL, targetOrder = "fixed")$fEst

stopifnot(isTRUE(all.equal(snapCheck(c(30, 55, 80)), c(30, 55, 80))))

## Fixed-order runs are deterministic, so their estimates pin the whole process:
## the memory window, the bracket rule, the relabelling of a bound the target
## reaches or passes, the equivalence test, the placement and the accuracy blend.
## The expected values below are the reference output of the model.
library(nlFit)

## bounded, perfect sensitivity, no weight on accuracy: each target lands midway
## between its rank neighbours, and the first one at firstEstimate of the line
b <- ordinalNumberLine(c(60, 20, 90, 45, 75, 10), upperBound = 100, lowerBound = 0,
                       firstEstimate = 0.4, memoryLength = 6, numberSensitivity = 1,
                       accuracyPercent = 0, pIncludeConceptualPoints = 0,
                       visibleReferencePoints = c(0, 100), conceptualReferencePoints = NULL,
                       targetOrder = "fixed")
stopifnot(isTRUE(all.equal(b$fEst, c(40, 20, 70, 30, 55, 10))))

## universal: targets on a label, past each screen edge and on the far side of a
## label. The edge targets relabel a bound and the responses are held on screen.
u <- ordinalNumberLine(c(-0.8, 0.3, 1.4, -1.2, 0.8, -0.5), upperBound = 1, lowerBound = -1,
                       firstEstimate = 0.3, memoryLength = 6, numberSensitivity = 1,
                       accuracyPercent = 0.25, pIncludeConceptualPoints = 0,
                       visibleReferencePoints = c(-0.8, 0.8), conceptualReferencePoints = NULL,
                       targetOrder = "fixed")
stopifnot(isTRUE(all.equal(u$fEst, c(-0.8, -0.165, 1, -1, 0.8, -0.725))))

## conceptual points held with certainty are reference points like any other, and
## a short memory window drops the earliest estimates back out of the set
cp <- ordinalNumberLine(c(60, 20, 90, 45, 75, 10), upperBound = 100, lowerBound = 0,
                        firstEstimate = NULL, memoryLength = 3, numberSensitivity = 1,
                        accuracyPercent = 0, pIncludeConceptualPoints = 1,
                        visibleReferencePoints = c(0, 100), conceptualReferencePoints = c(25, 50),
                        targetOrder = "fixed")
stopifnot(isTRUE(all.equal(cp$fEst, c(75, 12.5, 87.5, 37.5, 68.75, 12.5))))

## the loops collapse to 1 only when the run has nothing left to average over
parList <- list(upperBound = 100, lowerBound = 0, firstEstimate = 0.4, memoryLength = 6,
                numberSensitivity = 1, accuracyPercent = 0, pIncludeConceptualPoints = 0,
                visibleReferencePoints = c(0, 100), conceptualReferencePoints = NULL,
                targetOrder = "fixed")
s <- ordinalNumberLineSim(c(60, 20, 90, 45, 75, 10), parList, loops = 50)
stopifnot(isTRUE(all.equal(s$fEst, c(10, 20, 30, 40, 55, 70))))

## with a conceptual point drawn in, a fixed order still varies from loop to loop
parList$conceptualReferencePoints <- 50
parList$pIncludeConceptualPoints <- 0.5
set.seed(2)
s1 <- ordinalNumberLineSim(c(60, 20, 90, 45, 75, 10), parList, loops = 200)
set.seed(3)
s2 <- ordinalNumberLineSim(c(60, 20, 90, 45, 75, 10), parList, loops = 200)
stopifnot(!isTRUE(all.equal(s1$fEst, s2$fEst)))

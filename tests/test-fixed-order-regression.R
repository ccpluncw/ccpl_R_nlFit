#fixed-order runs are deterministic, so the expected values pin the process
library(nlFit)

#perfect ns and no accuracy: each target lands midway between rank neighbours
b <- ordinalNumberLine(c(60, 20, 90, 45, 75, 10), upperBound = 100, lowerBound = 0,
                       firstEstimate = 0.4, memoryLength = 6, numberSensitivity = 1,
                       accuracyPercent = 0, pConceptual = 0,
                       visibleReferencePoints = c(0, 100), conceptualReferencePoints = NULL,
                       targetOrder = "fixed")
stopifnot(isTRUE(all.equal(b$fEst, c(40, 20, 70, 30, 55, 10))))

#universal: targets past each edge relabel a bound, responses stay on screen
u <- ordinalNumberLine(c(-0.8, 0.3, 1.4, -1.2, 0.8, -0.5), upperBound = 1, lowerBound = -1,
                       firstEstimate = 0.3, memoryLength = 6, numberSensitivity = 1,
                       accuracyPercent = 0.25, pConceptual = 0,
                       visibleReferencePoints = c(-0.8, 0.8), conceptualReferencePoints = NULL,
                       targetOrder = "fixed")
stopifnot(isTRUE(all.equal(u$fEst, c(-0.8, -0.165, 1, -1, 0.8, -0.725))))

#certain conceptual points anchor, a short window drops the early estimates
cp <- ordinalNumberLine(c(60, 20, 90, 45, 75, 10), upperBound = 100, lowerBound = 0,
                        firstEstimate = NULL, memoryLength = 3, numberSensitivity = 1,
                        accuracyPercent = 0, pConceptual = 1,
                        visibleReferencePoints = c(0, 100), conceptualReferencePoints = c(25, 50),
                        targetOrder = "fixed")
stopifnot(isTRUE(all.equal(cp$fEst, c(75, 12.5, 87.5, 37.5, 68.75, 12.5))))

#the loops collapse to 1 only when the run has nothing left to average over
parList <- list(upperBound = 100, lowerBound = 0, firstEstimate = 0.4, memoryLength = 6,
                numberSensitivity = 1, accuracyPercent = 0, pConceptual = 0,
                visibleReferencePoints = c(0, 100), conceptualReferencePoints = NULL,
                targetOrder = "fixed")
s <- ordinalNumberLineSim(c(60, 20, 90, 45, 75, 10), parList, loops = 50)
stopifnot(isTRUE(all.equal(s$fEst, c(10, 20, 30, 40, 55, 70))))

#with a conceptual point drawn in, a fixed order still varies from loop to loop
parList$conceptualReferencePoints <- 50
parList$pConceptual <- 0.5
set.seed(2)
s1 <- ordinalNumberLineSim(c(60, 20, 90, 45, 75, 10), parList, loops = 200)
set.seed(3)
s2 <- ordinalNumberLineSim(c(60, 20, 90, 45, 75, 10), parList, loops = 200)
stopifnot(!isTRUE(all.equal(s1$fEst, s2$fEst)))

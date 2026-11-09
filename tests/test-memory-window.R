#trial 4 pins the window edge: trial 1 drops out at k = 2, stays at k = 3
library(nlFit)

targets <- c(10, 90, 50, 45)
run <- function(m) ordinalNumberLine(targets, upperBound = 100, lowerBound = 0,
                                     firstEstimate = NULL, memoryLength = m,
                                     numberSensitivity = 1, accuracyPercent = 0,
                                     pConceptual = 0,
                                     visibleReferencePoints = c(0, 100),
                                     conceptualReferencePoints = NULL,
                                     targetOrder = "fixed")$fEst

m2 <- run(2)
m3 <- run(3)

#trials 1-3 cannot see beyond their own history, so both windows agree there
stopifnot(isTRUE(all.equal(m2[1:3], c(50, 75, 62.5))))
stopifnot(isTRUE(all.equal(m3[1:3], c(50, 75, 62.5))))

#trial 4 low anchor: the bound at k = 2, the trial 1 estimate at k = 3
stopifnot(isTRUE(all.equal(m2[4], 31.25)))
stopifnot(isTRUE(all.equal(m3[4], 56.25)))

## memoryLength = k means the remembered set on trial t is the estimates from
## trials t-k through t-1 plus the reference points. On trial 4 with k = 2 the
## trial 1 estimate has dropped out; with k = 3 it is still there. The two runs
## are identical up to trial 4, so the trial 4 estimate pins the window edge.
library(nlFit)

targets <- c(10, 90, 50, 45)
run <- function(m) ordinalNumberLine(targets, upperBound = 100, lowerBound = 0,
                                     firstEstimate = NULL, memoryLength = m,
                                     numberSensitivity = 1, accuracyPercent = 0,
                                     pIncludeConceptualPoints = 0,
                                     visibleReferencePoints = c(0, 100),
                                     conceptualReferencePoints = NULL,
                                     targetOrder = "fixed")$fEst

m2 <- run(2)
m3 <- run(3)

## trials 1-3 cannot see beyond their own history, so both windows agree there
stopifnot(isTRUE(all.equal(m2[1:3], c(50, 75, 62.5))))
stopifnot(isTRUE(all.equal(m3[1:3], c(50, 75, 62.5))))

## trial 4: with k = 2 the anchors are the lower bound and the trial 3 estimate;
## with k = 3 the trial 1 estimate is still remembered and becomes the low anchor
stopifnot(isTRUE(all.equal(m2[4], 31.25)))
stopifnot(isTRUE(all.equal(m3[4], 56.25)))

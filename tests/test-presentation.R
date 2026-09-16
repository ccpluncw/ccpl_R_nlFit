## A target value presented more than once gets one row per presentation. The
## k-th presentation sits earlier in the run on average than the (k+1)-th, so the
## two carry different estimates; averaging the presentations of a value back
## together reproduces the single number a per-value average would have given.
library(nlFit)

targets <- c(30, 70, 30, 55, 30)
parList <- list(upperBound = 100, lowerBound = 0, firstEstimate = 0.4, memoryLength = 5,
                numberSensitivity = 0.7, accuracyPercent = 0.2, pConceptual = 0,
                visibleReferencePoints = c(0, 100), conceptualReferencePoints = NULL,
                targetOrder = "random")

## one run: the presentation column counts the occurrences in presented order
run <- ordinalNumberLine(targets, upperBound = 100, lowerBound = 0, firstEstimate = 0.4,
                         memoryLength = 5, numberSensitivity = 0.7, accuracyPercent = 0.2,
                         pConceptual = 0, visibleReferencePoints = c(0, 100),
                         conceptualReferencePoints = NULL, targetOrder = "fixed")
stopifnot(identical(run$target, targets))
stopifnot(identical(run$presentation, c(1L, 1L, 2L, 1L, 3L)))

## the simulation keeps one row per (value, presentation)
set.seed(11)
sim <- ordinalNumberLineSim(targets, parList, loops = 200)
stopifnot(nrow(sim) == length(targets))
stopifnot(identical(sim$target, c(30, 30, 30, 55, 70)))
stopifnot(identical(sim$presentation, c(1L, 2L, 3L, 1L, 1L)))

## averaging the presentations of a value back together gives the per-value mean.
## Every presentation is averaged over the same number of loops, so the mean of
## the presentation means is the mean over all the trials of that value.
set.seed(11)
byTrial <- replicate(200, ordinalNumberLine(targets, upperBound = 100, lowerBound = 0,
                                            firstEstimate = 0.4, memoryLength = 5,
                                            numberSensitivity = 0.7, accuracyPercent = 0.2,
                                            pConceptual = 0,
                                            visibleReferencePoints = c(0, 100),
                                            conceptualReferencePoints = NULL,
                                            targetOrder = "random"), simplify = FALSE)
byTrial <- do.call(rbind, byTrial)
perValue <- aggregate(byTrial["fEst"], by = list(target = byTrial$target), FUN = mean)
collapsed <- aggregate(sim["fEst"], by = list(target = sim$target), FUN = mean)
stopifnot(isTRUE(all.equal(perValue$fEst, collapsed$fEst)))

## the presentations of a repeated value are not all the same number
stopifnot(length(unique(sim$fEst[sim$target == 30])) > 1)

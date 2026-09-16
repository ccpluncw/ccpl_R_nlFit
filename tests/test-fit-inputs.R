## The fitting functions take the participant's data and a parameter list. A
## trial-level dataset has to number its presentations the way the simulation
## does, and a parameter list may leave out anything that has a default.
library(nlFit)

fitIt <- function(d) getOrdinalNumberlineFit(d, upperBound = 100, lowerBound = 0,
                        visibleReferencePoints = c(0, 100), firstEstimate = 0.5,
                        memoryLength = 5, numberSensitivity = 0.8, accuracyPercent = 0.2,
                        loops = 20, pars.n = 3)

df <- data.frame(target = c(10, 25, 40, 25, 70), presentation = c(1, 1, 1, 2, 1),
                 estimate = c(18, 30, 44, 26, 68))

## numbered the way the simulation numbers them, every row is scored
stopifnot(is.finite(fitIt(df)))

## numbering that starts at 2, or leaves a gap, matches fewer rows than the data
## holds; a fit computed on part of the data is an error rather than a score
for(numbering in list(c(2, 3, 2, 5, 2), c(1, 1, 1, 3, 1))) {
  bad <- df
  bad$presentation <- numbering
  out <- try(fitIt(bad), silent = TRUE)
  stopifnot(inherits(out, "try-error"))
  stopifnot(grepl("presentation", conditionMessage(attr(out, "condition"))))
}

## the same data without a presentation column is an averaged dataset: the
## presentations of a repeated value are averaged together and every row scores
stopifnot(is.finite(fitIt(df[, c("target", "estimate")])))

## a parameter list may leave out anything that has a default
avg <- data.frame(target = c(10, 30, 50, 70), estimate = c(18, 33, 48, 66))
full <- list(upperBound = 100, lowerBound = 0, firstEstimate = 0.5, memoryLength = 4,
             numberSensitivity = 0.8, accuracyPercent = 0.2, pConceptual = 0.5,
             visibleReferencePoints = c(0, 100), conceptualReferencePoints = 50,
             targetOrder = "fixed")
for(missing in c("lowerBound", "numberSensitivity", "accuracyPercent",
                 "pConceptual", "targetOrder")) {
  partial <- full
  partial[[missing]] <- NULL
  stopifnot(all(is.finite(ordinalNumberLineSim(avg$target, partial, loops = 5)$fEst)))
}

## the bounds of the line and the reference points have no default and say so
for(missing in c("upperBound", "visibleReferencePoints")) {
  partial <- full
  partial[[missing]] <- NULL
  out <- try(ordinalNumberLineSim(avg$target, partial, loops = 5), silent = TRUE)
  stopifnot(inherits(out, "try-error"))
  stopifnot(grepl(missing, conditionMessage(attr(out, "condition"))))
}

## the validator reads an absent lowerBound as 0, the same default the arguments
## carry, so the bound-order check still runs on it
stopifnot(validateNumberlineParameters(list(upperBound = 100)))
stopifnot(!validateNumberlineParameters(list(upperBound = -10)))
stopifnot(!validateNumberlineParameters(list(upperBound = 0, lowerBound = 100)))
stopifnot(validateNumberlineParameters(list(upperBound = 100, lowerBound = 0)))

## targets have to be numbers the process can place on a line
for(bad in list(as.factor(c(10, 30)), c("10", "30"), c(10, NA), c(10, Inf))) {
  out <- try(ordinalNumberLineSim(bad, full, loops = 5), silent = TRUE)
  stopifnot(inherits(out, "try-error"))
  stopifnot(grepl("targets", conditionMessage(attr(out, "condition"))))
}
## no targets is not an error: there is simply nothing to estimate
stopifnot(nrow(ordinalNumberLineSim(numeric(0), full, loops = 5)) == 0)

## the data a fit is scored against needs rows, and numbers in both columns
statFit <- function(d) getOrdinalNumberlineFit(d, upperBound = 100, lowerBound = 0,
                          visibleReferencePoints = c(0, 100), loops = 5, pars.n = 3)
badData <- list(avg[0, ],
                data.frame(target = as.factor(c(10, 30)), estimate = c(18, 33)),
                data.frame(target = c(10, 30), estimate = c("18", "33")),
                data.frame(notTarget = c(10, 30), estimate = c(18, 33)))
for(d in badData) {
  stopifnot(inherits(try(statFit(d), silent = TRUE), "try-error"))
  pdf(file.path(tempdir(), "nlFitTest.pdf"))
  out <- try(outputNLfitStats(d, full, pars.n = 3, loops = 5), silent = TRUE)
  dev.off()
  stopifnot(inherits(out, "try-error"))
}

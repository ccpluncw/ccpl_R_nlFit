#standard error of firstEstimate: formula, edges, random stream, reports
library(nlFit)

nearly <- function(a, b, tolerance = 1e-6) isTRUE(all.equal(a, b, tolerance = tolerance))

se <- function(pars, data, accuracyFree, loops = 1, h = 0.01) {
	statList <- c(list(upperBound = 100, lowerBound = 0,
					   visibleReferencePoints = c(0, 100),
					   conceptualReferencePoints = NULL), pars)
	sim <- ordinalNumberLineSim(data$target, statList, loops = loops)
	fitted <- nlFit:::nlMatchFitted(sim, data, "target", "presentation")
	nlFit:::nlFirstEstimateSE(statList, data, fitted, accuracyFree, "target",
							  "estimate", "presentation", loops, h)
}

#one target has a slope of (1 - ap) * line length
pars <- list(firstEstimate = 0.3, memoryLength = 1, accuracyPercent = 0.5,
			 numberSensitivity = 1, pConceptual = 0, targetOrder = "fixed")
stopifnot(nearly(se(pars, data.frame(target = 40, estimate = 39), FALSE), 4 / 50))

#with accuracy free the result is the fe entry of the inverse information
targets <- c(12, 80, 33, 57, 91, 5, 68)
pars$memoryLength <- 2
truth <- ordinalNumberLine(targets, 100, 0, firstEstimate = 0.3, memoryLength = 2,
						   numberSensitivity = 1, accuracyPercent = 0.5, pConceptual = 0,
						   visibleReferencePoints = c(0, 100),
						   conceptualReferencePoints = NULL, targetOrder = "fixed")
person <- data.frame(target = targets,
					 estimate = truth$fEst[match(targets, truth$target)] +
						 c(3, -2, 4, -1, 2, -3, 1))
predicted <- function(fe, ap) {
	s <- ordinalNumberLine(targets, 100, 0, firstEstimate = fe, memoryLength = 2,
						   numberSensitivity = 1, accuracyPercent = ap, pConceptual = 0,
						   visibleReferencePoints = c(0, 100),
						   conceptualReferencePoints = NULL, targetOrder = "fixed")
	s$fEst[match(sort(targets), s$target)]
}
slopes <- cbind((predicted(0.31, 0.5) - predicted(0.29, 0.5)) / 0.02,
				(predicted(0.3, 0.51) - predicted(0.3, 0.49)) / 0.02)
ordered <- person[order(person$target), ]
expected <- sqrt(mean((ordered$estimate - predicted(0.3, 0.5))^2) *
				 solve(crossprod(slopes))[1, 1])
stopifnot(nearly(se(pars, person, TRUE), expected))

#the step size does not drive the answer when the loops are random
noisy <- modifyList(pars, list(numberSensitivity = 0.6, targetOrder = "random"))
seeded <- function(h) { set.seed(3); se(noisy, person, TRUE, loops = 200, h = h) }
base <- seeded(0.01)
stopifnot(abs(seeded(0.005) / base - 1) < 0.1, abs(seeded(0.02) / base - 1) < 0.1)

#at ap 1 the data say nothing about firstEstimate
stopifnot(identical(se(modifyList(pars, list(accuracyPercent = 1)), person, TRUE), Inf),
		  identical(se(modifyList(pars, list(accuracyPercent = 1)), person, FALSE), Inf))

#the edges of the domain use a one-sided step
stopifnot(is.finite(se(modifyList(pars, list(firstEstimate = 0)), person, TRUE)),
		  is.finite(se(modifyList(pars, list(firstEstimate = 1)), person, TRUE)),
		  is.finite(se(modifyList(pars, list(accuracyPercent = 0)), person, TRUE)))

#a perfect fit has no error to carry
exact <- data.frame(target = targets,
					estimate = truth$fEst[match(targets, truth$target)])
stopifnot(nearly(se(pars, exact, TRUE), 0))

#the caller's random stream is untouched
statList <- c(list(upperBound = 100, lowerBound = 0, visibleReferencePoints = c(0, 100),
				   conceptualReferencePoints = NULL), noisy)
fitted <- data.frame(estimate = person$estimate, fEst = person$estimate + 2)
callSE <- function() nlFit:::nlFirstEstimateSE(statList, person, fitted, TRUE, "target",
											   "estimate", "presentation", 2)
set.seed(9)
nextDraw <- runif(1)
set.seed(9)
invisible(callSE())
stopifnot(identical(runif(1), nextDraw))

#the seed follows the stream: same stream same result, new stream new result
set.seed(21)
firstCall <- callSE()
set.seed(21)
repeated <- callSE()
set.seed(22)
stopifnot(identical(firstCall, repeated), callSE() != firstCall)

if(exists(".Random.seed", envir = globalenv())) rm(".Random.seed", envir = globalenv())
invisible(callSE())
stopifnot(!exists(".Random.seed", envir = globalenv()))

#what nlFit() reports
if(requireNamespace("smartGridSearch2", quietly = TRUE)) {
	line <- c(12, 80, 33, 57, 91, 5, 68, 44, 22, 74)
	set.seed(4)
	sim <- ordinalNumberLineSim(line, list(upperBound = 100, lowerBound = 0,
										   visibleReferencePoints = c(0, 100),
										   firstEstimate = 0.3, accuracyPercent = 0.3,
										   numberSensitivity = 0.7, targetOrder = "random"),
								loops = 100)
	data <- data.frame(target = sim$target, estimate = sim$fEst + rnorm(nrow(sim), 0, 3))
	settings <- list(multicore = FALSE, numLoops = 8, numIntervals = 5, optParamListN = 3,
					 optBoundLoops = 1, loops = 5, finalLoops = 20)
	run <- function(...) {
		kept <- NULL
		invisible(capture.output(kept <- nlFit(data, upperBound = 100,
											   visibleReferencePoints = c(0, 100),
											   search = "quick", control = settings, ...)))
		kept
	}

	free <- run(free = c("firstEstimate", "accuracyPercent"))
	row <- as.data.frame(free)
	shown <- capture.output(print(free))
	stopifnot(free$parameters$se[1] > 0, all(is.na(free$parameters$se[-1])),
			  identical(row$firstEstimate.se, free$parameters$se[1]),
			  grepl("SE [0-9]", shown[grepl("firstEstimate", shown)]),
			  sum(grepl(" SE ", shown)) == 1)

	#a fixed firstEstimate has no standard error
	held <- run(free = "accuracyPercent", firstEstimate = 0.3)
	stopifnot(all(is.na(held$parameters$se)), is.na(as.data.frame(held)$firstEstimate.se),
			  !any(grepl(" SE ", capture.output(print(held)))))

	#accuracy fixed uses firstEstimate alone
	alone <- run(free = "firstEstimate", accuracyPercent = 0.3, targetOrder = "fixed")
	statAlone <- c(list(upperBound = 100, lowerBound = 0, visibleReferencePoints = c(0, 100),
						conceptualReferencePoints = NULL, targetOrder = "fixed"),
				   as.list(setNames(alone$parameters$value, alone$parameters$parameter)))
	statAlone$firstEstimate <- alone$parameters$value[1]
	stopifnot(nearly(alone$parameters$se[1],
					 nlFit:::nlFirstEstimateSE(statAlone, data, alone$fitted, FALSE, "target",
											   "estimate", "presentation", 20)))

	#the report file carries one line for it
	report <- tempfile(fileext = ".txt")
	reported <- run(free = c("firstEstimate", "accuracyPercent"), sinkFilename = report)
	lines <- readLines(report)
	stopifnot(sum(grepl("^First Estimate = .*\\(SE = [0-9.]+\\)", lines)) == 1)
	heldReport <- tempfile(fileext = ".txt")
	invisible(run(free = "accuracyPercent", firstEstimate = 0.3, sinkFilename = heldReport))
	stopifnot(!any(grepl("SE =", readLines(heldReport))))
	unlink(c(report, heldReport))

	#trial-level data work the same way
	data <- data.frame(target = line, presentation = 1, estimate = data$estimate[match(line, data$target)])
	trial <- run(free = c("firstEstimate", "accuracyPercent"))
	stopifnot(is.finite(trial$parameters$se[1]), isTRUE(trial$columns$trialLevel))

	#a fit with no se column still prints and converts
	old <- free
	old$parameters$se <- NULL
	invisible(capture.output(print(old)))
	stopifnot(is.na(as.data.frame(old)$firstEstimate.se))
}

#the report function computes it only when told firstEstimate was searched
direct <- data.frame(target = targets, estimate = person$estimate)
directList <- c(list(upperBound = 100, lowerBound = 0, visibleReferencePoints = c(0, 100),
					 conceptualReferencePoints = NULL), pars)
withSE <- outputNLfitStats(direct, directList, pars.n = 2, loops = 1,
						   free = c("firstEstimate", "accuracyPercent"))
noSE <- outputNLfitStats(direct, directList, pars.n = 2, loops = 1)
stopifnot(nearly(withSE$runStats$fitStats$firstEstimateSE, expected),
		  is.na(noSE$runStats$fitStats$firstEstimateSE))

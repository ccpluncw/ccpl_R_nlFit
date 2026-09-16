## nlFit() wraps the objective, the search and the fit report in one call. What it
## has to get right before any of that runs is the bookkeeping: which parameters
## are free, whether the ranges it was handed are inside the domains of the model,
## and whether the settings it was handed are settings at all. The search itself is
## in a Suggests package, so every test that needs one is skipped when it is
## absent. The search tests run at the "quick" preset with the effort cut further,
## because what is under test is the wiring, not the precision of the estimates.
library(nlFit)

haveSGS <- requireNamespace("smartGridSearch2", quietly = TRUE)

errorFrom <- function(expr) tryCatch({ force(expr); NA_character_ },
									 error = function(e) conditionMessage(e))
warningFrom <- function(expr) tryCatch({ force(expr); NA_character_ },
									   warning = function(w) conditionMessage(w))

## a participant simulated from the model, so the search has something with a
## recoverable answer in it
truth <- list(upperBound = 100, lowerBound = 0,
			  visibleReferencePoints = c(5, 50), firstEstimate = 0.35,
			  accuracyPercent = 0.45, numberSensitivity = 0.75,
			  targetOrder = "random")
set.seed(2)
sim <- ordinalNumberLineSim(c(3, 6, 8, 10, 13, 15, 18, 22, 26, 30,
							  38, 45, 55, 62, 70, 80, 90),
							truth, loops = 300)
person <- data.frame(target = sim$target,
					 estimate = round(pmin(pmax(sim$fEst + rnorm(nrow(sim), 0, 3), 0), 100), 1))

## the settings that make a test finish in seconds. They are far below anything a
## reported fit would use.
tiny <- list(multicore = FALSE, numLoops = 12, numIntervals = 8,
			 optParamListN = 3, optBoundLoops = 1, loops = 10, finalLoops = 10)

fitPerson <- function(control = tiny, ...) {
	nlFit(person, upperBound = 100, lowerBound = 0, visibleReferencePoints = c(5, 50),
		  search = "quick", control = control, ...)
}


## ---- the search package is named, not assumed --------------------------------

## without smartGridSearch2 there is no search, so the call has to say how to get
## it rather than failing somewhere inside. Nothing else in this file can run.
if(!haveSGS) {
	message("smartGridSearch2 is not installed: the search tests are skipped.")
	msg <- errorFrom(nlFit(person, upperBound = 100, lowerBound = 0,
						   visibleReferencePoints = c(5, 50)))
	stopifnot(grepl('remotes::install_github("ccpluncw/ccpl_R_smartGridSearch2")',
					msg, fixed = TRUE))
}

if(haveSGS) {

## ---- what free will and will not accept --------------------------------------

## a range may narrow a parameter's domain but not widen it. A proportion outside
## [0, 1] is rejected at both ends, and the message names the parameter.
msg <- errorFrom(fitPerson(free = list(numberSensitivity = c(-0.5, 1))))
stopifnot(grepl("numberSensitivity", msg), grepl("domain", msg))
msg <- errorFrom(fitPerson(free = list(pConceptual = c(0, 1.5))))
stopifnot(grepl("pConceptual", msg), grepl("domain", msg))

## memoryLength counts remembered estimates, so a fractional bound is not a
## narrower range, it is a different kind of number
msg <- errorFrom(fitPerson(free = list(memoryLength = c(1.5, 10))))
stopifnot(grepl("memoryLength", msg), grepl("whole numbers", msg))

## its step is a property of the model and is not the caller's to set
msg <- errorFrom(fitPerson(free = list(memoryLength = c(1, 10, 2))))
stopifnot(grepl("memoryLength", msg), grepl("step", msg))

## a range longer than the target set is outside the domain too
msg <- errorFrom(fitPerson(free = list(memoryLength = c(1, nrow(person) + 5))))
stopifnot(grepl("memoryLength", msg), grepl("domain", msg))

## a name the model has no parameter for
msg <- errorFrom(fitPerson(free = c("firstEstimate", "slope")))
stopifnot(grepl("slope", msg))

## nothing to search is not an empty search, it is a different function
stopifnot(grepl("nothing to fit", errorFrom(fitPerson(free = character(0)))),
		  grepl("nothing to fit", errorFrom(fitPerson(free = NULL))))


## ---- what control will and will not accept -----------------------------------

## a setting the search does not have is a typo, and a typo that is dropped
## silently is a fit run at settings the caller did not ask for
msg <- errorFrom(fitPerson(control = list(numLoops = 12, numLops = 40)))
stopifnot(grepl("numLops", msg), grepl("not a search setting", msg))

## the two loop counts are settings of this function rather than of the search,
## and both are accepted
stopifnot(all(c("loops", "finalLoops", "numLoops", "multicore") %in%
			  nlFit:::nlFitControlNames()))


## ---- a parameter cannot be both searched and given ---------------------------

## the searched value would overwrite the given one, and a caller reading the
## result would take the returned value for the one they set
msg <- warningFrom(fitPerson(free = c("accuracyPercent"), accuracyPercent = 0.4))
stopifnot(grepl("accuracyPercent", msg), grepl("ignored", msg))


## ---- pars.n is counted, never supplied ---------------------------------------

## the number of free parameters is what the information criteria are penalised
## by, so it has to follow free in both of free's forms
fitVector <- fitPerson(free = c("firstEstimate", "accuracyPercent"))
stopifnot(fitVector$fitStats$pars.n == 2,
		  sum(fitVector$parameters$free) == 2,
		  identical(fitVector$parameters$parameter[fitVector$parameters$free],
					c("firstEstimate", "accuracyPercent")))

fitList <- fitPerson(free = list(firstEstimate = c(0, 1),
								 accuracyPercent = c(0.2, 0.8),
								 numberSensitivity = c(0.5, 1, 0.01)))
stopifnot(fitList$fitStats$pars.n == 3,
		  sum(fitList$parameters$free) == 3)

## a narrowed range is the range that was searched, and it is reported as such
searched <- fitList$parameters[fitList$parameters$parameter == "accuracyPercent", ]
stopifnot(searched$lower == 0.2, searched$upper == 0.8,
		  searched$value >= 0.2, searched$value <= 0.8)

## a fixed parameter has no range, and memoryLength left alone is the number of
## rows fitted
fixed <- fitList$parameters[fitList$parameters$parameter == "memoryLength", ]
stopifnot(!fixed$free, is.na(fixed$lower), is.na(fixed$upper), fixed$value == nrow(person))


## ---- a fixed value outside its domain stops before the search ----------------

msg <- errorFrom(fitPerson(free = c("firstEstimate"), numberSensitivity = 1.4))
stopifnot(grepl("numberSensitivity", msg))


## ---- one row per fit ---------------------------------------------------------

row <- as.data.frame(fitList)
stopifnot(nrow(row) == 1,
		  all(c("firstEstimate", "memoryLength", "accuracyPercent", "numberSensitivity",
				"pConceptual", "r2", "BIC", "AIC", "pars.n") %in% names(row)),
		  nrow(rbind(as.data.frame(fitVector), row)) == 2)

## the object carries the fitted values and prints without error
stopifnot(nrow(fitList$fitted) == nrow(person), "fEst" %in% names(fitList$fitted))
invisible(capture.output(print(fitList)))


## ---- the reported fit is the fit of the reported parameters ------------------

## with a fixed target order and no conceptual points the simulation is
## deterministic, so the fit report can be recomputed by hand and has to agree to
## the digit. outputNLfitStats' own seed argument records the draw; it is passed
## here only so the comparison names the run it is comparing against.
fitFixed <- nlFit(person, upperBound = 100, lowerBound = 0,
				  visibleReferencePoints = c(5, 50),
				  free = c("firstEstimate", "accuracyPercent"),
				  targetOrder = "fixed", search = "quick", control = tiny)
byHand <- outputNLfitStats(person,
						   statList = list(upperBound = 100, lowerBound = 0,
										   visibleReferencePoints = c(5, 50),
										   conceptualReferencePoints = NULL,
										   targetOrder = "fixed",
										   firstEstimate = fitFixed$parameters$value[1],
										   memoryLength = fitFixed$parameters$value[2],
										   accuracyPercent = fitFixed$parameters$value[3],
										   numberSensitivity = fitFixed$parameters$value[4],
										   pConceptual = fitFixed$parameters$value[5]),
						   pars.n = fitFixed$fitStats$pars.n, loops = tiny$finalLoops,
						   seed = 1)
stopifnot(byHand$runStats$fitStats$r2 == fitFixed$fitStats$r2,
		  byHand$runStats$fitStats$BIC == fitFixed$fitStats$BIC,
		  byHand$runStats$fitStats$AIC == fitFixed$fitStats$AIC,
		  fitFixed$fitStats$n == nrow(person))


## ---- the search finds the participant it was given ---------------------------

## a participant generated from the model with no noise, at a fixed target order
## and with one parameter free, has an answer the search can land on exactly. The
## objective is deterministic here, so this is a check that the bounds, the
## objective and the reported value all describe the same parameter.
exactPars <- list(upperBound = 100, lowerBound = 0, visibleReferencePoints = c(5, 50),
				  firstEstimate = 0.2, accuracyPercent = 0, numberSensitivity = 1,
				  targetOrder = "fixed")
clean <- ordinalNumberLineSim(person$target, exactPars, loops = 1)
exactPerson <- data.frame(target = clean$target, estimate = clean$fEst)

exactFit <- nlFit(exactPerson, upperBound = 100, lowerBound = 0,
				  visibleReferencePoints = c(5, 50),
				  free = list(firstEstimate = c(0, 1)),
				  accuracyPercent = 0, numberSensitivity = 1, targetOrder = "fixed",
				  search = "quick",
				  control = list(multicore = FALSE, numLoops = 20, numIntervals = 20,
								 optParamListN = 4, optBoundLoops = 1,
								 loops = 1, finalLoops = 1))
stopifnot(abs(exactFit$parameters$value[1] - exactPars$firstEstimate) < 0.1,
		  exactFit$fitStats$r2 > 0.99,
		  exactFit$fitStats$pars.n == 1)

## with three parameters free and noise on the estimates the parameters trade off
## against one another, so the values the search returns are not the values the
## data were generated at. What has to hold is that the search did its job: the
## set it returns scores no worse under the objective than the set the data came
## from, and the fit it reports is the fit of that set.
recovered <- nlFit(person, upperBound = 100, lowerBound = 0,
				   visibleReferencePoints = c(5, 50), search = "quick",
				   control = list(multicore = FALSE, numLoops = 30, numIntervals = 12,
								  optParamListN = 5, optBoundLoops = 2,
								  loops = 40, finalLoops = 100))
estimated <- recovered$parameters$value
names(estimated) <- recovered$parameters$parameter

scoreAt <- function(firstEstimate, accuracyPercent, numberSensitivity) {
	getOrdinalNumberlineFit(person, upperBound = 100, lowerBound = 0,
							visibleReferencePoints = c(5, 50),
							firstEstimate = firstEstimate, accuracyPercent = accuracyPercent,
							numberSensitivity = numberSensitivity, loops = 200, pars.n = 3)
}
atTruth <- scoreAt(truth$firstEstimate, truth$accuracyPercent, truth$numberSensitivity)
atFit <- scoreAt(estimated[["firstEstimate"]], estimated[["accuracyPercent"]],
				 estimated[["numberSensitivity"]])

stopifnot(all(estimated[c("firstEstimate", "accuracyPercent", "numberSensitivity")] >= 0),
		  all(estimated[c("firstEstimate", "accuracyPercent", "numberSensitivity")] <= 1),
		  recovered$fitStats$r2 > 0.9,
		  atFit <= atTruth + 5,
		  nrow(as.data.frame(recovered)) == 1)

}

cat("test-nlFit.R passed\n")

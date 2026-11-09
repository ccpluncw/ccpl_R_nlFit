#where each trial's estimate belongs, rowStart is each value's block offset
nlOutputRow <- function(presented, uniqueValues, rowStart) {
	rowStart[match(presented, uniqueValues)] + nlPresentationIndex(presented)
}


#reject targets that cannot be ordered, an empty set gives an empty result
nlCheckTargets <- function(targets) {
	if(!is.numeric(targets)) {
		stop("nlFit: targets must be numeric, not ", class(targets)[1], ".")
	}
	if(any(!is.finite(targets))) {
		stop("nlFit: targets must all be finite; ", sum(!is.finite(targets)), " are NA, NaN or infinite.")
	}
	invisible(NULL)
}


#the fit needs rows and numeric target and estimate columns
nlCheckFitData <- function(data, dataTargetCol, dataEstimateCol) {
	if(is.null(nrow(data)) || nrow(data) < 1) {
		stop("nlFit: data has no rows, so there is nothing to fit.")
	}
	for(column in c(dataTargetCol, dataEstimateCol)) {
		if(is.null(data[[column]])) stop("nlFit: data has no column named '", column, "'.")
		if(!is.numeric(data[[column]])) {
			stop("nlFit: the data column '", column, "' must be numeric, not ", class(data[[column]])[1], ".")
		}
	}
	invisible(NULL)
}


#misnumbered presentations lose rows in the join, and a partial fit is an error
nlStopOnUnmatchedTrials <- function(matched, supplied, dataPresentationCol) {
	if(matched >= supplied) return(invisible(NULL))
	stop("nlFit: ", supplied - matched, " of ", supplied,
		 " data rows had no simulated trial to match. The '", dataPresentationCol,
		 "' column must number the occurrences of each target value 1, 2, ... k in the order the rows are presented, with no gaps and no repeats.")
}


#' This function runs the ordinalNumberLine simulation and averages the runs.
#'
#' Function that runs the ordinalNumberLine simulation loops times and returns the mean
#' predicted estimate of each target. The estimates are averaged by target value and by
#' the occurrence number of that value within a run, so a target value presented more than
#' once gets one row per presentation rather than a single collapsed row. The first
#' presentation of a value sits earlier in the run on average than the later ones, so it
#' has a different expected estimate even under a random target order. The presentations
#' after the first often carry the same estimate as each other: once a value has been
#' estimated the repeat falls in its own earlier range of equivalent numbers and collapses
#' onto that estimate, so k presentations usually give two distinct predictions, not k.
#'
#' @param targets A vector of numbers that are the to-be-estimated values. A value may appear more than once.
#' @param parList A list with the parameter names and values for the nlFit analysis. The list element name must be the parameter name and the value is the contents. upperBound and visibleReferencePoints have no default and must be present. firstEstimate, memoryLength and conceptualReferencePoints may be absent or NULL, which is their default. lowerBound, numberSensitivity, accuracyPercent, pConceptual and targetOrder are filled with their defaults by fillNumberlineParList() when they are absent.
#' @param loops A number specifying the number of loops that will be run in the  simulation when it calculates the estimates. Higher numbers produce more precise estimates, but also increase the time needed to converge on a solution. Loops are collapsed to 1 when the run is deterministic, that is, when the target order is fixed or drawn once and there is no conceptual-point inclusion draw to average over.  Default \code{1000}.
#' @param verbose A boolean that specifies whether to print intermediate steps. This is used for debugging.  Default \code{FALSE}.
#''
#' @return A dataframe with one row per (target value, presentation) containing the target value (target), the mean predicted estimate over the loops (fEst), and the occurrence number of that value within a run (presentation).
#' @keywords ordinalNumberline ordinal number-line
#' @export
#' @examples
#' ordinalNumberLineSim(c(2, 3, 4, 5, 6),
#'                      parList = list(upperBound = 10, lowerBound = 0,
#'                                     firstEstimate = 0.5, memoryLength = 5,
#'                                     numberSensitivity = 1, accuracyPercent = 0.5,
#'                                     pConceptual = 0,
#'                                     visibleReferencePoints = c(0, 10),
#'                                     conceptualReferencePoints = NULL,
#'                                     targetOrder = "fixed"),
#'                      loops = 10)

ordinalNumberLineSim <- function(targets, parList, loops = 1000, verbose = FALSE) {

	nlCheckTargets(targets)

	#fill the defaults here rather than fail inside the trial loop
	parList <- fillNumberlineParList(parList)

	#no defaults: the bounds end the line, the reference points set its scale
	if(is.null(parList[["upperBound"]])) {
		stop("ordinalNumberLineSim: parList must contain upperBound. Pass the upper end of the number line, or the upper screen edge for a universal number line.")
	}
	if(is.null(parList[["visibleReferencePoints"]])) {
		stop("ordinalNumberLineSim: parList must contain visibleReferencePoints. Pass the bounds for a bounded number line, or the labelled values for a universal number line.")
	}

	#collapse the loops only when no draw is left to average over
	conceptualFixed <- is.null(parList[["conceptualReferencePoints"]]) ||
		(!is.null(parList[["pConceptual"]]) &&
			(nlEffectiveConceptual(parList[["numberSensitivity"]],
								   parList[["pConceptual"]]) %in% c(0, 1)))
	visibleP <- nlVisibleProbability(parList[["numberSensitivity"]])
	deterministicRun <- conceptualFixed && (visibleP == 0 || visibleP == 1)

	if(parList[["targetOrder"]] == "single")	{
		targetSeq <- sample(targets)
		if(deterministicRun) loops <- 1
	} else {
		targetSeq <- targets
	}
	if(parList[["targetOrder"]] == "fixed" && deterministicRun) {
		loops <- 1
	}

	#every run has the same multiset, so the rows are fixed before the loops
	uniqueValues <- sort(unique(targetSeq))
	multiplicity <- tabulate(match(targetSeq, uniqueValues), nbins = length(uniqueValues))
	rowStart <- cumsum(multiplicity) - multiplicity

	estimates <- matrix(NA_real_, nrow = length(targetSeq), ncol = loops)
	for(lps in seq_len(loops)) {
		run <- nlSimulateRun(targets = targetSeq,
							 upperBound = parList[["upperBound"]],
							 lowerBound = parList[["lowerBound"]],
							 firstEstimate = parList[["firstEstimate"]],
							 memoryLength = parList[["memoryLength"]],
							 accuracyPercent = parList[["accuracyPercent"]],
							 numberSensitivity = parList[["numberSensitivity"]],
							 pConceptual = parList[["pConceptual"]],
							 visibleReferencePoints = parList[["visibleReferencePoints"]],
							 conceptualReferencePoints = parList[["conceptualReferencePoints"]],
							 targetOrder = parList[["targetOrder"]],
							 verbose = verbose)
		estimates[nlOutputRow(run$targets, uniqueValues, rowStart), lps] <- run$estimate
	}

	#keep a target whose estimates are all NA rather than drop it
	data.frame(target = rep(uniqueValues, multiplicity),
			   fEst = rowMeans(estimates, na.rm = TRUE),
			   presentation = sequence(multiplicity))
}

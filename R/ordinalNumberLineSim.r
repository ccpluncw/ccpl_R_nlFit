# where each presented trial's estimate belongs in the (value, presentation)
# output. rowStart is the offset of each distinct value's block of rows.
nlOutputRow <- function(presented, uniqueValues, rowStart) {
	rowStart[match(presented, uniqueValues)] + nlPresentationIndex(presented)
}


#' This function runs the ordinalNumberLine simulation and averages the runs.
#'
#' Function that runs the ordinalNumberLine simulation loops times and returns the mean
#' predicted estimate of each target. The estimates are averaged by target value and by
#' the occurrence number of that value within a run, so a target value presented more than
#' once gets one row per presentation rather than a single collapsed row. The k-th
#' presentation of a value sits earlier in the run on average than the (k+1)-th, so the two
#' have different expected estimates even under a random target order.
#'
#' @param targets A vector of numbers that are the to-be-estimated values. A value may appear more than once.
#' @param parList A list with the parameter names and values for the nlFit analysis. The list element name must be the parameter name and the value is the contents. The required elements are: firstEstimate; upperBound; lowerBound; memoryLength; numberSensitivity; accuracyPercent; pIncludeConceptualPoints; visibleReferencePoints; conceptualReferencePoints; targetOrder.
#' @param loops A number specifying the number of loops that will be run in the  simulation when it calculates the estimates. Higher numbers produce more precise estimates, but also increase the time needed to converge on a solution. Loops are collapsed to 1 when the run is deterministic, that is, when the target order is fixed or drawn once and there is no conceptual-point inclusion draw to average over.  Default = 1000.
#' @param verbose A boolean that specifies whether to print intermediate steps. This is used for debugging.  Default is FALSE.
#''
#' @return A dataframe with one row per (target value, presentation) containing the target value (target), the mean predicted estimate over the loops (fEst), and the occurrence number of that value within a run (presentation).
#' @keywords ordinalNumberline ordinal number-line
#' @export
#' @examples
#' ordinalNumberLineSim(c(2, 3, 4, 5, 6),
#'                      parList = list(upperBound = 10, lowerBound = 0,
#'                                     firstEstimate = 0.5, memoryLength = 5,
#'                                     numberSensitivity = 1, accuracyPercent = 0.5,
#'                                     pIncludeConceptualPoints = 0,
#'                                     visibleReferencePoints = c(0, 10),
#'                                     conceptualReferencePoints = NULL,
#'                                     targetOrder = "fixed"),
#'                      loops = 10)

ordinalNumberLineSim <- function(targets, parList, loops = 1000, verbose = FALSE) {

	#the reference points define the regions of the line and set the scale of the
	#equivalence ranges, so there is nothing to simulate without them.
	if(is.null(parList[["visibleReferencePoints"]])) {
		stop("ordinalNumberLineSim: parList must contain visibleReferencePoints. Pass the bounds for a bounded number line, or the labelled values for a universal number line.")
	}

	#a fixed or once-drawn target order still varies across loops when conceptual points
	#are drawn in, so only collapse the loops when nothing is left to average over.
	deterministicRun <- is.null(parList[["conceptualReferencePoints"]]) ||
		(!is.null(parList[["pIncludeConceptualPoints"]]) &&
			(parList[["pIncludeConceptualPoints"]] == 0 || parList[["pIncludeConceptualPoints"]] == 1))

	if(parList[["targetOrder"]] == "single")	{
		targetSeq <- sample(targets)
		if(deterministicRun) loops <- 1
	} else {
		targetSeq <- targets
	}
	if(parList[["targetOrder"]] == "fixed" && deterministicRun) {
		loops <- 1
	}

	#every run presents the same multiset of targets, so the output rows are fixed
	#before the loops start: one row per distinct value per presentation of it.
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
							 pIncludeConceptualPoints = parList[["pIncludeConceptualPoints"]],
							 visibleReferencePoints = parList[["visibleReferencePoints"]],
							 conceptualReferencePoints = parList[["conceptualReferencePoints"]],
							 targetOrder = parList[["targetOrder"]],
							 verbose = verbose)
		estimates[nlOutputRow(run$targets, uniqueValues, rowStart), lps] <- run$estimate
	}

	#keep a target whose estimates are all NA rather than dropping it from the output
	data.frame(target = rep(uniqueValues, multiplicity),
			   fEst = rowMeans(estimates, na.rm = TRUE),
			   presentation = sequence(multiplicity))
}

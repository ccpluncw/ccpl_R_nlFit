#whether any visible point lies off the bounds
nlHasLabel <- function(visibleReferencePoints, lowerBound, upperBound) {
	any(!(visibleReferencePoints %in% c(lowerBound, upperBound)))
}


#probability each visible point is used: below 0 ns sets it
nlVisibleProbability <- function(numberSensitivity) {
	1 + min(numberSensitivity, 0)
}


#the conceptual probability: 0 at or below the gate
nlEffectiveConceptual <- function(numberSensitivity, pConceptual) {
	if(numberSensitivity <= 0) 0 else pConceptual
}


#half width is (1 - ns) times the distance to the nearest available landmark
nlBands <- function(targets, numberSensitivity, references, regionLength) {
	#with no landmark on the run the whole response region sets the scale
	if(length(references) == 0) {
		halfWidth <- rep((1 - numberSensitivity) * regionLength, length(targets))
	} else {
		halfWidth <- (1 - numberSensitivity) * do.call(pmin, lapply(references, function(r) abs(targets - r)))
	}
	list(low = targets - halfWidth, high = targets + halfWidth)
}


#one run: the landmarks are drawn afresh on every trial
nlSimulateRun <- function(targets, upperBound, lowerBound, firstEstimate, memoryLength,
						  accuracyPercent, numberSensitivity, pConceptual,
						  visibleReferencePoints, conceptualReferencePoints, targetOrder,
						  verbose = FALSE) {

	if(targetOrder == "random") targets <- sample(targets)

	numTargets <- length(targets)
	if(is.null(memoryLength)) memoryLength <- numTargets

	pVis <- nlVisibleProbability(numberSensitivity)
	pCon <- if(is.null(conceptualReferencePoints)) 0
		else nlEffectiveConceptual(numberSensitivity, pConceptual)
	sens <- max(numberSensitivity, 0)
	#accurate responding needs some sensitivity
	ap <- if(numberSensitivity <= 0) 0 else accuracyPercent
	regionLength <- upperBound - lowerBound

	mem <- nlReferenceStore(lowerBound, upperBound, visibleReferencePoints,
							conceptualReferencePoints, keyOffset = numTargets)
	#role of each reference row, fixed before any bound is relabelled
	onBound <- mem$value %in% c(lowerBound, upperBound)
	isVisible <- mem$value %in% visibleReferencePoints
	visibleKey <- mem$key[isVisible & !onBound]
	visibleValue <- mem$value[isVisible & !onBound]
	conceptualKey <- mem$key[!isVisible & !onBound]

	estimates <- numeric(numTargets)
	for(trialNumber in seq_len(numTargets)) {
		target <- targets[trialNumber]
		#a draw at probability 0 or 1 uses no random number
		usedVisible <- if(pVis == 1) visibleReferencePoints
			else if(pVis == 0) NULL
			else visibleReferencePoints[stats::runif(length(visibleReferencePoints)) < pVis]
		conceptualOut <- if(pCon == 1) logical(length(conceptualKey))
			else if(pCon == 0) rep(TRUE, length(conceptualKey))
			else stats::runif(length(conceptualKey)) >= pCon
		skip <- c(visibleKey[!(visibleValue %in% usedVisible)], conceptualKey[conceptualOut])

		band <- nlBands(target, sens, usedVisible, regionLength)

		remembered <- nlRememberedSet(mem, trialNumber, memoryLength)
		keep <- !(remembered$trial == 0 & remembered$key %in% skip)
		remembered <- lapply(remembered, function(x) x[keep])
		bracket <- nlBracket(remembered, target)
		if(!is.null(bracket$relabel)) mem <- nlRelabelReference(mem, bracket$relabel, target)
		remembered <- bracket$remembered

		equivalence <- nlEquivalence(remembered, bracket$low, bracket$high, target,
									 target, band$low, band$high)
		placement <- nlPlacement(remembered, equivalence$low, equivalence$high, firstEstimate)
		estimates[trialNumber] <- nlResponse(placement, equivalence$accuracyTarget,
											 ap, lowerBound, upperBound)
		mem <- nlRemember(mem, target, target, estimates[trialNumber],
						  band$low, band$high, trialNumber, trialNumber)

		if(verbose) nlTraceTrial(trialNumber, target, remembered, bracket, equivalence,
								 placement, ap, estimates[trialNumber])
	}

	list(targets = targets, estimate = estimates)
}


#' This function simulates the numberLine process and return the predited estimates.
#'
#' Function that simulates the numberLine process and return the predited estimates.
#'
#' The reference points are drawn on every trial. Only the visible points used on a trial set its range of equivalence, scaled to the nearest one, or to the length of the response region when none is used. Conceptual points are anchors only. A point not used on a trial is not a reference point on that trial, although a bound remains a response limit.
#' The participant places each target ordinally in relation to the previous estimates and the
#' reference points. A target that is not bracketed by a remembered estimate is placed in the
#' region between its two reference points using firstEstimate; every other target is placed
#' midway between its two anchors. This is moderated by the memoryLength, which specifies how
#' many of the previous estimates are remembered (larger is better), and by numberSensitivity,
#' which specifies how finely numbers near a reference point are discriminated. Finally,
#' accuracyPercent specifies the influence of accuracy on the estimates.  This is a proportion,
#' between 0 and 1, whereby estimate is calculated as follows:
#' estimate = (estimate based on algroithm * 1-accuracyPercent) + presented target * accuracyPercent
#'
#' This is one run of the task. Use ordinalNumberLineSim() to average the estimates over many runs.
#'
#' @param targets A vector of numbers that are the to-be-estimated values. A value may appear more than once; each occurrence is a separate trial and is numbered in the presentation column of the output.
#' @param upperBound A number that identifes the upper point beyond which the partcipant cannot respond. In the bounded task this is the upper end of the number line and is also passed as a visible reference point. In the universal task it is the upper screen edge and is normally not a visible reference point. This must be specified in "number-line units."
#' @param lowerBound A number that identifes the lower point beyond which the partcipant cannot respond. In the bounded task this is the lower end of the number line and is also passed as a visible reference point. In the universal task it is the lower screen edge and is normally not a visible reference point.  DEFAULT = 0.
#' @param firstEstimate A proportion between 0 and 1 that identifies the spatial bias applied whenever a target is not bracketed by a remembered estimate, that is, whenever both of its anchors are reference points. 0 places the estimate at the bottom of the region between the two anchors, 1 at the top. In the bounded task this is the first trial only; in the universal task it recurs for the first target in each region between labelled points, and again in any region whose estimates have dropped out of the memory window. A conceptual reference point counts as a reference point, so whether firstEstimate applies on a given trial depends on which conceptual points were drawn for that run.  If NULL, the midpoint of the region is used.  DEFAULT = NULL
#' @param memoryLength An integer that specifies the number of previous estimates that are remembered. On trial t the remembered set is the estimates from trials t-memoryLength through t-1, plus every reference point. The larger the number, the more accurate the estimates are going to be (given the constraints of the process). DEFAULT = length(targets).
#' @param accuracyPercent A proportion between 0 and 1 that specifies the weight given to accurate responding: 0 = no weight, 1 = perfect responding.  DEFAULT = 0
#' @param numberSensitivity A number between -1 and 1 that specifies the range of equivelance. A number's range is scaled by its distance to the nearest reference point, so numbers near a landmark are discriminated finely and numbers far from one coarsely. When numberSensitivity=1, every number is discriminated from every other. When numberSensitivity=0, a number is not discriminated from anything within its distance to the nearest reference point. Two numbers are treated as the same when either one falls in the other's range; a target is compared to a reference point in one direction only, so labels stay perfectly discriminable. DEFAULT = 1 (perfect precision) At or below 0, sensitivity, accuracyPercent and pConceptual act as 0. Below 0, each visible reference point is used on a trial with probability \code{1 + numberSensitivity}, so at -1 none is.
#' @param pConceptual A proportion between 0 and 1 that specifies the probability that a conceptualReferencePoint is used on a trial. Each conceptual point is drawn independently on every trial. A conceptual point used on a trial anchors that trial but does not set its range of equivalence. It acts as 0 at or below a numberSensitivity of 0. DEFAULT = 0
#' @param visibleReferencePoints A vector of numbers that specify the visible (displayed) points that identify the value of positions on the number line. In the bounded task these are the upper and lower bound; in the universal task they are the labelled values and the bounds are the screen edges. This is required; there is no default.
#' @param conceptualReferencePoints A vector of numbers that specify any conceptual points that the participant may use to identify the value of positions on the number line. These may be the middle of the bounded number-line, or a learned position on the number_line. Currently, conceptualReferencePoints are only modeled if they are used thoughout the task (not introduced part of the way through) Default is NULL.
#' @param targetOrder A string specifying whether to draw a fresh random order of the targets for this run ("random") or to take them in the order given ("fixed"). "single" also takes them in the order given: it means "one order held across the loops", which is a property of the loops and so belongs to ordinalNumberLineSim(), which draws that order and passes it here.  Default is "random".
#' @param verbose A boolean that specifies whether to print intermediate steps. This is used for debugging.  Default is FALSE.
#''
#' @return A dataframe with one row per trial, in the order the trials were presented, containing the target value (target), the predicted estimate (fEst), and the occurrence number of that target value within the run (presentation).
#' @keywords ordinalNumberline number line
#' @export
#' @examples
#' ordinalNumberLine(c(2, 3, 4, 5, 6), upperBound = 10, lowerBound = 0,
#'                   visibleReferencePoints = c(0, 10), firstEstimate = 0.5,
#'                   accuracyPercent = 0.5, targetOrder = "fixed")

ordinalNumberLine <- function(targets, upperBound, lowerBound = 0, firstEstimate = NULL, memoryLength = NULL, accuracyPercent = 0, numberSensitivity = 1, pConceptual = 0, visibleReferencePoints = NULL, conceptualReferencePoints = NULL, targetOrder = "random", verbose = FALSE) {

  #the reference points define the regions of the line and set the scale of the
  #equivalence ranges, so there is nothing to simulate without them.
  if(is.null(visibleReferencePoints)) {
    stop("ordinalNumberLine: visibleReferencePoints is required. Pass the bounds for a bounded number line, or the labelled values for a universal number line.")
  }

  run <- nlSimulateRun(targets = targets, upperBound = upperBound, lowerBound = lowerBound,
					   firstEstimate = firstEstimate, memoryLength = memoryLength,
					   accuracyPercent = accuracyPercent, numberSensitivity = numberSensitivity,
					   pConceptual = pConceptual,
					   visibleReferencePoints = visibleReferencePoints,
					   conceptualReferencePoints = conceptualReferencePoints,
					   targetOrder = targetOrder, verbose = verbose)

  data.frame(target = run$targets, fEst = run$estimate,
			 presentation = nlPresentationIndex(run$targets))
}

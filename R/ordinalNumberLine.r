# which conceptual points the participant has on this run. Each point is drawn
# independently, once per run, and is then present on every trial of that run.
nlDrawConceptual <- function(conceptualReferencePoints, pIncludeConceptualPoints) {
	if(is.null(conceptualReferencePoints)) return(NULL)
	included <- sample(c(TRUE, FALSE), length(conceptualReferencePoints),
					   prob = c(pIncludeConceptualPoints, (1 - pIncludeConceptualPoints)), replace = TRUE)
	if(any(included)) conceptualReferencePoints[included] else NULL
}


# the range of equivalent numbers around each target. The half width is the
# distance to the nearest reference point scaled by the inverted sensitivity, so
# numbers near a landmark are discriminated finely and numbers far from one
# coarsely. The bounds do not set the scale here: only the points the participant
# can see or holds conceptually do.
nlBands <- function(targets, numberSensitivity, references) {
	halfWidth <- (1 - numberSensitivity) * do.call(pmin, lapply(references, function(r) abs(targets - r)))
	list(low = targets - halfWidth, high = targets + halfWidth)
}


# one run of the task: draw the conceptual points, build the reference frame and
# the equivalence ranges, then walk the trials. The targets are taken in the
# order given unless targetOrder asks for a fresh permutation. The presented
# order is returned alongside the estimates because the caller needs both.
nlSimulateRun <- function(targets, upperBound, lowerBound, firstEstimate, memoryLength,
						  accuracyPercent, numberSensitivity, pIncludeConceptualPoints,
						  visibleReferencePoints, conceptualReferencePoints, targetOrder, verbose = FALSE) {

	if(targetOrder == "random") targets <- sample(targets)

	numTargets <- length(targets)
	if(is.null(memoryLength)) memoryLength <- numTargets

	conceptualIncluded <- nlDrawConceptual(conceptualReferencePoints, pIncludeConceptualPoints)
	reference <- nlReferenceStore(lowerBound, upperBound, visibleReferencePoints,
								  conceptualIncluded, keyOffset = numTargets)
	bands <- nlBands(targets, numberSensitivity, c(visibleReferencePoints, conceptualIncluded))

	run <- nlRunTrials(targets = targets, labels = targets, low = bands$low, high = bands$high,
					   keys = seq_len(numTargets), reference = reference, memoryLength = memoryLength,
					   accuracyPercent = accuracyPercent, firstEstimate = firstEstimate,
					   lowerBound = lowerBound, upperBound = upperBound, verbose = verbose)

	list(targets = targets, estimate = run$estimate)
}


#' This function simulates the numberLine process and return the predited estimates.
#'
#' Function that simulates the numberLine process and return the predited estimates.
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
#' @param numberSensitivity A proportion between 0 and 1 that specifies the range of equivelance. A number's range is scaled by its distance to the nearest reference point, so numbers near a landmark are discriminated finely and numbers far from one coarsely. When numberSensitivity=1, every number is discriminated from every other. When numberSensitivity=0, a number is not discriminated from anything within its distance to the nearest reference point. Two numbers are treated as the same when either one falls in the other's range; a target is compared to a reference point in one direction only, so labels stay perfectly discriminable. DEFAULT = 1 (perfect precision)
#' @param pIncludeConceptualPoints A proportion between 0 and 1 that specifies the probability that a conceptualReferencePoint is available on a run. Each conceptual point is drawn independently, once per run, and is then present on every trial of that run. Because the drawn points enter the reference set, this probability moves the estimates through two channels: which anchors are available and how wide each target's range of equivalence is.  DEFAULT = 0
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

ordinalNumberLine <- function(targets, upperBound, lowerBound = 0, firstEstimate = NULL, memoryLength = NULL, accuracyPercent = 0, numberSensitivity = 1, pIncludeConceptualPoints = 0, visibleReferencePoints = NULL, conceptualReferencePoints = NULL, targetOrder = "random", verbose = FALSE) {

  #the reference points define the regions of the line and set the scale of the
  #equivalence ranges, so there is nothing to simulate without them.
  if(is.null(visibleReferencePoints)) {
    stop("ordinalNumberLine: visibleReferencePoints is required. Pass the bounds for a bounded number line, or the labelled values for a universal number line.")
  }

  run <- nlSimulateRun(targets = targets, upperBound = upperBound, lowerBound = lowerBound,
					   firstEstimate = firstEstimate, memoryLength = memoryLength,
					   accuracyPercent = accuracyPercent, numberSensitivity = numberSensitivity,
					   pIncludeConceptualPoints = pIncludeConceptualPoints,
					   visibleReferencePoints = visibleReferencePoints,
					   conceptualReferencePoints = conceptualReferencePoints,
					   targetOrder = targetOrder, verbose = verbose)

  data.frame(target = run$targets, fEst = run$estimate,
			 presentation = nlPresentationIndex(run$targets))
}

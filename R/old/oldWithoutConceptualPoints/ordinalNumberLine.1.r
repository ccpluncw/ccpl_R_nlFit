#' This function simulates the numberLine process and return the predited estimates.
#'
#' Function that simulates the numberLine process and return the predited estimates.
#' This function assumes that participants randomly place thier first response. Then their section response is randomly
#' presented between the first response and either the upper or lower boundary (depending on whether the target is greater
#' than or less than (respectively) the first target). Then, the partcipant will place the following estimates  ordinally
#' in relation to the previous estimates (and the upper and lower bounds). This is moderated by the memoryLength, which
#' specifies how well they remember the previous estimates (larger is better) and rangeLength, which specifies
#' how close they set the bounds around the remembered items (smaller is better). Finally, accuracyPercent specifies
#' the influence of accuracy on the the estimates.  This is a proportion, between 0 and 1, whereby estimate is
#' calculated as follows: estimate = (estimate based on algroithm * 1-accuracyPercent) + presented target * accuracyPercent
#'
#' @param targets A vector of numbers that are the to-be-estimated values.
#' @param upperBound A number that identifes the upper point beyond which the partcipant cannot respond.  This is the upperBound of the bounded number line or the upper screen edge when the unbounded number line is used. This must be specified in "number-line units."
#' @param lowerBound A number that identifes the lower point beyond which the partcipant cannot respond.  This is the lowerBound of the bounded and unbounded number line or the lower screen edge when the universal number line is used.  DEFAULT = 0.
#' @param firstEstimate A number that is the first estimated value.  This value will influence the remaining results.  If NULL, then a random draw from between the upperBound and lowerBound will serve as the first estimate.  DEFAULT = NULL
#' @param memoryLength A integer that specifies the number of previous estimates that are remembered. The larger the number, the more accurate the estimates are going to be (given the constraints of the process). DEFAULT = legnth(targets).
#' @param rangeLength An integer that specifies the size of the window on the vector of remembered estimates that is used to identify the bounds of the next estimate. smaller number will produce more accurate estmates.  DEFAULT = 2
#' @param accuracyPercent A proportion between 0 and 1 that specifies the weight given to accurate responding: 0 = no weight, 1 = perfect responding.  DEFAULT = 0
#' @param numberSensitivity A proportion between 0 and 1 that specifies the precision that previous targets are remembered. Specifically, it is the range of equivelance, where all targets between a low of (numberSensitivity * target) and a high of (2-numberSensitivity) are treated as equivelent and will be put in the same place on the number-line. DEFAULT = 1 (perfect precision)
#' @param referencePoints A vector of numbers that specify the displayed points that identify the value of positions on the number line. These are the upper and lower bound of the bounded number line, 0 and 1 for the unbounded number line, and anything the researcher uses for the universal number line or learning tasks.  Default is NULL.
#' @param verbose A boolean that specifies whether to print intermediate steps. This is used for debugging.  Default is FALSE.
#''
#' @return a dataframe containing the target value (target) and predicted estimate (fEst).
#' @keywords ordinalNumberline number line
#' @export
#' @importFrom dplyr %>%
#' @examples ordinalNumberLine (c(2,3,4, 5, 6), 100, 0, firstEstimate = 400, rangeLength = 5, accuracyPercent = 0.5)

ordinalNumberLine.1 <- function(targets, upperBound, lowerBound = 0, firstEstimate = NULL, memoryLength = NULL, rangeLength = 1, accuracyPercent = 0, numberSensitivity = 1, referencePoints = NULL, verbose = FALSE) {

  numTargets <- length(targets)
  if (is.null(memoryLength)) memoryLength <- numTargets

  df.data <- data.frame(trial = seq(1, numTargets, 1), target = targets, low = numberSensitivity * targets, high = (2 - numberSensitivity) * targets,  estimate = NA)

	#upper and lower bounds are just (potentially) unseen references
	df.reference <- data.frame(trial = 0, target = c(lowerBound, upperBound), low = c(lowerBound, upperBound), high = c(lowerBound, upperBound), estimate = c(lowerBound, upperBound))

	if(!is.null(referencePoints)) {
		df.reference <- unique(rbind(df.reference, data.frame(trial = 0, target = referencePoints, low = referencePoints, high = referencePoints, estimate = referencePoints)))
	}

	df.data <- rbind(df.data,df.reference)

	for(i in 1:numTargets) {
		df.data[df.data$trial == i, "estimate"] <- getEstimateNL(df.data, i, trialCol = "trial", targetCol = "target", estimateCol = "estimate", memoryLength = memoryLength, rangeLength = rangeLength, numberSensitivity = numberSensitivity, accuracyPercent = accuracyPercent, firstEstimate = firstEstimate, verbose = verbose)
	}

	df.out <- df.data[df.data$trial > 0,c("target", "estimate")]
	names(df.out) <- c("target", "fEst")

  return(df.out)
}

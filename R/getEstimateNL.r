# Design rule for this package: extend the model by adding arguments whose
# defaults reproduce the current behaviour exactly; never change the meaning of
# an existing parameter. The anchor offset, for instance, is fixed at one rank on
# each side and may become a parameter in the future.


#' This function simulates the numberLine process and returns the predicted estimates.
#'
#' Function that walks the trials in a prepared dataframe and fills in the predicted estimate
#' for each of them. Each trial is placed ordinally between the two remembered values that
#' bracket it, where the remembered values are the estimates inside the memory window plus
#' every reference point. ordinalNumberLine() prepares the dataframe and calls this; call it
#' directly only to drive the process from a dataframe you have built yourself.
#'
#' A target that reaches a bound has no remembered value on that side, so the bound is
#' relabelled to the target and keeps that value for the rest of the run. The relabel moves
#' the target and value columns of the bound's row only: its estimate and its range of
#' equivalent numbers stay where they were, so a relabelled bound never satisfies an
#' equivalence test. The two ends are not symmetric: the lower guard fires at or below the
#' lower bound, the upper guard only strictly above the upper bound. A target sitting exactly
#' on a bound therefore relabels that bound to its own value, which changes nothing.
#'
#' @param data A dataframe with one row per trial and one row per reference point, holding the columns named by trialCol, targetCol, valueCol, lowCol, highCol and estimateCol. Trials are numbered 1 through totalNumTargets; a trial number of 0 marks a reference point, whose estimate is its position on the line. It is created in ordinalNumberLine() and passed to this function.
#' @param totalNumTargets An integer specifying the number of trials (targets) in data. The function estimates trials 1 through totalNumTargets in order.
#' @param upperBound A number that identifes the upper point beyond which the partcipant cannot respond.  This is the upperBound of the bounded number line or the upper screen edge when the unbounded number line is used. This must be specified in "number-line units."
#' @param lowerBound A number that identifes the lower point beyond which the partcipant cannot respond.  This is the lowerBound of the bounded and unbounded number line or the lower screen edge when the universal number line is used.  DEFAULT = 0.
#' @param trialCol A string specifying the column of data that holds the trialNumbers.  DEFAULT = "trial".
#' @param targetCol A string specifying the column of data that holds the targetValues.  DEFAULT = "target".
#' @param estimateCol A string specifying the column of data that holds the estimated values.  DEFAULT = "estimate".
#' @param valueCol A string specifying the column of data that holds the target values.  DEFAULT = "targetValue".
#' @param lowCol A string specifying the column of data that holds the lower value of the range of equivelent numbers derived from numberSensitivity.  DEFAULT = "low".
#' @param highCol A string specifying the column of data that holds the higher value of the range of equivelent numbers derived from numberSensitivity.  DEFAULT = "high".
#' @param memoryLength An integer that specifies the number of previous estimates that are remembered. On trial t the remembered set is the estimates from trials t-memoryLength through t-1, plus every reference point. The larger the number, the more accurate the estimates are going to be (given the constraints of the process). DEFAULT = totalNumTargets.
#' @param numberSensitivity A proportion between 0 and 1 that specifies the range of equivelance. The ranges are computed in ordinalNumberLine() and read from the lowCol and highCol columns of data, so this argument does not affect the estimates here.  DEFAULT = 1 (perfect precision)
#' @param accuracyPercent A proportion between 0 and 1 that specifies the weight given to accurate responding: 0 = no weight, 1 = perfect responding.  DEFAULT = 0
#' @param firstEstimate A proportion between 0 and 1 that identifies the spatial bias applied whenever a target is not bracketed by a remembered estimate, that is, whenever both of its anchors are reference points. 0 places the estimate at the bottom of the region between the two anchors, 1 at the top. Because a conceptual reference point is a reference point, whether firstEstimate applies on a given trial depends on which conceptual points were included on that run.  If NULL, the midpoint of the region is used.  DEFAULT = NULL
#' @param verbose A boolean that specifies whether to print intermediate steps. This is used for debugging.  Default is FALSE.
#''
#' @return The dataframe that was passed in, with the estimate column filled in for the trial rows and with any relabelled bound carrying its new value.
#' @keywords ordinalNumberline number line
#' @export
#' @examples
#' df <- data.frame(trial = c(1, 2, 0, 0),
#'                  target = c(30, 70, 0, 100),
#'                  low = c(30, 70, 0, 100),
#'                  high = c(30, 70, 0, 100),
#'                  estimate = c(NA, NA, 0, 100),
#'                  targetValue = c(30, 70, 0, 100))
#' getEstimateNL(df, totalNumTargets = 2, upperBound = 100, lowerBound = 0, memoryLength = 2)


getEstimateNL <- function(data, totalNumTargets, upperBound, lowerBound = 0, trialCol = "trial", targetCol = "target", valueCol = "targetValue", lowCol = "low", highCol = "high", estimateCol = "estimate",  memoryLength = NULL, numberSensitivity = 1, accuracyPercent = 0, firstEstimate = NULL, verbose = FALSE)  {

	if(is.null(memoryLength)) memoryLength <- totalNumTargets

	#the row order of data breaks ties in value, so a row's position is carried
	#through the simulation as its key
	trialRow <- match(seq_len(totalNumTargets), data[[trialCol]])
	referenceRow <- which(data[[trialCol]] == 0)

	reference <- nlStoreFromRows(value = data[[targetCol]][referenceRow],
								 label = data[[valueCol]][referenceRow],
								 estimate = data[[estimateCol]][referenceRow],
								 low = data[[lowCol]][referenceRow],
								 high = data[[highCol]][referenceRow],
								 trial = rep(0, length(referenceRow)),
								 key = referenceRow)

	run <- nlRunTrials(targets = data[[targetCol]][trialRow],
					   labels = data[[valueCol]][trialRow],
					   low = data[[lowCol]][trialRow],
					   high = data[[highCol]][trialRow],
					   keys = trialRow,
					   reference = reference,
					   memoryLength = memoryLength,
					   accuracyPercent = accuracyPercent,
					   firstEstimate = firstEstimate,
					   lowerBound = lowerBound,
					   upperBound = upperBound,
					   verbose = verbose)

	data[[estimateCol]][trialRow] <- run$estimate

	#any relabelled bound goes back to the caller carrying its new value
	isReference <- run$store$trial == 0
	data[[targetCol]][run$store$key[isReference]] <- run$store$value[isReference]
	data[[valueCol]][run$store$key[isReference]] <- run$store$label[isReference]

	return(data)
}

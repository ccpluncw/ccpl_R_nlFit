#' This function simulates the numberLine process and return the predited estimate for a single trial.
#'
#' Function simulates the numberLine process and return the predited estimate for a single trial
#'
#' @param data A dataframe with the following columns: trialNumber, targetValue, estimate. It is created in ordinalNumberline() and passed to this function.
#' @param trialNumber A number that identifes the trial number of the estimate to be made.
#' @param trialCol A string specifying the column of data that holds the trialNumbers.  DEFAULT = "trial".
#' @param targetCol A string specifying the column of data that holds the targetValues.  DEFAULT = "target".
#' @param estimateCol A string specifying the column of data that holds the estimated values.  DEFAULT = "estimate".
#' @param valueCol A string specifying the column of data that holds the target values.  DEFAULT = "targetValue".
#' @param lowCol A string specifying the column of data that holds the lower value of the range of equivelent numbers derived from numberSensitivity.  DEFAULT = "low".
#' @param highCol A string specifying the column of data that holds the higher value of the range of equivelent numbers derived from numberSensitivity.  DEFAULT = "high".
#' @param memoryLength A integer that specifies the number of previous estimates that are remembered. The larger the number, the more accurate the estimates are going to be (given the constraints of the process). DEFAULT = legnth(targets).
#' @param rangeLength An integer that specifies the size of the window on the vector of remembered estimates that is used to identify the bounds of the next estimate. smaller number will produce more accurate estmates.  DEFAULT = 1
#' @param numberSensitivity A proportion between 0 and 1 that specifies the precision that previous targets are remembered. Specifically, it is the range of equivelance, where all targets between a low of (numberSensitivity * target) and a high of (2-numberSensitivity) are treated as equivelent and will be put in the same place on the number-line. DEFAULT = 1 (perfect precision)
#' @param accuracyPercent A proportion between 0 and 1 that specifies the weight given to accurate responding: 0 = no weight, 1 = perfect responding.  DEFAULT = 0
#' @param firstEstimate A number that is the first estimated value.  This value will influence the remaining results.  If NULL, then a random draw from between the upperBound and lowerBound will serve as the first estimate.  DEFAULT = NULL
#' @param verbose A boolean that specifies whether to print intermediate steps. This is used for debugging.  Default is FALSE.
#''
#' @return a dataframe containing the target value (target) and predicted estimate (fEst).
#' @keywords ordinalNumberline number line
#' @export
#' @importFrom dplyr %>%
#' @examples ordinalNumberLine (c(2,3,4, 5, 6), 100, 0, firstEstimate = 400, rangeLength = 5, accuracyPercent = 0.5)


getEstimateNL <- function(data, trialNumber, trialCol = "trial", targetCol = "target", valueCol = "targetValue", lowCol = "low", highCol = "high", estimateCol = "estimate",  memoryLength = NULL, rangeLength = 1, numberSensitivity = 1, accuracyPercent = 0, firstEstimate = NULL, verbose = FALSE)  {

		targetInfo <- data[data[[trialCol]] == trialNumber, ]

		if((trialNumber - memoryLength - 1) < 1) previousTargets <- data[data[[trialCol]] <= (trialNumber - 1), ]
			else previousTargets <- data[(data[[trialCol]] >= (trialNumber - memoryLength - 1) & data[[trialCol]] <= (trialNumber - 1)) | data[[trialCol]] ==0, ]

		numTargets <- nrow(previousTargets)
		previousTargets <- previousTargets[order(previousTargets[[targetCol]]),]
		adjacentIdx <- which.min(abs(previousTargets[[targetCol]] - targetInfo[[targetCol]]))

		minAdjIdx <- ifelse(targetInfo[[targetCol]] > previousTargets[adjacentIdx, targetCol], adjacentIdx - (rangeLength - 1), adjacentIdx - rangeLength)
		minAdjIdx <- ifelse(minAdjIdx < 1, 1, minAdjIdx)

		maxAdjIdx <- ifelse(targetInfo[[targetCol]] > previousTargets[adjacentIdx, targetCol], adjacentIdx + rangeLength, adjacentIdx + (rangeLength - 1))
		maxAdjIdx <- ifelse(maxAdjIdx > numTargets, numTargets, maxAdjIdx)

	#if the target is within the high and low of previousTarget[minAdjIdx],
	#then set ptMinInTargetRange to TRUE
		ptMinInTargetRange <-ifelse(targetInfo[[targetCol]] > previousTargets[minAdjIdx, lowCol] & targetInfo[[targetCol]] < previousTargets[minAdjIdx, highCol], TRUE, FALSE)
	# and do the same for the ptMaxInTargetRange
		ptMaxInTargetRange <-ifelse(targetInfo[[targetCol]] > previousTargets[maxAdjIdx, lowCol] & targetInfo[[targetCol]] < previousTargets[maxAdjIdx, highCol], TRUE, FALSE)

	#if the target is in only one previousTarget range, then set the minAdjIdx = maxAdjIdx so the estimate will be equal to that previousTarget
		maxAdjIdx <- ifelse(ptMinInTargetRange == TRUE & ptMaxInTargetRange == FALSE, minAdjIdx, maxAdjIdx)
		minAdjIdx <- ifelse(ptMinInTargetRange == FALSE & ptMaxInTargetRange == TRUE, maxAdjIdx, minAdjIdx)

		df.tmp <- data[data[[trialCol]] < trialNumber, ]
		#there can be multiple instances of a target in df.tmp
		lowTarget <- df.tmp[df.tmp[[targetCol]] == previousTargets[minAdjIdx, targetCol],]
		highTarget <- df.tmp[df.tmp[[targetCol]] == previousTargets[maxAdjIdx, targetCol],]
		#if one of them is a conceptual or visual reference point, use that, else get the average estimate
		eLow <- ifelse(min(lowTarget[[trialCol]]) == 0, mean(lowTarget[lowTarget[[trialCol]] == 0, estimateCol]), mean(lowTarget[[estimateCol]]))
		eHigh <- ifelse(min(highTarget[[trialCol]]) == 0, mean(highTarget[highTarget[[trialCol]] == 0, estimateCol]), mean(highTarget[[estimateCol]]))

		estimate <- ifelse( (trialNumber == 1) & !is.null(firstEstimate), firstEstimate, mean(c(eLow, eHigh)))

		#### apply influence of numberSensitivity to the target value as well
		accuracyTargetVec <- NULL
		if (ptMinInTargetRange == TRUE) {
		  accuracyTargetVec <- c(accuracyTargetVec, unique(lowTarget[[valueCol]]))
		}
		if (ptMaxInTargetRange == TRUE) {
		  accuracyTargetVec <- c(accuracyTargetVec, unique(highTarget[[valueCol]]))
		}
		if(sum(ptMinInTargetRange, ptMaxInTargetRange) == 0 ) {
		  accuracyTargetVec <- targetInfo[[valueCol]]
		}
		accuracyTarget <- mean (accuracyTargetVec)

		targetInfo[[estimateCol]] <- estimate * (1 - accuracyPercent) + (accuracyTarget * accuracyPercent)


if(length(estimate) > 1) {
	print(eLow)
	print(eHigh)
	print(estimate)
	print(targetInfo$target)
	print(trialNumber)
}
	if(verbose) {
		print("**** NEW ****")
		print("**** target ****")
		print(targetInfo$target)
		print("**** previousTargets ****")
		print(previousTargets)
		print("**** AdjIdx ****")
		print(adjacentIdx)
		print("**** minAdjIdx ****")
		print(minAdjIdx)
		print("**** maxAdjIdx ****")
		print(maxAdjIdx)
		print("**** Min ****")
		print(previousTargets[minAdjIdx, ])
		print("**** Max ****")
		print(previousTargets[maxAdjIdx, ])
		print("**** estimate ****")
		print(estimate)
		print("**** accuracyTargetVec ****")
		print(accuracyTargetVec)
		print("**** accuracyTarget ****")
		print(accuracyTarget)
		print("**** accuracyPercent ****")
		print(accuracyPercent)
		print("**** Final Estimate ****")
		print(targetInfo[[estimateCol]])
	}

	return(targetInfo)

}

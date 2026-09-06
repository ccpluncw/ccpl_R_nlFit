#' This function simulates the numberLine process and return the predited estimate for a single trial.
#'
#' Function simulates the numberLine process and return the predited estimate for a single trial
#'
#' @param data A dataframe with the following columns: trialNumber, targetValue, estimate. It is created in ordinalNumberline() and passed to this function.
#' @param totalNumTargets An integer specifying the number of trials (targets) in data. The function estimates trials 1 through totalNumTargets in order.
#' @param upperBound A number that identifes the upper point beyond which the partcipant cannot respond.  This is the upperBound of the bounded number line or the upper screen edge when the unbounded number line is used. This must be specified in "number-line units."
#' @param lowerBound A number that identifes the lower point beyond which the partcipant cannot respond.  This is the lowerBound of the bounded and unbounded number line or the lower screen edge when the universal number line is used.  DEFAULT = 0.
#' @param trialCol A string specifying the column of data that holds the trialNumbers.  DEFAULT = "trial".
#' @param targetCol A string specifying the column of data that holds the targetValues.  DEFAULT = "target".
#' @param estimateCol A string specifying the column of data that holds the estimated values.  DEFAULT = "estimate".
#' @param valueCol A string specifying the column of data that holds the target values.  DEFAULT = "targetValue".
#' @param lowCol A string specifying the column of data that holds the lower value of the range of equivelent numbers derived from numberSensitivity.  DEFAULT = "low".
#' @param highCol A string specifying the column of data that holds the higher value of the range of equivelent numbers derived from numberSensitivity.  DEFAULT = "high".
#' @param memoryLength An integer that specifies the number of previous estimates that are remembered. On trial t the remembered set is the estimates from trials t-memoryLength through t-1, plus every reference point. The larger the number, the more accurate the estimates are going to be (given the constraints of the process). DEFAULT = length(targets).
#' @param numberSensitivity A proportion between 0 and 1 that specifies the range of equivelance. When numberSensitivity=0, the target is not differentiated from the nearest referenece point. When numberSensitivity=1, the target is differentiated from all other targets. When 0< numberSensitivity < 1, the range of equivelence is proportional to the distance to the nearest reference point. Two numbers are treated as equivalent when either one falls in the other's range, though a target is compared to a reference point in one direction only.  DEFAULT = 1 (perfect precision)
#' @param accuracyPercent A proportion between 0 and 1 that specifies the weight given to accurate responding: 0 = no weight, 1 = perfect responding.  DEFAULT = 0
#' @param firstEstimate A proportion between 0 and 1 that identifies the spatial bias applied whenever a target is not bracketed by a remembered estimate, that is, whenever both of its anchors are reference points. 0 places the estimate at the bottom of the region between the two anchors, 1 at the top. Because a conceptual reference point is a reference point, whether firstEstimate applies on a given trial depends on which conceptual points were included on that run.  If NULL, the midpoint of the region is used.  DEFAULT = NULL
#' @param verbose A boolean that specifies whether to print intermediate steps. This is used for debugging.  Default is FALSE.
#''
#' @return a dataframe containing the target value (target) and predicted estimate (fEst).
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

	for(trialNumber in 1:totalNumTargets) {
		# get the current target
		targetInfo <- data[data[[trialCol]] == trialNumber, ]

		#get the "previousTargets" but limit the memoryLength plus the reference points
		if((trialNumber - memoryLength) < 1) previousTargets <- data[data[[trialCol]] <= (trialNumber - 1), ]
			else previousTargets <- data[(data[[trialCol]] >= (trialNumber - memoryLength) & data[[trialCol]] <= (trialNumber - 1)) | data[[trialCol]] ==0, ]

		#get the number of "previous targets"
		numTargets <- nrow(previousTargets)
		#sort previous targets by the target value
		previousTargets <- previousTargets[order(previousTargets[[targetCol]]),]
		#find the index of the previous target that has the closest value as the current target
		adjacentIdx <- which.min(abs(previousTargets[[targetCol]] - targetInfo[[targetCol]]))

		#if the current target > adjacent target, then assign it as the minimum. Otherwise assign the
		#index before it as the minimum. The anchors are the immediate rank neighbours on each side;
		#the offset is fixed at one rank and may become a parameter in the future.
		minAdjIdx <- ifelse(targetInfo[[targetCol]] > previousTargets[adjacentIdx, targetCol], adjacentIdx, adjacentIdx - 1)

		## Here, we are adjusting the lowerBound value to that of the new, lower valued target
		## This happens when the target lies at or beyond the lowerBound, so there is no
		## remembered value below it. This should only happen 1) when the experimenter creates
		## this situation or 2) the respondent beleives the lowerBound is closer to the
		## visibleReferencePoints than it actually is in the Universal NumberLine.
		## The relabel moves the bound row's target and targetValue only: its low, high and
		## estimate stay at the original bound, so it can never satisfy an equivalence test.
		if(minAdjIdx < 1) {
			#set minAdjIdx = 1
			minAdjIdx <- 1
			#assign the lower bound value to the current target in original data
			data[data[[targetCol]] == previousTargets[adjacentIdx, targetCol] & data[[trialCol]] == 0, targetCol] <- targetInfo[[targetCol]]
			data[data[[valueCol]] == previousTargets[adjacentIdx, targetCol] & data[[trialCol]] == 0, valueCol] <- targetInfo[[targetCol]]
			#assign the lower bound value to the current target in previous targets
			previousTargets[adjacentIdx, targetCol] <- targetInfo[[targetCol]]
			previousTargets[adjacentIdx, valueCol] <- targetInfo[[targetCol]]
		}

		maxAdjIdx <- ifelse(targetInfo[[targetCol]] > previousTargets[adjacentIdx, targetCol], adjacentIdx + 1, adjacentIdx)
		if(maxAdjIdx > numTargets) {
			maxAdjIdx <- numTargets
			#assign the upper bound value to the current target in original data
			data[data[[targetCol]] == previousTargets[adjacentIdx, targetCol] & data[[trialCol]] == 0, targetCol] <- targetInfo[[targetCol]]
			data[data[[valueCol]] == previousTargets[adjacentIdx, targetCol] & data[[trialCol]] == 0, valueCol] <- targetInfo[[targetCol]]
			#assign the upper bound value to the current target in previous targets
			previousTargets[adjacentIdx, targetCol] <- targetInfo[[targetCol]]
			previousTargets[adjacentIdx, valueCol] <- targetInfo[[targetCol]]
		}

	#the target and an anchor are treated as the same number when the target falls inside
	#the anchor's range of equivalent numbers, or, for a remembered estimate, when the
	#anchor's value falls inside the target's range. Each number therefore keeps its own
	#resolution and the comparison does not depend on which was seen first. A reference
	#point is always discriminable from the target, so the reverse test skips trial 0 rows.
		ptMinInTargetRange <- (targetInfo[[targetCol]] >= previousTargets[minAdjIdx, lowCol] & targetInfo[[targetCol]] <= previousTargets[minAdjIdx, highCol]) |
			(previousTargets[minAdjIdx, trialCol] > 0 & previousTargets[minAdjIdx, targetCol] >= targetInfo[[lowCol]] & previousTargets[minAdjIdx, targetCol] <= targetInfo[[highCol]])
	# and do the same for the ptMaxInTargetRange
		ptMaxInTargetRange <- (targetInfo[[targetCol]] >= previousTargets[maxAdjIdx, lowCol] & targetInfo[[targetCol]] <= previousTargets[maxAdjIdx, highCol]) |
			(previousTargets[maxAdjIdx, trialCol] > 0 & previousTargets[maxAdjIdx, targetCol] >= targetInfo[[lowCol]] & previousTargets[maxAdjIdx, targetCol] <= targetInfo[[highCol]])

	#if the target is in only one previousTarget range, then set the minAdjIdx = maxAdjIdx so the estimate will be equal to that previousTarget
		maxAdjIdx <- ifelse(ptMinInTargetRange == TRUE & ptMaxInTargetRange == FALSE, minAdjIdx, maxAdjIdx)
		minAdjIdx <- ifelse(ptMinInTargetRange == FALSE & ptMaxInTargetRange == TRUE, maxAdjIdx, minAdjIdx)

		lowTarget <- previousTargets[minAdjIdx, ]
		highTarget <- previousTargets[maxAdjIdx, ]

		eLow <- ifelse(min(lowTarget[[trialCol]]) == 0, mean(lowTarget[lowTarget[[trialCol]] == 0, estimateCol]), mean(lowTarget[[estimateCol]]))
		eHigh <- ifelse(min(highTarget[[trialCol]]) == 0, mean(highTarget[highTarget[[trialCol]] == 0, estimateCol]), mean(highTarget[[estimateCol]]))

	#when both anchors are reference points there is no remembered estimate bracketing the
	#target, so firstEstimate sets where in the region the estimate falls. Otherwise the
	#target is placed midway between its two anchors.
		unanchored <- (lowTarget[[trialCol]] == 0) & (highTarget[[trialCol]] == 0)
		estimate <- ifelse( unanchored & !is.null(firstEstimate), eLow+(firstEstimate*abs(eHigh - eLow)), mean(c(eLow, eHigh)))

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

		##### limit output so that it cannot go past the upper and lower bounds
		targetInfo[[estimateCol]] <- ifelse(targetInfo[[estimateCol]] > upperBound, upperBound, ifelse(targetInfo[[estimateCol]] < lowerBound, lowerBound, targetInfo[[estimateCol]]))

		### fill target into data
		data[data[[trialCol]] == trialNumber, ] <- targetInfo

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
	}
	return(data)
}

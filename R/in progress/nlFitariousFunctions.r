
### this is an attempt to speed up ordinalNumberLine
library(dplyr)


getPreviousTargets <- function (df.reference, targetVector, trialNumber, memoryLength, numberSensitivity) {
	
	previousTargets <- df.reference

	if(trialNumber > 1) {
		if((trialNumber - memoryLength - 1) < 1) pTargets <- targetVector[1:(trialNumber - 1)]
		else pTargets <- targetVector[(trialNumber - memoryLength - 1):(trialNumber - 1)]

		previousTargets <- rbind(previousTargets, data.frame(trial = seq(1, length(pTargets), 1), target = pTargets, low = (numberSensitivity * pTargets), high = ((2 - numberSensitivity) * pTargets), estimate = NA))
	} 
	return(previousTargets)
	
}

getRangeIdx <- function(data, trialNumber, trialCol, targetCol, estimateCol,  df.reference,  memoryLength, rangeLength, numberSensitivity, accuracyPercent, firstEstimate, verbose = FALSE)  {

		target <- data[data[[trialCol]] == trialNumber, "target"]

		previousTargets <- getPreviousTargets (df.reference = df.reference, targetVector = data[[targetCol]], trialNumber = trialNumber, memoryLength = memoryLength, numberSensitivity = numberSensitivity)
		
		numTargets <- nrow(previousTargets)
		previousTargets <- previousTargets[order(previousTargets$target),]
		adjacentIdx <- which.min(abs(previousTargets$target - target))
	
		#get lowerIdx
		#this should never happen if done correctly
		if(adjacentIdx == numTargets & target > previousTargets[adjacentIdx, "target"]) {
			minAdjIdx <- adjacentIdx
		} else {
			minAdjIdx <- ifelse(target > previousTargets[adjacentIdx, "target"], adjacentIdx - (rangeLength - 1), adjacentIdx - rangeLength)
			minAdjIdx <- ifelse(minAdjIdx < 1, 1, minAdjIdx)
		}
	
		#get upperIdx
		#this should never happen if done correctly
		if(adjacentIdx == numTargets & target < previousTargets[adjacentIdx, "target"]) {
			maxAdjIdx <- adjacentIdx
		} else {
			maxAdjIdx <- ifelse(target > previousTargets[adjacentIdx, "target"], adjacentIdx + rangeLength, adjacentIdx + (rangeLength - 1))
			maxAdjIdx <- ifelse(maxAdjIdx > numTargets, numTargets, maxAdjIdx)
	  }
		
	#if the target is within the high and low of previousTargets[minAdjIdx], 
	#then set maxAdjIdx = minAdjIdx so the estimate will be the same estimate as was given to  previousTargets[minAdjIdx]
		maxAdjIdx <-ifelse(target < previousTargets$high[minAdjIdx] && target > previousTargets$low[minAdjIdx], minAdjIdx, maxAdjIdx)
	# and do the same for the maxAdjIdx
		minAdjIdx <-ifelse(target < previousTargets$high[maxAdjIdx] && target > previousTargets$low[maxAdjIdx], maxAdjIdx, minAdjIdx)
		
		df.data.in <- rbind(data,df.reference)
		
		
#		df.tmp <- data[data[[trialCol]] < trialNumber, ]
#		eLow <- df.tmp[df.tmp[[targetCol]] == previousTargets[minAdjIdx, "target"], estimateCol]
#		eHigh <- df.tmp[df.tmp[[targetCol]] == previousTargets[maxAdjIdx, "target"], estimateCol]
#		estimate <- ifelse( (trialNumber == 1) & !is.null(firstEstimate), firstEstimate, mean(c(eLow, eHigh)))
#		estimate <- estimate * (1 - accuracyPercent) + (target * accuracyPercent)
		
		
		
		estimate <- getEstimate(df.data.in, trialNumber, target, previousTargets[minAdjIdx, "target"], previousTargets[maxAdjIdx, "target"], trialCol = trialCol, targetCol = targetCol, estimateCol = estimateCol, firstEstimate = firstEstimate, accuracyPercent = accuracyPercent, verbose = verbose)
	  
#		rangeTargetsDF <- data.frame(lowerTarget = previousTargets[minAdjIdx, "target"], upperTarget = previousTargets[maxAdjIdx, "target"], estimate = estimate)
	
	if(verbose) {
		print("**** NEW ****")
		print("**** target ****")
		print(target)
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
	}
#	return(rangeTargetsDF)
	return(estimate)
	
}	

getEstimate <- function (data,trialNumber, target, lowerBoundaryTarget, upperBoundaryTarget, trialCol = "trial", targetCol = "target", estimateCol = "estimate", firstEstimate, accuracyPercent = 1, verbose = FALSE) {
	
	df.tmp <- data[data[[trialCol]] < trialNumber, ]
	eLow <- df.tmp[df.tmp[[targetCol]] == lowerBoundaryTarget, estimateCol]
	eHigh <- df.tmp[df.tmp[[targetCol]] == upperBoundaryTarget, estimateCol]
	estimate <- ifelse( (trialNumber == 1) & !is.null(firstEstimate), firstEstimate, mean(c(eLow, eHigh)))

	if(verbose) {
		print("******** NEW ********")
		print(df.tmp)
		print("**** target ****")
		print(target)
		print("**** lbt ****")
		print(lowerBoundaryTarget)
		print("**** (hbt) ****")
		print(upperBoundaryTarget)
		print("**** eLow ****")
		print(eLow)
		print("**** eHigh ****")
		print(eHigh)
		print("**** estimate ****")
		print(estimate)
	}

	estimate <- estimate * (1 - accuracyPercent) + (target * accuracyPercent)
	
	return(estimate)
	
}

getEstimate.x <- function (trialNumber, data, trialCol = "trial", targetCol = "target", lowerBoundingTargetCol = "lowerBoundingTarget", upperBoundingTarget = "upperBoundingTarget", estimateCol = "estimate", firstEstimate, accuracyPercent = 1, verbose = FALSE) {
	
	target <- data[data[[trialCol]] == trialNumber, targetCol]
	lbt <- data[data[[trialCol]] == trialNumber, lowerBoundingTargetCol]
	hbt <- data[data[[trialCol]] == trialNumber, upperBoundingTarget]

	df.tmp <- data[data[[trialCol]] < trialNumber, ]
	eLow <- df.tmp[df.tmp[[targetCol]] == lbt, estimateCol]
	eHigh <- df.tmp[df.tmp[[targetCol]] == hbt, estimateCol]
	estimate <- ifelse( (trialNumber == 1) & !is.null(firstEstimate), firstEstimate, mean(c(eLow, eHigh)))

	if(verbose) {
		print("******** NEW ********")
		print("**** target ****")
		print(target)
		print("**** lbt ****")
		print(lbt)
		print("**** (hbt) ****")
		print(hbt)
		print("**** eLow ****")
		print(eLow)
		print("**** eHigh ****")
		print(eHigh)
		print("**** estimate ****")
		print(estimate)
	}

	estimate <- estimate * (1 - accuracyPercent) + (target * accuracyPercent)
	
	return(estimate)
	
}

getEstimateAll <- function(data, trialNumber, trialCol, targetCol, estimateCol,  df.reference,  memoryLength, rangeLength, numberSensitivity, accuracyPercent, firstEstimate, verbose = FALSE)  {

		target <- data[data[[trialCol]] == trialNumber, "target"]
		
		if((trialNumber - memoryLength - 1) < 1) previousTargets <- data[data[[trialCol]] <= (trialNumber - 1), ]
			else previousTargets <- data[(data[[trialCol]] >= (trialNumber - memoryLength - 1) & data[[trialCol]] <= (trialNumber - 1)) | data[[trialCol]] ==0, ]
			
		numTargets <- nrow(previousTargets)
		previousTargets <- previousTargets[order(previousTargets$target),]
		adjacentIdx <- which.min(abs(previousTargets$target - target))
	
		#get lowerIdx
		#this should never happen if done correctly
		# if(adjacentIdx == numTargets & target > previousTargets[adjacentIdx, "target"]) {
		# 	minAdjIdx <- adjacentIdx
		# } else {
			minAdjIdx <- ifelse(target > previousTargets[adjacentIdx, "target"], adjacentIdx - (rangeLength - 1), adjacentIdx - rangeLength)
			minAdjIdx <- ifelse(minAdjIdx < 1, 1, minAdjIdx)
		# }
	
		#get upperIdx
		#this should never happen if done correctly
		# if(adjacentIdx == numTargets & target < previousTargets[adjacentIdx, "target"]) {
		# 	maxAdjIdx <- adjacentIdx
		# } else {
			maxAdjIdx <- ifelse(target > previousTargets[adjacentIdx, "target"], adjacentIdx + rangeLength, adjacentIdx + (rangeLength - 1))
			maxAdjIdx <- ifelse(maxAdjIdx > numTargets, numTargets, maxAdjIdx)
	  # }
		
	#if the target is within the high and low of previousTargets[minAdjIdx], 
	#then set maxAdjIdx = minAdjIdx so the estimate will be the same estimate as was given to  previousTargets[minAdjIdx]
		maxAdjIdx <-ifelse(target < previousTargets$high[minAdjIdx] && target > previousTargets$low[minAdjIdx], minAdjIdx, maxAdjIdx)
	# and do the same for the maxAdjIdx
		minAdjIdx <-ifelse(target < previousTargets$high[maxAdjIdx] && target > previousTargets$low[maxAdjIdx], maxAdjIdx, minAdjIdx)

		
		df.tmp <- data[data[[trialCol]] < trialNumber, ]
		eLow <- df.tmp[df.tmp[[targetCol]] == previousTargets[minAdjIdx, "target"], estimateCol]
		eHigh <- df.tmp[df.tmp[[targetCol]] == previousTargets[maxAdjIdx, "target"], estimateCol]
		estimate <- ifelse( (trialNumber == 1) & !is.null(firstEstimate), firstEstimate, mean(c(eLow, eHigh)))
		estimate <- estimate * (1 - accuracyPercent) + (target * accuracyPercent)
		
#		estimate <- getEstimate(data, trialNumber, target, previousTargets[minAdjIdx, "target"], previousTargets[maxAdjIdx, "target"], trialCol = trialCol, targetCol = targetCol, estimateCol = estimateCol, firstEstimate = firstEstimate, accuracyPercent = accuracyPercent, verbose = verbose)
	  
#		rangeTargetsDF <- data.frame(lowerTarget = previousTargets[minAdjIdx, "target"], upperTarget = previousTargets[maxAdjIdx, "target"], estimate = estimate)
	
	if(verbose) {
		print("**** NEW ****")
		print("**** target ****")
		print(target)
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
	}
#	return(rangeTargetsDF)
	return(estimate)
	
}	

ordinalNumberLine2 <- function(targets, upperBound, lowerBound = 0, firstEstimate = NULL, memoryLength = NULL, rangeLength = 1, accuracyPercent = 0, numberSensitivity = 1, referencePoints = NULL, verbose = FALSE) {
  
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
		df.data[df.data$trial == i, "estimate"] <- getEstimateAll(df.data, i, trialCol = "trial", targetCol = "target", estimateCol = "estimate", memoryLength = memoryLength, rangeLength = rangeLength, numberSensitivity = numberSensitivity, accuracyPercent = accuracyPercent, firstEstimate = firstEstimate, verbose = verbose)
	}

	df.out <- df.data[df.data$trial > 0,c("target", "estimate")]
	names(df.out) <- c("target", "fEst")

  return(df.out)
}

# targets <- sample(c(2,5,18,34,56,78, 100, 122, 147, 150, 163, 179, 246, 366, 486, 606, 722, 725, 738, 754, 818, 938))
# loops <- 1000
# firstEstimate <- 500
# accuracyPercent <- 0
# numberSensitivity <- .9
# rangeLength <- 1
# upperBound <- 1000
# lowerBound <- 0
# referencePoints <- c(0, 1000)
# verbose <- FALSE
# memoryLength <- length(targets)
#
# ordinalNumberLine2(targets, upperBound, lowerBound , firstEstimate, memoryLength, rangeLength, accuracyPercent, numberSensitivity, referencePoints, verbose = FALSE)
#

#upper and lower bounds are just (potentially) unseen references
# df.reference <- data.frame(target = c(lowerBound, upperBound), low = c(lowerBound, upperBound), high = c(lowerBound, upperBound))
# if(!is.null(referencePoints)) {
# 	df.reference <- rbind(df.reference, data.frame(target = referencePoints, low = referencePoints, high = referencePoints))
# }
# df.reference <- unique(df.reference)
#
#
# getPreviousTargets(df.reference, targets, length(targets), memoryLength, numberSensitivity)
#
# source("2codes V4.r")

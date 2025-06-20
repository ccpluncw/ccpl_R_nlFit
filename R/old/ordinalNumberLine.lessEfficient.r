#' This function simulates the numberLine process and return the predited estimates.
#'
#' Function that fits the RRW and returns (1-r2), where r2 is the fit of the RRW simulation to the empirical RT and error data
#' this function assumes that participants randomly place thier first response. Then their section response is randomly
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
#''
#' @return a dataframe containing the target value (target) and predicted estimate (fEst).
#' @keywords ordinalNumberline number line 
#' @export
#' @importFrom dplyr %>%
#' @examples ordinalNumberLine (c(2,3,4, 5, 6), 100, 0, firstEstimate = 400, rangeLength = 5, accuracyPercent = 0.5)

ordinalNumberLine.lessEfficient <- function(targets,  upperBound, lowerBound = 0,firstEstimate = NULL, memoryLength = NULL, rangeLength = 2, accuracyPercent = 0, numberSensitivity = 1, referencePoints = NULL) {

	numTargets <- length(targets)
	if(is.null(memoryLength)) memoryLength <- numTargets
	
	df.data <- data.frame(target =  rep(NA,numTargets), fEst = rep(NA,numTargets))
	
	previousTargets <- NULL
	### add reference points to the previous targets vector so they will influence future responses
	if(!is.null(referencePoints)) {
		trial <- Inf #make the reference points "everlasting" so they are unaffected by memoryLength
		numReferencePoints <- length(referencePoints)
		
		previousTargets.tmp <- data.frame(target = referencePoints, estimate = referencePoints, low = referencePoints, high = referencePoints, trial = trial)
	} 
	
	trial <- 1
	i <- 1
	for(ts in targets) {
		numPresentedTargetsInPreviousTargets <- (nrow(previousTargets) - numReferencePoints)

		#if the target is a reference point, make it accurate
		if(ts %in% referencePoints) {
			estimate <- previousTargets[previousTargets$target == ts, "estimate"]
		} else if(trial == 1) {
			if(is.null(firstEstimate)) {
				estmate <- runif(1, lowerBound, upperBound)
			} else {
				estimate <- firstEstimate		
			}		

		} else if(nrow(previousTargets) == 1) {
				#taking the mean is faster than a random draw.  A random draw will produce the mean over time
				estimate <- ifelse(previousTargets$low[1] > ts, mean(c(lowerBound, previousTargets$estimate[1])),
				 ifelse(previousTargets$high[1] < ts, mean(c(previousTargets$estimate[1], upperBound)), 
				 	previousTargets$estimate[1]))			
		} else {				
		
			previousTargets <- previousTargets[order(previousTargets$target), ]
			adjacentIdx <- which.min(abs(previousTargets$target - ts))				
			minAdjIdx <- ifelse(previousTargets$target[adjacentIdx] > ts, adjacentIdx - 1, adjacentIdx)
			minAdjIdx <- ifelse(minAdjIdx < 1, 1, minAdjIdx)
			clMaxAdjIdx <- minAdjIdx + 1
			clMaxAdjIdx <- ifelse(clMaxAdjIdx > nrow(previousTargets), nrow(previousTargets), clMaxAdjIdx)
			
			if((previousTargets$high[minAdjIdx] > ts) & (previousTargets$low[minAdjIdx] < ts)) {
				estimate <- previousTargets$estimate[minAdjIdx]
			} else if ((previousTargets$high[clMaxAdjIdx] > ts) & (previousTargets$low[clMaxAdjIdx] < ts)) {
				estimate <-previousTargets$estimate[clMaxAdjIdx]
			} else {

				clMinIdx <- minAdjIdx - rangeLength + 1 #add 1 because the minAdjIdx is 1 away
				clMinIdx <- ifelse(clMinIdx < 1, 1, clMinIdx)
				clMaxIdx <- clMaxAdjIdx + (rangeLength - 1) #subtract 1 because the minIdx is 1 above
				clMaxIdx <- ifelse(clMaxIdx > nrow(previousTargets), nrow(previousTargets), clMaxIdx)
				estimate <- ifelse(previousTargets$low[clMinIdx] > ts, mean(c(lowerBound, previousTargets$estimate[clMinIdx])),
											ifelse(previousTargets$high[clMaxIdx] < ts, mean(c(previousTargets$estimate[clMaxIdx], upperBound)), 
													mean(c(previousTargets$estimate[clMinIdx] , previousTargets$estimate[clMaxIdx]))))
			}

		}

		estimate <- estimate * (1-accuracyPercent) + ts*accuracyPercent

		if(is.null(previousTargets)) {
			previousTargets <- data.frame(target = ts, estimate = estimate, low = (numberSensitivity * ts), high = ((2-numberSensitivity) * ts), trial = trial) 
		} else if( numPresentedTargetsInPreviousTargets < memoryLength) {
			previousTargets.tmp <- data.frame(target = ts, estimate = estimate, low = (numberSensitivity * ts), high = ((2-numberSensitivity) * ts), trial = trial) 
			previousTargets <- rbind(previousTargets, previousTargets.tmp)
		} else {
			previousTargets <- previousTargets[order(previousTargets$trial), ]
			previousTargets <- previousTargets[-1,]
			previousTargets.tmp <- data.frame(target = ts, estimate = estimate, low = (numberSensitivity * ts), high = ((2-numberSensitivity) * ts), trial = trial) 
			previousTargets <- rbind(previousTargets, previousTargets.tmp)
		}
		df.data$fEst[i] <- estimate
		df.data$target[i] <- ts
		i <- i + 1
		trial <- trial + 1
	}

	return(df.data)
	
}
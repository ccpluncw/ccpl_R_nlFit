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
#' @param upperBound A number that is the upper bound of the numberLine.
#' @param lowerBound A number that is the lower bound of the numberLine. DEFAULT = 0.
#' @param firstEstimate A number that is the first estimated value.  This value will influence the remaining results.  If NULL, then a random draw from between the upperBound and lowerBound will serve as the first estimate.  DEFAULT = NULL
#' @param memoryLength A integer that specifies the number of previous estimates that are remembered. The larger the number, the more accurate the estimates are going to be (given the constraints of the process). DEFAULT = legnth(targets). 
#' @param rangeLength An integer that specifies the size of the window on the vector of remembered estimates that is used to identify the bounds of the next estimate. smaller number will produce more accurate estmates.  DEFAULT = 2
#' @param accuracyPercent a proportion between 0 and 1 that specifies the weight given to accurate responding: 0 = no weight, 1 = perfect responding.  DEFAULT = 0
#''
#' @return a dataframe containing the target value (target) and predicted estimate (fEst).
#' @keywords ordinalNumberline number line 
#' @export
#' @importFrom dplyr %>%
#' @examples ordinalNumberLine (c(2,3,4, 5, 6), 100, 0, firstEstimate = 400, rangeLength = 5, accuracyPercent = 0.5)

ordinalNumberLine.1 <- function(targets,  upperBound, lowerBound = 0,firstEstimate = NULL, memoryLength = NULL, rangeLength = 2, accuracyPercent = 0) {

	numTargets <- length(targets)
	if(is.null(memoryLength)) memoryLength <- numTargets
	
	df.data <- data.frame(target =  rep(NA,numTargets), fEst = rep(NA,numTargets))
		
	previousTargets <-NULL
	trial <- 1
	i <- 1
	for(ts in targets) {
		if(is.null(previousTargets)) {
			
			if(is.null(firstEstimate)) {
				estmate <- runif(1, upperBound, lowerBound)
			} else {
				estimate <- firstEstimate		
			}		

		} else if(nrow(previousTargets) == 1) {

			#taking the mean is faster than a random draw.  A random draw will produce the mean over time
			estimate <- ifelse(previousTargets$target[1] > ts, mean(c(lowerBound, previousTargets$estimate[1])), mean(c(previousTargets$estimate[1], upperBound))) 
			
		} else{				
		
			previousTargets <- previousTargets[order(previousTargets$target), ]
			minIdx <- which.min(abs(previousTargets$target - ts))
			minIdx <- ifelse(previousTargets$target[minIdx] > ts, minIdx - 1, minIdx)
			clMinIdx <- minIdx - rangeLength + 1 #add 1 because the minIdx is 1 away
			clMinIdx <- ifelse(clMinIdx < 1, 1, clMinIdx)
			clMaxIdx <- minIdx + rangeLength
			clMaxIdx <- ifelse(clMaxIdx > nrow(previousTargets), nrow(previousTargets), clMaxIdx)
			
			estimate <- ifelse(previousTargets$target[clMinIdx] > ts, mean(c(lowerBound, previousTargets$estimate[clMinIdx])),
										ifelse(previousTargets$target[clMaxIdx] < ts, mean(c(previousTargets$estimate[clMaxIdx], upperBound)), 
										ifelse(previousTargets$estimate[clMinIdx] > previousTargets$estimate[clMaxIdx], 
											mean(c(previousTargets$estimate[clMaxIdx], previousTargets$estimate[clMinIdx])),
											mean(c(previousTargets$estimate[clMinIdx] , previousTargets$estimate[clMaxIdx])))))

		}

		estimate <- estimate * (1-accuracyPercent) + ts*accuracyPercent
		if(is.null(previousTargets)) {
			previousTargets <- data.frame(target = ts, estimate = estimate, trial = trial) 
		} else if(nrow(previousTargets) < memoryLength) {
			previousTargets.tmp <- data.frame(target = ts, estimate = estimate, trial = trial) 
			previousTargets <- rbind(previousTargets, previousTargets.tmp)
		} else {
			previousTargets <- previousTargets[order(previousTargets$trial), ]
			previousTargets <- previousTargets[-1,]
			previousTargets.tmp <- data.frame(target = ts, estimate = estimate, trial = trial) 
			previousTargets <- rbind(previousTargets, previousTargets.tmp)
		}

		df.data$fEst[i] <- estimate
		df.data$target[i] <- ts
		i <- i + 1
		trial <- trial + 1
	}

	return(df.data)
	
}
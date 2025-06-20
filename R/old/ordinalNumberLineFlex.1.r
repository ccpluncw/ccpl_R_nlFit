#' This function runs the ordinalNumberLine with some flexible parameters.
#'
#' Function that runs the ordinalNumberLine with some flexible parameters
#' @param targets A vector of numbers that are the to-be-estimated values.
#' @param upperBound A number that is the upper bound of the numberLine.
#' @param lowerBound A number that is the lower bound of the numberLine. DEFAULT = 0.
#' @param firstEstimate A number that is the first estimated value.  This value will influence the remaining results.  If NULL, then a random draw from between the upperBound and lowerBound will serve as the first estimate.  DEFAULT = NULL
#' @param memoryLength A integer that specifies the number of previous estimates that are remembered. The larger the number, the more accurate the estimates are going to be (given the constraints of the process). DEFAULT = legnth(targets). 
#' @param rangeLength An integer that specifies the size of the window on the vector of remembered estimates that is used to identify the bounds of the next estimate. smaller number will produce more accurate estmates.  DEFAULT = 2
#' @param accuracyPercent a proportion between 0 and 1 that specifies the weight given to accurate responding: 0 = no weight, 1 = perfect responding.  DEFAULT = 0
#' @param loops A number specifying the number of loops that will be run in the  simulation when it calculates the estimates. This is only useful when targetOrder = "random" because the predictions are deterministic based on the order that the targets are presented. When targetOrder = "random" the loops option will be used. Higher numbers produce more precise estimates, but also increase the time needed to converge on a solution.  Default is 1000.
#' @param targetOrder A string specifying whether to keep the order in targets fixed ("fixed"), to ranndomize the order for every iteration of the loop ("random"), or to randomize it once and then use that order for all the loops ("single").  Default is "random".
#''
#' @return A dataframe containing the target value (target) and predicted estimate (fEst).
#' @keywords ordinalNumberline ordinal number-line
#' @export
#' @examples ordinalNumberLineFles (c(2,3,4, 5, 6), 100, 0, firstEstimate = 400, rangeLength = 5, accuracyPercent = 0.5, targetOrder = "random", loops = 100)


ordinalNumberLineFlex.1 <- function (targets,  upperBound, lowerBound = 0,firstEstimate = NULL, memoryLength = NULL, rangeLength = 2, accuracyPercent = 0, targetOrder = "random", loops = 1000) {

	numTargets <- length(targets)
	
	df.data <- NULL
	if(targetOrder == "single")	{
		targetSeq <- sample(targets)
		loops <- 1
	}
	if(targetOrder == "fixed") {
		targetSeq <- targets
		loops <- 1
	}
	
	for(lps in 1:loops) {

		if(targetOrder == "random") {
			targetSeq <- sample(targets)
		}

		df.tmp <- ordinalNumberLine.1(targets = targetSeq,  upperBound = upperBound, lowerBound = 0,firstEstimate = firstEstimate, memoryLength = memoryLength, rangeLength = rangeLength, accuracyPercent = accuracyPercent)
		df.data <- chutils::ch.rbind(df.data, df.tmp)

	}

	df.sum <- df.data %>% dplyr::group_by (target) %>% dplyr::summarize (fEst = mean(fEst, na.rm = T))
	return(df.sum)

}
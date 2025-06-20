#' This function runs the ordinalNumberLine with some flexible parameters.
#'
#' Function that runs the ordinalNumberLine with some flexible parameters
#' @param targets A vector of numbers that are the to-be-estimated values.
#' @param parList A list with the parameter names and values for the nlFit analysis. The list element name must be the parameter name and the value is the contents. The required elements are: firstEstimate; upperBound; lowerBound; rangeLength; memoryLength; numberSensitivity; accuracyPercent; referencePoinsts; targetOrder.
#' @param loops A number specifying the number of loops that will be run in the  simulation when it calculates the estimates. Higher numbers produce more precise estimates, but also increase the time needed to converge on a solution.  Default = 1000.
#''
#' @return A dataframe containing the target value (target) and predicted estimate (fEst).
#' @keywords ordinalNumberline ordinal number-line
#' @export
#' @examples ordinalNumberLineFles (c(2,3,4, 5, 6), 100, 0, firstEstimate = 400, rangeLength = 5, accuracyPercent = 0.5, targetOrder = "random", loops = 100)


ordinalNumberLineFlex2 <- function (targets, parList, loops = 1000) {

	numTargets <- length(targets)
	
	df.data <- NULL
	if(parList[["targetOrder"]] == "single")	{
		targetSeq <- sample(targets)
		loops <- 1
	}
	if(parList[["targetOrder"]] == "fixed") {
		targetSeq <- targets
		loops <- 1
	}
	
	for(lps in 1:loops) {

		if(parList[["targetOrder"]] == "random") {
			targetSeq <- sample(targets)
		}

		df.tmp <- ordinalNumberLine2(targets = targetSeq,  firstEstimate = parList[["firstEstimate"]], upperBound = parList[["upperBound"]], lowerBound =parList[["lowerBound"]], rangeLength = parList[["rangeLength"]], memoryLength = parList[["memoryLength"]], accuracyPercent = parList[["accuracyPercent"]], numberSensitivity = parList[["numberSensitivity"]], referencePoints = parList[["referencePoints"]])
		df.data <- chutils::ch.rbind(df.data, df.tmp)

	}

	df.sum <- df.data %>% dplyr::group_by (target) %>% dplyr::summarize (fEst = mean(fEst, na.rm = T))
	return(df.sum)

}
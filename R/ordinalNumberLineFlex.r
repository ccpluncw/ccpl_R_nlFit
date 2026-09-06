#' This function runs the ordinalNumberLine with some flexible parameters.
#'
#' Function that runs the ordinalNumberLine with some flexible parameters
#' @param myTargets A vector of numbers that are the to-be-estimated values.
#' @param parList A list with the parameter names and values for the nlFit analysis. The list element name must be the parameter name and the value is the contents. The required elements are: firstEstimate; upperBound; lowerBound; memoryLength; numberSensitivity; accuracyPercent; pIncludeConceptualPoints; visibleReferencePoints; conceptualReferencePoints; targetOrder.
#' @param loops A number specifying the number of loops that will be run in the  simulation when it calculates the estimates. Higher numbers produce more precise estimates, but also increase the time needed to converge on a solution. Loops are collapsed to 1 when the run is deterministic, that is, when the target order is fixed or drawn once and there is no conceptual-point inclusion draw to average over.  Default = 1000.
#' @param verbose A boolean that specifies whether to print intermediate steps. This is used for debugging.  Default is FALSE.
#''
#' @return A dataframe containing the target value (target) and predicted estimate (fEst).
#' @keywords ordinalNumberline ordinal number-line
#' @export
#' @importFrom stats aggregate
#' @examples
#' ordinalNumberLineFlex(c(2, 3, 4, 5, 6),
#'                       parList = list(upperBound = 10, lowerBound = 0,
#'                                      firstEstimate = 0.5, memoryLength = 5,
#'                                      numberSensitivity = 1, accuracyPercent = 0.5,
#'                                      pIncludeConceptualPoints = 0,
#'                                      visibleReferencePoints = c(0, 10),
#'                                      conceptualReferencePoints = NULL,
#'                                      targetOrder = "fixed"),
#'                       loops = 10)


ordinalNumberLineFlex <- function (myTargets, parList, loops = 1000, verbose = FALSE) {

	df.data <- NULL

	#a fixed or once-drawn target order still varies across loops when conceptual points
	#are drawn in, so only collapse the loops when nothing is left to average over.
	deterministicRun <- is.null(parList[["conceptualReferencePoints"]]) ||
		(!is.null(parList[["pIncludeConceptualPoints"]]) &&
			(parList[["pIncludeConceptualPoints"]] == 0 || parList[["pIncludeConceptualPoints"]] == 1))

	if(parList[["targetOrder"]] == "single")	{
		targetSeq <- sample(myTargets)
		if(deterministicRun) loops <- 1
	} else {
		targetSeq <- myTargets
	}
	if(parList[["targetOrder"]] == "fixed" && deterministicRun) {
		loops <- 1
	}

	for(lps in 1:loops) {


		df.tmp <- ordinalNumberLine(targets = targetSeq,  firstEstimate = parList[["firstEstimate"]], upperBound = parList[["upperBound"]], lowerBound =parList[["lowerBound"]], memoryLength = parList[["memoryLength"]], accuracyPercent = parList[["accuracyPercent"]], numberSensitivity = parList[["numberSensitivity"]], pIncludeConceptualPoints = parList[["pIncludeConceptualPoints"]], visibleReferencePoints = parList[["visibleReferencePoints"]], conceptualReferencePoints = parList[["conceptualReferencePoints"]], targetOrder = parList[["targetOrder"]], verbose = verbose)
		df.data <- chutils::ch.rbind(df.data, df.tmp)

	}

	#keep a target whose estimates are all NA rather than dropping it from the output
	df.sum <- aggregate(df.data["fEst"], by = list(target = df.data$target), FUN = mean, na.rm = TRUE)
	return(df.sum)

}

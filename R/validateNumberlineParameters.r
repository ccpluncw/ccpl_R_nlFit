#' This function validates the number line parameters for the fitNL analysis.
#'
#' Function that validates the number line parameters for the fitNL analysis. A searched parameter that is out of range makes the parameter set invalid (FALSE). An absent lowerBound is taken as 0, which is its default everywhere else in the package, so the bound-order check still runs.
#' @param parList A list with the parameter names and values. The list element name must be the parameter name and the value is the contents.
#''
#' @return A boolean that specifies the parameter values as valid (TRUE) or invalid (FALSE).
#' @keywords ordinalNumberline paramters validate
#' @export
#' @examples validateNumberlineParameters (parList = list(upperBound = 100, lowerBound = 0, firstEstimate = 0.5))

validateNumberlineParameters <- function(parList) {
	out <- TRUE
	validTargetOrders <- c("fixed", "random", "single")

	if(is.null(parList[["upperBound"]])) {
		out <- FALSE
	} else {
		#lowerBound defaults to 0 wherever it is an argument, so an absent one is 0 here too
		lowerBound <- if(is.null(parList[["lowerBound"]])) 0 else parList[["lowerBound"]]
		if(parList[["upperBound"]] < lowerBound) out <- FALSE
	}

	if(!is.null(parList[["memoryLength"]])) {
		if(parList[["memoryLength"]] < 1) out <- FALSE
	}

	if(!is.null(parList[["accuracyPercent"]])) {
		if(parList[["accuracyPercent"]] < 0 | parList[["accuracyPercent"]] > 1) out <- FALSE
	}

	if(!is.null(parList[["numberSensitivity"]])) {
		if(parList[["numberSensitivity"]] < 0 | parList[["numberSensitivity"]] > 1) out <- FALSE
	}

	if(!is.null(parList[["firstEstimate"]])) {
		if(parList[["firstEstimate"]] < 0 | parList[["firstEstimate"]] > 1) out <- FALSE
	}

	if(!is.null(parList[["pConceptual"]])) {
		if(parList[["pConceptual"]] < 0 | parList[["pConceptual"]] > 1) out <- FALSE
	}

	if(!is.null(parList[["targetOrder"]])) {
		if(!(parList[["targetOrder"]] %in% validTargetOrders)) out <- FALSE
	}

	return(out)
}

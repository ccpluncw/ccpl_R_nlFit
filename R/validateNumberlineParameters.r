#' This function validates the number line parameters for the fitNL analysis.
#'
#' Function that validates the number line parameters for the fitNL analysis
#' @param parList A list with the parameter names and values. The list element name must be the parameter name and the value is the contents.
#''
#' @return A boolean that specifies the parameter values as valid (TRUE) or invalid (FALSE).
#' @keywords ordinalNumberline paramters validate
#' @export
#' @examples validateNumberlineParameters (parList = list(upperBound = 100, lowerBound = 0, firstEstimate = 50))

validateNumberlineParameters <- function(parList) {
	out <- TRUE
	validTargetOrders <- c("fixed", "random", "single")

	if(is.null(parList[["upperBound"]])) {
		out <- FALSE
	} else {
		if(is.null(parList[["lowerBound"]])) {
			if(parList[["upperBound"]] <= 0) out <- FALSE
		} else {
			if(parList[["upperBound"]] < parList[["lowerBound"]]) out <- FALSE
		}

	}

	if(!is.null(parList[["memoryLength"]])) {
		if(parList[["memoryLength"]] < 1) out <- FALSE
	}
	if(!is.null(parList[["rangeLength"]])) {
		if(parList[["rangeLength"]] < 1) out <- FALSE
	}

	if(!is.null(parList[["accuracyPercent"]])) {
		if(parList[["accuracyPercent"]] < 0 | parList[["accuracyPercent"]] > 1) out <- FALSE
	}

	if(!is.null(parList[["numberSensitivity"]])) {
		if(parList[["numberSensitivity"]] < 0 | parList[["numberSensitivity"]] > 1) out <- FALSE
	}

	if(!is.null(parList[["pIncludeConceptualPoints"]])) {
		if(parList[["pIncludeConceptualPoints"]] < 0 | parList[["pIncludeConceptualPoints"]] > 1) out <- FALSE
	}

	if(!is.null(parList[["targetOrder"]])) {
		if(!(parList[["targetOrder"]] %in% validTargetOrders)) out <- FALSE
	}

	return(out)
}

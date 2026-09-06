#' This function ensures that the parameter list the fitNL analysis is complete.
#'
#' Function that ensures that the parameter list the fitNL analysis is complete. This means that a parameter that is null is assigned the default value. memoryLength is not filled here because its default is the number of targets, which is known only at simulation time; ordinalNumberLine() sets it.
#' @param parList A list with the parameter names and values. The list element name must be the parameter name and the value is the contents.
#''
#' @return The filled parameter list.
#' @keywords ordinalNumberline paramters fill
#' @export
#' @examples fillNumberlineParList (parList = list(upperBound = 100, lowerBound = 0, firstEstimate = 0.5))


fillNumberlineParList <- function(parList) {
	if(is.null(parList[["lowerBound"]])) parList[["lowerBound"]] <- 0
	if(is.null(parList[["accuracyPercent"]])) parList[["accuracyPercent"]] <- 0
	if(is.null(parList[["numberSensitivity"]])) parList[["numberSensitivity"]] <- 1
	if(is.null(parList[["pIncludeConceptualPoints"]])) parList[["pIncludeConceptualPoints"]] <- 0
	if(is.null(parList[["targetOrder"]])) parList[["targetOrder"]] <- "random"

	return(parList)
}

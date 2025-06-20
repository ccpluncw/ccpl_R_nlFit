#' This function ensures that the parameter list the fitNL analysis is complete.
#'
#' Function that ensures that the parameter list the fitNL analysis is complete. This means that a parameter that is null is assigned the default value
#' @param parList A list with the parameter names and values. The list element name must be the parameter name and the value is the contents.
#''
#' @return The filled parameter list.
#' @keywords ordinalNumberline paramters fill
#' @export
#' @examples fillNumberlineParList (parList = list(upperBound = 100, lowerBound = 0, firstEstimate = 50))


fillNumberlineParList <- function(parList) {
	if(is.null(parList[["lowerBound"]])) parList[["lowerBound"]] <- 0
	if(is.null(parList[["rangeLength"]])) parList[["rangeLength"]] <- 2
	if(is.null(parList[["accuracyPercent"]])) parList[["accuracyPercent"]] <- 0
	if(is.null(parList[["numberSensitivity"]])) parList[["numberSensitivity"]] <- 1
	if(is.null(parList[["targetOrder"]])) parList[["targetOrder"]] <- "random"

	return(parList)
}
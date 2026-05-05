#' This function outputs the results of the fitNL analysis.
#'
#' Function that outputs the results of the fitNL analysis
#' @param data This is a dataframe that must contain the following columns: target; estimate. The dataset can also contain columns that effect code the influence of different parameters (to be implemented).
#' @param parList A list with the parameter names and values for the nlFit analysis. The list element name must be the parameter name and the value is the contents. The required elements are: firstEstimate; upperBound; lowerBound; rangeLength; memoryLength; targetOrder; numberSensitivity, accuracyPercent.
#' @param dataTargetCol A string that identifies the name of the column in data that contains the target values. The default is "target"
#' @param dataEstimateCol A string that identifies the name of the column in data that contains the participant's estimate values. The default is "estimate"
#' @param pars.n The number of free parameters. When NULL, the program will assume the maximum number of free parameters. Default = 4.
#' @param loops A number specifying the number of loops that will be run in the  simulation when it calculates the estimates. Higher numbers produce more precise estimates, but also increase the time needed to converge on a solution.  Default is 1000.
#' @param sinkFilename A string that identifies the name of file (.txt) in which the fit statistics will be saved. The default is NULL, whereby the fit statistics will not be saved.
#' @param appendSinkFile A boolean to specify whether the new data should be appended to the output file (TRUE) or write over the output file (FALSE). Default = TRUE
#' @param plotFilename A string that identifies the name of file (.pdf) in which the data plot will be saved. The default is NULL, whereby the plot will not be saved.
#' @param multicore A boolean that specifies whether to run the process on multiple cores.  Default is FALSE.
#''
#' @return The function returns a list containing (1) a list of the parameter values and fit statistics [runStats], and (2) a dataframe [df.fitted] that contains the "data" plus the fitted values from the model (fEst))
#' @keywords ordinalNumberline fit statistics
#' @export
#' @examples outputNLfitStats (data, parList = list(upperBound = 100, lowerBound = 0, firstEstimate = 50, rangeLength = 4, memoryLength = 20, ))

outputNLfitStats <- function(data, statList, dataTargetCol = "target", dataEstimateCol = "estimate", pars.n = 1, loops = 1000, sinkFilename = NULL, appendSinkFile = TRUE, plotFileName = NULL, xlim = NULL, ylim = NULL, multicore = FALSE) {

	if(multicore) {
		df.fitted <- ordinalNumberLineFlex_mc (data[[dataTargetCol]], statList, loops=loops)
	} else {
		df.fitted <- ordinalNumberLineFlex (data[[dataTargetCol]], statList, loops=loops)
	}

	df.fitted <- merge(df.fitted, data,  by.x = "target", by.y = dataTargetCol)

	fit.r2 <- round(chutils::ch.R2(df.fitted[[dataEstimateCol]], df.fitted[["fEst"]]),2)
	fit.BIC <- round(chutils::ch.IC(df.fitted[[dataEstimateCol]], df.fitted[["fEst"]], pars.n, ICtype = "BIC"), 0)
	fit.AIC <- round(chutils::ch.IC(df.fitted[[dataEstimateCol]], df.fitted[["fEst"]], pars.n, ICtype = "AIC"), 0)

	if(is.null(xlim)) xlim <- c(min(statList$lowerBound, data[[dataTargetCol]]), max(statList$upperBound, data[[dataTargetCol]]))
	if(is.null(ylim)) ylim <- c(min(statList$lowerBound, data[[dataEstimateCol]], df.fitted$fEst), max(statList$upperBound, data[[dataEstimateCol]], df.fitted$fEst))
	pdf(plotFileName)
		plot(data[[dataEstimateCol]] ~ data[[dataTargetCol]], ylim = ylim, xlim = xlim)
		#with(data, plot(estimate ~ target, ylim = yLim, xlim = xLim))
		with(df.fitted, lines(fEst ~ target, col = "blue"))
		text( (0.7*xlim[2]), (0.15*ylim[2]), paste("r2 =", fit.r2, "BIC =", fit.BIC))
		abline(0,1)
		cat("\nr2 =", fit.r2 , "; BIC =", fit.BIC, "\n\n")
	dev.off()

  sink(sinkFilename, append = appendSinkFile)
    cat("\n\n **************** nlFit Statistics **************** \n\n")

    cat("\n\n ******** Number Line Info ******** \n\n")
    cat("Lower Bound = ", statList$lowerBound, "\n\n")
    cat("Upper Bound = ", statList$upperBound, "\n\n")
		if(is.null(statList$visibleReferencePoints)) visibleReferencePoints <- "NA"
			else visibleReferencePoints <- statList$visibleReferencePoints
    cat("Visible Reference Points = ", visibleReferencePoints, "\n\n")
		if(is.null(statList$conceptualReferencePoints)) conceptualReferencePoints <- "NA"
			else conceptualReferencePoints <- statList$conceptualReferencePoints
		cat("Conceptual Reference Points = ", conceptualReferencePoints, "\n\n")
    cat("Target Order = ", statList$targetOrder, "\n\n")

    cat("\n\n ******** Parameter Values ******** \n\n")

    cat("First Estimate = ", statList$firstEstimate, "\n\n")
    cat("Range Length = ", statList$rangeLength, "\n\n")
    cat("Memory Length = ", statList$memoryLength, "\n\n")
    cat("Number Sensitivity = ", statList$numberSensitivity, "\n\n")
    cat("Accuracy Percent = ", statList$accuracyPercent, "\n\n")
		pIncludeConceptualPoints <- ifelse(is.null(statList$pIncludeConceptualPoints), "NA", statList$pIncludeConceptualPoints)
		cat("p(conceptualReferencePoints) = ", pIncludeConceptualPoints, "\n\n")

    cat("\n\n ******** Final Model Fit Statistics ******** \n\n")

    cat(" N Parameters = ", pars.n, "\n\n")
    cat(" R Square = ", fit.r2, "\n")
    cat(" BIC = ", fit.BIC, "\n")
    cat(" AIC = ", fit.AIC, "\n")

  sink(NULL)


  fitStats <- list(AIC = fit.AIC, BIC = fit.BIC, r2 = fit.r2, freeParameters = pars.n)

  runStats <- list(parameters = statList, fitStats = fitStats)

  outlist <- list(df.fitted = df.fitted, runStats = runStats)

	return(outlist)
}

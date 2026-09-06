#' This function outputs the results of the fitNL analysis.
#'
#' Function that outputs the results of the fitNL analysis
#' @param data This is a dataframe that must contain the following columns: target; estimate. It may hold one row per target, the usual case, or one row per trial with a presentation column, in which case the fit is trial level. A trial-level dataframe has more rows per participant than an averaged one at the same pars.n, so its BIC and AIC are on a different scale and are not comparable with those of an averaged fit. The dataset can also contain columns that effect code the influence of different parameters (to be implemented).
#' @param statList A list with the parameter names and values for the nlFit analysis. The list element name must be the parameter name and the value is the contents. The required elements are: firstEstimate; upperBound; lowerBound; memoryLength; targetOrder; numberSensitivity; accuracyPercent; pIncludeConceptualPoints; visibleReferencePoints; conceptualReferencePoints.
#' @param dataTargetCol A string that identifies the name of the column in data that contains the target values. The default is "target"
#' @param dataEstimateCol A string that identifies the name of the column in data that contains the participant's estimate values. The default is "estimate"
#' @param dataPresentationCol A string that identifies the name of the column in data that numbers the occurrences of a repeated target value, counted in the order they were presented. When data has that column the simulation is matched to the data trial by trial, on target and presentation. When it does not, the simulated estimates are averaged over the presentations of each value and matched on target alone, which is what an averaged dataset needs. The default is "presentation".
#' @param pars.n The number of free parameters, that is, the number of parameters the search varied. This is required; there is no default.
#' @param loops A number specifying the number of loops that will be run in the  simulation when it calculates the estimates. Higher numbers produce more precise estimates, but also increase the time needed to converge on a solution.  Default is 1000.
#' @param sinkFilename A string that identifies the name of file (.txt) in which the fit statistics will be saved. The default is NULL, whereby the fit statistics are written to the console.
#' @param appendSinkFile A boolean to specify whether the new data should be appended to the output file (TRUE) or write over the output file (FALSE). Default = TRUE
#' @param plotFileName A string that identifies the name of file (.pdf) in which the data plot will be saved. The default is NULL, whereby the plot is drawn on the current graphics device.
#' @param xlim A vector of two numbers giving the x axis limits of the plot. When NULL, the limits span the bounds and the targets in data. Default is NULL.
#' @param ylim A vector of two numbers giving the y axis limits of the plot. When NULL, the limits span the bounds, the estimates in data, and the fitted estimates. Default is NULL.
#' @param seed The random seed the caller set before running the fit. It is recorded in the output header so a run can be reproduced. The function does not set the seed itself. Default is NULL, whereby no seed is reported.
#''
#' @return The function returns a list containing (1) a list of the parameter values and fit statistics (`runStats`), and (2) a dataframe (`df.fitted`) that contains the "data" plus the fitted values from the model (fEst))
#' @keywords ordinalNumberline fit statistics
#' @export
#' @importFrom grDevices pdf dev.off
#' @importFrom graphics abline lines text
#' @importFrom utils packageVersion
#' @importFrom stats aggregate
#' @examples
#' df <- data.frame(target = c(10, 30, 50, 70, 90), estimate = c(20, 35, 48, 66, 88))
#' statList <- list(upperBound = 100, lowerBound = 0, firstEstimate = 0.5,
#'                  memoryLength = 5, numberSensitivity = 0.8, accuracyPercent = 0.2,
#'                  pIncludeConceptualPoints = 0, visibleReferencePoints = c(0, 100),
#'                  conceptualReferencePoints = NULL, targetOrder = "fixed")
#' outputNLfitStats(df, statList, pars.n = 3, loops = 10)

outputNLfitStats <- function(data, statList, dataTargetCol = "target", dataEstimateCol = "estimate", pars.n, loops = 1000, sinkFilename = NULL, appendSinkFile = TRUE, plotFileName = NULL, xlim = NULL, ylim = NULL, seed = NULL, dataPresentationCol = "presentation") {

	if(missing(pars.n) || is.null(pars.n)) {
		stop("outputNLfitStats: pars.n is required. Set it to the number of parameters the search varied.")
	}

	df.fitted <- ordinalNumberLineSim(data[[dataTargetCol]], statList, loops=loops)

	#trial-level data are matched presentation by presentation; averaged data have no
	#presentation column, so the presentations of a repeated value are averaged too.
	if(!is.null(dataPresentationCol) && dataPresentationCol %in% names(data)) {
		df.fitted <- merge(df.fitted, data, by.x = c("target", "presentation"), by.y = c(dataTargetCol, dataPresentationCol))
	} else {
		df.fitted <- aggregate(df.fitted["fEst"], by = list(target = df.fitted$target), FUN = mean, na.rm = TRUE)
		df.fitted <- merge(df.fitted, data,  by.x = "target", by.y = dataTargetCol)
	}

	fit.r2 <- round(chutils::ch.R2(df.fitted[[dataEstimateCol]], df.fitted[["fEst"]]),2)
	fit.BIC <- round(chutils::ch.IC(df.fitted[[dataEstimateCol]], df.fitted[["fEst"]], pars.n, ICtype = "BIC"), 0)
	fit.AIC <- round(chutils::ch.IC(df.fitted[[dataEstimateCol]], df.fitted[["fEst"]], pars.n, ICtype = "AIC"), 0)

	if(is.null(xlim)) xlim <- c(min(statList$lowerBound, data[[dataTargetCol]]), max(statList$upperBound, data[[dataTargetCol]]))
	if(is.null(ylim)) ylim <- c(min(statList$lowerBound, data[[dataEstimateCol]], df.fitted$fEst), max(statList$upperBound, data[[dataEstimateCol]], df.fitted$fEst))

	#without a file name the plot belongs on whatever device the caller already has open
	if(!is.null(plotFileName)) pdf(plotFileName)
		plot(data[[dataEstimateCol]] ~ data[[dataTargetCol]], ylim = ylim, xlim = xlim)
		with(df.fitted, lines(fEst ~ target, col = "blue"))
		text( (0.7*xlim[2]), (0.15*ylim[2]), paste("r2 =", fit.r2, "BIC =", fit.BIC))
		abline(0,1)
		cat("\nr2 =", fit.r2 , "; BIC =", fit.BIC, "\n\n")
	if(!is.null(plotFileName)) dev.off()

	#without a file name the statistics go to the console; sink(NULL) would otherwise
	#close a connection the caller opened.
  if(!is.null(sinkFilename)) sink(sinkFilename, append = appendSinkFile)
    cat("\n\n **************** nlFit Statistics **************** \n\n")

    cat("nlFit version = ", as.character(packageVersion("nlFit")), "\n\n")
    cat("Run date = ", format(Sys.time()), "\n\n")
    cat("Loops = ", loops, "\n\n")
		if(!is.null(seed)) cat("Seed = ", seed, "\n\n")

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

  if(!is.null(sinkFilename)) sink(NULL)


  fitStats <- list(AIC = fit.AIC, BIC = fit.BIC, r2 = fit.r2, freeParameters = pars.n)

  runStats <- list(parameters = statList, fitStats = fitStats)

  outlist <- list(df.fitted = df.fitted, runStats = runStats)

	return(outlist)
}

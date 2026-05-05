#' This function fits the ordinalNumberLine and returns a fit statistic.
#'
#' Function that fits fits the ordinalNumberLine and returns a fit statistic
#' @param data This is a dataframe that must contain the following columns: target; estimate. The dataset can also contain columns that effect code the influence of different parameters (to be implemented).
#' @param upperBound A number that is the upper bound of the numberLine.
#' @param lowerBound A number that is the lower bound of the numberLine. DEFAULT = 0.
#' @param firstEstimate A proportion between 0 and 1 that idenitifies the bias of the first estimated value. The firstEstimate will always fall in the region between the visible reference points (or upper and lower bound) that retain ordinality. The first estmate is a value between 0-1, where 0 specifies the lowest point in that region, 1 specifies the highest point in that region, etc. This value will influence the remaining results.  If NULL, then a random draw from between relevant region will serve as the first estimate.  DEFAULT = NULL
#' @param memoryLength A integer that specifies the number of previous estimates that are remembered. The larger the number, the more accurate the estimates are going to be (given the constraints of the process). DEFAULT = legnth(targets).
#' @param rangeLength An integer that specifies the size of the window on the vector of remembered estimates that is used to identify the bounds of the next estimate. smaller number will produce more accurate estmates.  DEFAULT = 2
#' @param accuracyPercent a proportion between 0 and 1 that specifies the weight given to accurate responding: 0 = no weight, 1 = perfect responding.  DEFAULT = 0
#' @param numberSensitivity A proportion between 0 and 1 that specifies the range of equivelance. When numberSensitivity=0, the target is not differentiated from the nearest referenece point. When numberSensitivity=1, the target is differentiated from all other targets. When 0< numberSensitivity < 1, the range of equivelence is proportional to the value. DEFAULT = 1 (perfect precision)
#' @param visibleReferencePoints A vector of numbers that specify the visible (displayed) points that identify the value of positions on the number line. These are the upper and lower bound of the bounded number line, 0 and 1 for the unbounded number line, and anything the researcher uses for the universal number line or learning tasks.  Default is NULL.
#' @param conceptualReferencePoints A vector of numbers that specify any conceptual points that the participant may use to identify the value of positions on the number line. These may be the middle of the bounded number-line, or a learned position on the number_line. Currently, conceptualReferencePoints are only modeled if they are used thoughout the task (not introduced part of the way through) Default is NULL.
#' @param loops A number specifying the number of loops that will be run in the  simulation when it calculates the estimates. Higher numbers produce more precise estimates, but also increase the time needed to converge on a solution.  Default is 1000.
#' @param targetOrder A string specifying whether to keep the order in targets fixed ("fixed"), to ranndomize the order for every iteration of the loop ("random"), or to randomize it once and then use that order for all the loops ("single").  Default is "random".
#' @param dataTargetCol A string that identifies the name of the column in data that contains the target values. The default is "target"
#' @param dataEstimateCol A string that identifies the name of the column in data that contains the participant's estimate values. The default is "estimate"
#' @param minimizeStat A string that specifies which statistic to minimize when optimizing the model fit.  The options are: "BIC" , "AIC" , or "R_Square". Default is "BIC".
#' @param pars.n The number of free parameters. When NULL, the program will attempt to calculate the number of free parameters from the input. Default = NULL.
#' @param verbose A boolean that specifies whether to print intermediate steps. This is used for debugging.  Default is FALSE.
#' @param multicore A boolean that specifies whether to run the process on multiple cores.  Default is FALSE.
#''
#' @return The minimization statistic for the fit of the model to the data.  This is the value that will be miniized by the optimization program.
#' @keywords ordinalNumberline ordinal number-line
#' @export
#' @examples getOrdinalNumberlineFit (data, parList = list(upperBound = 100, lowerBound = 0, firstEstimate = 50))

getOrdinalNumberlineFit <- function(data, upperBound, lowerBound = 0,firstEstimate = NULL, memoryLength = NULL, rangeLength = 2, accuracyPercent = 0, numberSensitivity = 1, pIncludeConceptualPoints = 0, visibleReferencePoints = NULL, conceptualReferencePoints = NULL, loops = 1000, targetOrder = "random", dataTargetCol = "target", dataEstimateCol = "estimate", minimizeStat = 'BIC', pars.n = NULL, verbose = FALSE, multicore = FALSE) {

	parList <- list(upperBound = upperBound, lowerBound = lowerBound,firstEstimate = firstEstimate, memoryLength = memoryLength, rangeLength = rangeLength, numberSensitivity = numberSensitivity, pIncludeConceptualPoints = pIncludeConceptualPoints, visibleReferencePoints = visibleReferencePoints, conceptualReferencePoints = conceptualReferencePoints, accuracyPercent = accuracyPercent, targetOrder = targetOrder)

  if(is.null(pars.n)) {
    pars.n = length((parList))
  }

  #make sure minimizeStat is valid
  minimizeOpts <- c("BIC", "AIC", "R_Square")
  if(!(minimizeStat %in% minimizeOpts) ) {
    stop (paste("you set minimizeStat to:", minimizeStat, ", but it must be one of the following:", minimizeOpts, sep=" "))
  }

  #make sure that the input parameters from the grid search are valid
  validParams <- validateNumberlineParameters(parList)

  #if the parameters from the grid search are valid, see how well they fit the data
  if(validParams == TRUE) {

		parList <- fillNumberlineParList(parList)

		if(multicore) {
			df.fitted <- ordinalNumberLineFlex_mc(data[[dataTargetCol]], parList, loops=loops, verbose = verbose)
		} else {
			df.fitted <- ordinalNumberLineFlex(data[[dataTargetCol]], parList, loops=loops, verbose = verbose)
		}

		df.dataFit <- merge(data, df.fitted, by.x = dataTargetCol, by.y = "target")
    #get potential minimization variable (1-r2)
    out.rss <- 1 - chutils::ch.R2(df.dataFit[[dataEstimateCol]], df.dataFit[["fEst"]])

    #get potential minimization variable BIC
    out.BIC <- chutils::ch.IC(df.dataFit[[dataEstimateCol]], df.dataFit[["fEst"]], pars.n, ICtype = "BIC")
    out.AIC <- chutils::ch.IC(df.dataFit[[dataEstimateCol]], df.dataFit[["fEst"]], pars.n, ICtype = "AIC")
  } else {
    #if the parameters are not valid, then assign the minimization variable Infinite value (Inf)
    out.rss <- Inf
    out.BIC <- Inf
    out.AIC <- Inf
  }

  #output the minimization variable
  switch(minimizeStat, BIC = return(out.BIC), AIC = return(out.AIC), R_Square = return(out.rss))
}

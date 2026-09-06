#' This function fits the ordinalNumberLine and returns a fit statistic.
#'
#' Function that fits fits the ordinalNumberLine and returns a fit statistic
#' @param data This is a dataframe that must contain the following columns: target; estimate. The dataset can also contain columns that effect code the influence of different parameters (to be implemented).
#' @param upperBound A number that identifes the upper point beyond which the partcipant cannot respond. In the bounded task this is the upper end of the number line and is also passed as a visible reference point. In the universal task it is the upper screen edge.
#' @param lowerBound A number that identifes the lower point beyond which the partcipant cannot respond. In the bounded task this is the lower end of the number line and is also passed as a visible reference point. In the universal task it is the lower screen edge. DEFAULT = 0.
#' @param firstEstimate A proportion between 0 and 1 that identifies the spatial bias applied whenever a target is not bracketed by a remembered estimate, that is, whenever both of its anchors are reference points. 0 places the estimate at the bottom of the region between the two anchors, 1 at the top.  If NULL, the midpoint of the region is used.  DEFAULT = NULL
#' @param memoryLength An integer that specifies the number of previous estimates that are remembered. On trial t the remembered set is the estimates from trials t-memoryLength through t-1, plus every reference point. The larger the number, the more accurate the estimates are going to be (given the constraints of the process). DEFAULT = length(targets).
#' @param accuracyPercent a proportion between 0 and 1 that specifies the weight given to accurate responding: 0 = no weight, 1 = perfect responding.  DEFAULT = 0
#' @param numberSensitivity A proportion between 0 and 1 that specifies the range of equivelance. A number's range is scaled by its distance to the nearest reference point. When numberSensitivity=1, every number is discriminated from every other. When numberSensitivity=0, a number is not discriminated from anything within its distance to the nearest reference point. DEFAULT = 1 (perfect precision)
#' @param pIncludeConceptualPoints A proportion between 0 and 1 that specifies the probability that a conceptualReferencePoint is available on a run. Each conceptual point is drawn independently, once per run.  DEFAULT = 0
#' @param visibleReferencePoints A vector of numbers that specify the visible (displayed) points that identify the value of positions on the number line. In the bounded task these are the upper and lower bound; in the universal task they are the labelled values and the bounds are the screen edges. This is required; there is no default.
#' @param conceptualReferencePoints A vector of numbers that specify any conceptual points that the participant may use to identify the value of positions on the number line. These may be the middle of the bounded number-line, or a learned position on the number_line. Currently, conceptualReferencePoints are only modeled if they are used thoughout the task (not introduced part of the way through) Default is NULL.
#' @param loops A number specifying the number of loops that will be run in the  simulation when it calculates the estimates. Higher numbers produce more precise estimates, but also increase the time needed to converge on a solution.  Default is 1000.
#' @param targetOrder A string specifying whether to keep the order in targets fixed ("fixed"), to ranndomize the order for every iteration of the loop ("random"), or to randomize it once and then use that order for all the loops ("single").  Default is "random".
#' @param dataTargetCol A string that identifies the name of the column in data that contains the target values. The default is "target"
#' @param dataEstimateCol A string that identifies the name of the column in data that contains the participant's estimate values. The default is "estimate"
#' @param minimizeStat A string that specifies which statistic to minimize when optimizing the model fit.  The options are: "BIC" , "AIC" , or "R_Square". Default is "BIC".
#' @param pars.n The number of free parameters, that is, the number of parameters the search varies. This is required; there is no default.
#' @param verbose A boolean that specifies whether to print intermediate steps. This is used for debugging.  Default is FALSE.
#' @param multicore A boolean that specifies whether to run the process on multiple cores.  Default is FALSE.
#''
#' @return The minimization statistic for the fit of the model to the data.  This is the value that will be miniized by the optimization program.
#' @keywords ordinalNumberline ordinal number-line
#' @export
#' @examples
#' df <- data.frame(target = c(10, 30, 50, 70, 90), estimate = c(20, 35, 48, 66, 88))
#' getOrdinalNumberlineFit(df, upperBound = 100, lowerBound = 0,
#'                         visibleReferencePoints = c(0, 100),
#'                         firstEstimate = 0.5, numberSensitivity = 0.8,
#'                         accuracyPercent = 0.2, loops = 10, pars.n = 3)

getOrdinalNumberlineFit <- function(data, upperBound, lowerBound = 0,firstEstimate = NULL, memoryLength = NULL, accuracyPercent = 0, numberSensitivity = 1, pIncludeConceptualPoints = 0, visibleReferencePoints = NULL, conceptualReferencePoints = NULL, loops = 1000, targetOrder = "random", dataTargetCol = "target", dataEstimateCol = "estimate", minimizeStat = 'BIC', pars.n, verbose = FALSE, multicore = FALSE) {

  #these are structural inputs, not points in the search space, so a bad one stops the
  #run rather than scoring Inf and leaving the search without a signal.
  if(missing(pars.n) || is.null(pars.n)) {
    stop("getOrdinalNumberlineFit: pars.n is required. Set it to the number of parameters the search varies.")
  }
  if(is.null(visibleReferencePoints)) {
    stop("getOrdinalNumberlineFit: visibleReferencePoints is required. Pass the bounds for a bounded number line, or the labelled values for a universal number line.")
  }

  parList <- list(upperBound = upperBound, lowerBound = lowerBound,firstEstimate = firstEstimate, memoryLength = memoryLength, numberSensitivity = numberSensitivity, pIncludeConceptualPoints = pIncludeConceptualPoints, visibleReferencePoints = visibleReferencePoints, conceptualReferencePoints = conceptualReferencePoints, accuracyPercent = accuracyPercent, targetOrder = targetOrder)

  #make sure minimizeStat is valid
  minimizeOpts <- c("BIC", "AIC", "R_Square")
  if(!(minimizeStat %in% minimizeOpts) ) {
    stop (paste0("you set minimizeStat to: ", minimizeStat, ", but it must be one of the following: ", paste(minimizeOpts, collapse = ", ")))
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

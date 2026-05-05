#' This function simulates the numberLine process and return the predited estimates.
#'
#' Function that simulates the numberLine process and return the predited estimates.
#' This function assumes that participants randomly place thier first response. Then their section response is randomly
#' presented between the first response and either the upper or lower boundary (depending on whether the target is greater
#' than or less than (respectively) the first target). Then, the partcipant will place the following estimates  ordinally
#' in relation to the previous estimates (and the upper and lower bounds). This is moderated by the memoryLength, which
#' specifies how well they remember the previous estimates (larger is better) and rangeLength, which specifies
#' how close they set the bounds around the remembered items (smaller is better). Finally, accuracyPercent specifies
#' the influence of accuracy on the the estimates.  This is a proportion, between 0 and 1, whereby estimate is
#' calculated as follows: estimate = (estimate based on algroithm * 1-accuracyPercent) + presented target * accuracyPercent
#'
#' @param targets A vector of numbers that are the to-be-estimated values.
#' @param upperBound A number that identifes the upper point beyond which the partcipant cannot respond.  This is the upperBound of the bounded number line or the upper screen edge when the unbounded number line is used. This must be specified in "number-line units."
#' @param lowerBound A number that identifes the lower point beyond which the partcipant cannot respond.  This is the lowerBound of the bounded and unbounded number line or the lower screen edge when the universal number line is used.  DEFAULT = 0.
#' @param firstEstimate A number that is the first estimated value.  This value will influence the remaining results.  If NULL, then a random draw from between the upperBound and lowerBound will serve as the first estimate.  DEFAULT = NULL
#' @param memoryLength A integer that specifies the number of previous estimates that are remembered. The larger the number, the more accurate the estimates are going to be (given the constraints of the process). DEFAULT = legnth(targets).
#' @param rangeLength An integer that specifies the size of the window on the vector of remembered estimates that is used to identify the bounds of the next estimate. smaller number will produce more accurate estmates.  DEFAULT = 2
#' @param accuracyPercent A proportion between 0 and 1 that specifies the weight given to accurate responding: 0 = no weight, 1 = perfect responding.  DEFAULT = 0
#' @param numberSensitivity A proportion between 0 and 1 that specifies the precision that previous targets are remembered. Specifically, it is the range of equivelance, where all targets between a low of (numberSensitivity * target) and a high of (2-numberSensitivity) are treated as equivelent and will be put in the same place on the number-line. DEFAULT = 1 (perfect precision)
#' @param visibleReferencePoints A vector of numbers that specify the visible (displayed) points that identify the value of positions on the number line. These are the upper and lower bound of the bounded number line, 0 and 1 for the unbounded number line, and anything the researcher uses for the universal number line or learning tasks.  Default is NULL.
#' @param conceptualReferencePoints A vector of numbers that specify any conceptual points that the participant may use to identify the value of positions on the number line. These may be the middle of the bounded number-line, or a learned position on the number_line. Currently, conceptualReferencePoints are only modeled if they are used thoughout the task (not introduced part of the way through) Default is NULL.
#' @param targetOrder A string specifying whether to keep the order in targets fixed ("fixed"), to ranndomize the order for every iteration of the loop ("random"), or to randomize it once and then use that order for all the loops ("single").  Default is "random".
#' @param verbose A boolean that specifies whether to print intermediate steps. This is used for debugging.  Default is FALSE.
#''
#' @return a dataframe containing the target value (target) and predicted estimate (fEst).
#' @keywords ordinalNumberline number line
#' @export
#' @importFrom dplyr %>%
#' @examples ordinalNumberLine (c(2,3,4, 5, 6), 100, 0, firstEstimate = 400, rangeLength = 5, accuracyPercent = 0.5)

ordinalNumberLineX <- function(targets, upperBound, lowerBound = 0, firstEstimate = NULL, memoryLength = NULL, rangeLength = 1, accuracyPercent = 0, numberSensitivity = 1, pIncludeConceptualPoints = 0, visibleReferencePoints = NULL, conceptualReferencePoints = NULL, targetOrder = "random", verbose = FALSE) {


  # if targetOrder is random, then randomize the targets
  if(targetOrder == "random") {
    targets <- sample(targets)
  }

  numTargets <- length(targets)
  if (is.null(memoryLength)) memoryLength <- numTargets

  #upper and lower bounds are just (potentially) unseen references
  #to be added below to df.reference
  df.bounds <- data.frame(trial = 0, target = c(lowerBound, upperBound), low = c(lowerBound, upperBound), high = c(lowerBound, upperBound), estimate = c(lowerBound, upperBound), targetValue = c(lowerBound, upperBound))

  #make visible reference points available
	if(!is.null(visibleReferencePoints)) {
    df.reference <- data.frame(trial = 0, target = visibleReferencePoints, low = visibleReferencePoints, high = visibleReferencePoints, estimate = visibleReferencePoints, targetValue = visibleReferencePoints)

    #only add the upper and/or lower bounds are not also visible
    df.reference <- rbind(df.bounds[!(df.bounds$target %in% df.reference$target), ], df.reference)
	} else {
    #if there are no visible reference points, then the upper and lower bounds are the reference points.
    df.reference <- df.bounds
  }

  #make conceptual reference points available with error in identification (high > visibleReferencePoints > low)
  conceptualReferencePoints.include <- NULL
  if(!is.null(conceptualReferencePoints)) {
    includeConceptual <- sample(c(T, F),length(conceptualReferencePoints) , prob = c(pIncludeConceptualPoints, (1-pIncludeConceptualPoints)), replace = T)
    if(any(includeConceptual)) {
      #include only the points identified above
      conceptualReferencePoints.include <- conceptualReferencePoints[includeConceptual]
      ### add to df.reference
      df.reference <- unique(rbind(df.reference, data.frame(trial = 0, target = conceptualReferencePoints.include, low = conceptualReferencePoints.include, high = conceptualReferencePoints.include, estimate = conceptualReferencePoints.include, targetValue = conceptualReferencePoints.include)))
    }
	}

  ######################
  # numberSensitivity is a function of distance from visible and conceptual reference points, rather than the target value
  ### also distance from conceptualReferencePoints

    ######### invert numberSensitivity
    # Invert the numberSensitivity parameter so 1 remains "high sensitivity" (Resulting in 0 error)
    # and 0 remains "low sensitivity" (Resulting in Max error). This is to remain
    # consistent with earlier versions
  sensitivity_adj <- 1 - numberSensitivity
    #########

  if(!is.null(visibleReferencePoints)) {
    references <- c(visibleReferencePoints, conceptualReferencePoints.include)
    refDist <- sapply(targets, function(x) min(abs(references- x)))
  } else {
    refDist <- targets
  }
  df.data <- data.frame(trial = seq(1, numTargets, 1), target = targets, refDist = refDist, low = ifelse(refDist > 0, targets - (sensitivity_adj * refDist), targets + (sensitivity_adj * refDist)), high = ifelse(refDist > 0, targets + (sensitivity_adj * refDist), targets - (sensitivity_adj * refDist)),  estimate = NA, targetValue = targets)
  df.data$refDist <- NULL
  ######################

	df.data <- rbind(df.data,df.reference)

	for(i in 1:numTargets) {
    df.data[df.data$trial == i, ] <- getEstimateNL(df.data, i, upperBound = upperBound, lowerBound = lowerBound, trialCol = "trial", targetCol = "target", estimateCol = "estimate", valueCol = "targetValue", lowCol = "low", highCol = "high", memoryLength = memoryLength, rangeLength = rangeLength, numberSensitivity = numberSensitivity, accuracyPercent = accuracyPercent, firstEstimate = firstEstimate, verbose = verbose)
	}

	df.out <- df.data[df.data$trial > 0,c("target", "estimate")]
	names(df.out) <- c("target", "fEst")

  return(df.out)
}

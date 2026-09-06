#' This function simulates the numberLine process and return the predited estimates.
#'
#' Function that simulates the numberLine process and return the predited estimates.
#' The participant places each target ordinally in relation to the previous estimates and the
#' reference points. A target that is not bracketed by a remembered estimate is placed in the
#' region between its two reference points using firstEstimate; every other target is placed
#' midway between its two anchors. This is moderated by the memoryLength, which specifies how
#' many of the previous estimates are remembered (larger is better), and by numberSensitivity,
#' which specifies how finely numbers near a reference point are discriminated. Finally,
#' accuracyPercent specifies the influence of accuracy on the estimates.  This is a proportion,
#' between 0 and 1, whereby estimate is calculated as follows:
#' estimate = (estimate based on algroithm * 1-accuracyPercent) + presented target * accuracyPercent
#'
#' @param targets A vector of numbers that are the to-be-estimated values.
#' @param upperBound A number that identifes the upper point beyond which the partcipant cannot respond. In the bounded task this is the upper end of the number line and is also passed as a visible reference point. In the universal task it is the upper screen edge and is normally not a visible reference point. This must be specified in "number-line units."
#' @param lowerBound A number that identifes the lower point beyond which the partcipant cannot respond. In the bounded task this is the lower end of the number line and is also passed as a visible reference point. In the universal task it is the lower screen edge and is normally not a visible reference point.  DEFAULT = 0.
#' @param firstEstimate A proportion between 0 and 1 that identifies the spatial bias applied whenever a target is not bracketed by a remembered estimate, that is, whenever both of its anchors are reference points. 0 places the estimate at the bottom of the region between the two anchors, 1 at the top. In the bounded task this is the first trial only; in the universal task it recurs for the first target in each region between labelled points, and again in any region whose estimates have dropped out of the memory window. A conceptual reference point counts as a reference point, so whether firstEstimate applies on a given trial depends on which conceptual points were drawn for that run.  If NULL, the midpoint of the region is used.  DEFAULT = NULL
#' @param memoryLength An integer that specifies the number of previous estimates that are remembered. On trial t the remembered set is the estimates from trials t-memoryLength through t-1, plus every reference point. The larger the number, the more accurate the estimates are going to be (given the constraints of the process). DEFAULT = length(targets).
#' @param accuracyPercent A proportion between 0 and 1 that specifies the weight given to accurate responding: 0 = no weight, 1 = perfect responding.  DEFAULT = 0
#' @param numberSensitivity A proportion between 0 and 1 that specifies the range of equivelance. A number's range is scaled by its distance to the nearest reference point, so numbers near a landmark are discriminated finely and numbers far from one coarsely. When numberSensitivity=1, every number is discriminated from every other. When numberSensitivity=0, a number is not discriminated from anything within its distance to the nearest reference point. Two numbers are treated as the same when either one falls in the other's range; a target is compared to a reference point in one direction only, so labels stay perfectly discriminable. DEFAULT = 1 (perfect precision)
#' @param pIncludeConceptualPoints A proportion between 0 and 1 that specifies the probability that a conceptualReferencePoint is available on a run. Each conceptual point is drawn independently, once per run, and is then present on every trial of that run. Because the drawn points enter the reference set, this probability moves the estimates through two channels: which anchors are available and how wide each target's range of equivalence is.  DEFAULT = 0
#' @param visibleReferencePoints A vector of numbers that specify the visible (displayed) points that identify the value of positions on the number line. In the bounded task these are the upper and lower bound; in the universal task they are the labelled values and the bounds are the screen edges. This is required; there is no default.
#' @param conceptualReferencePoints A vector of numbers that specify any conceptual points that the participant may use to identify the value of positions on the number line. These may be the middle of the bounded number-line, or a learned position on the number_line. Currently, conceptualReferencePoints are only modeled if they are used thoughout the task (not introduced part of the way through) Default is NULL.
#' @param targetOrder A string specifying whether to keep the order in targets fixed ("fixed"), to ranndomize the order for every iteration of the loop ("random"), or to randomize it once and then use that order for all the loops ("single").  Default is "random".
#' @param verbose A boolean that specifies whether to print intermediate steps. This is used for debugging.  Default is FALSE.
#''
#' @return a dataframe containing the target value (target) and predicted estimate (fEst).
#' @keywords ordinalNumberline number line
#' @export
#' @examples
#' ordinalNumberLine(c(2, 3, 4, 5, 6), upperBound = 10, lowerBound = 0,
#'                   visibleReferencePoints = c(0, 10), firstEstimate = 0.5,
#'                   accuracyPercent = 0.5, targetOrder = "fixed")

ordinalNumberLine <- function(targets, upperBound, lowerBound = 0, firstEstimate = NULL, memoryLength = NULL, accuracyPercent = 0, numberSensitivity = 1, pIncludeConceptualPoints = 0, visibleReferencePoints = NULL, conceptualReferencePoints = NULL, targetOrder = "random", verbose = FALSE) {

  #the reference points define the regions of the line and set the scale of the
  #equivalence ranges, so there is nothing to simulate without them.
  if(is.null(visibleReferencePoints)) {
    stop("ordinalNumberLine: visibleReferencePoints is required. Pass the bounds for a bounded number line, or the labelled values for a universal number line.")
  }

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
  df.reference <- data.frame(trial = 0, target = visibleReferencePoints, low = visibleReferencePoints, high = visibleReferencePoints, estimate = visibleReferencePoints, targetValue = visibleReferencePoints)

  #only add the upper and/or lower bounds are not also visible
  df.reference <- rbind(df.bounds[!(df.bounds$target %in% df.reference$target), ], df.reference)

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

    ######### invert numberSensitivity
    # Invert the numberSensitivity parameter so 1 remains "high sensitivity" (Resulting in 0 error)
    # and 0 remains "low sensitivity" (Resulting in Max error).
  sensitivity_adj <- 1 - numberSensitivity
    #########

  #distance to the nearest reference point sets the half-width of each target's range of
  #equivalent numbers, so the range is symmetric around the target.
  references <- c(visibleReferencePoints, conceptualReferencePoints.include)
  refDist <- sapply(targets, function(x) min(abs(references- x)))

  df.data <- data.frame(trial = seq(1, numTargets, 1), target = targets, low = targets - (sensitivity_adj * refDist), high = targets + (sensitivity_adj * refDist),  estimate = NA, targetValue = targets)
  ######################

	df.data <- rbind(df.data,df.reference)

  df.data <- getEstimateNL(df.data, numTargets, upperBound = upperBound, lowerBound = lowerBound, trialCol = "trial", targetCol = "target", estimateCol = "estimate", valueCol = "targetValue", lowCol = "low", highCol = "high", memoryLength = memoryLength, numberSensitivity = numberSensitivity, accuracyPercent = accuracyPercent, firstEstimate = firstEstimate, verbose = verbose)

	df.out <- df.data[df.data$trial > 0,c("target", "estimate")]
	names(df.out) <- c("target", "fEst")

  return(df.out)
}

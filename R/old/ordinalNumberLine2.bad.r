ordinalNumberLine2.bad <- function(targets, upperBound, lowerBound = 0, firstEstimate = NULL, memoryLength = NULL, rangeLength = 2, accuracyPercent = 0, numberSensitivity = 1, referencePoints = NULL) {

  numTargets <- length(targets)
  if (is.null(memoryLength)) {
    memoryLength <- numTargets
  }

  # Pre-allocate df.data
  df.data <- data.frame(target = targets, fEst = rep(NA, numTargets))

  # Pre-sort previous targets if reference points are provided
  previousTargets <- NULL
  if (!is.null(referencePoints)) {
    trial <- Inf
    previousTargets <- data.frame(target = referencePoints, estimate = referencePoints, low = referencePoints, high = referencePoints, trial = trial)
  }

  trial <- 1
  for (i in seq_along(targets)) {
		previousTargets <- arrange(previousTargets, target)
    numPresentedTargets <- nrow(previousTargets) - length(referencePoints)

    ts <- targets[i]

    # Handle reference points and initial estimate
    if (ts %in% referencePoints) {
      estimate <- previousTargets[previousTargets$target == ts, "estimate"]
    } else if (trial == 1) {
      estimate <- ifelse(is.null(firstEstimate), runif(1, upperBound, lowerBound), firstEstimate)
    } else {
      # Find the closest previous target
      adjacentIdx <- which.min(abs(previousTargets$target - ts))
      minAdjIdx <- ifelse(previousTargets$target[adjacentIdx] > ts, adjacentIdx - 1, adjacentIdx)
      minAdjIdx <- max(1, minAdjIdx)
      clMaxAdjIdx <- min(nrow(previousTargets), minAdjIdx + 1)

      # Determine the estimate based on the closest targets
      if ((previousTargets$high[minAdjIdx] > ts) & (previousTargets$low[minAdjIdx] < ts)) {
        estimate <- previousTargets$estimate[minAdjIdx]
      } else if ((previousTargets$high[clMaxAdjIdx] > ts) & (previousTargets$low[clMaxAdjIdx] < ts)) {
        estimate <- previousTargets$estimate[clMaxAdjIdx]
      } else {
        clMinIdx <- max(1, minAdjIdx - rangeLength + 1)
        clMaxIdx <- min(nrow(previousTargets), clMaxAdjIdx + (rangeLength - 1))
        estimate <- ifelse(previousTargets$low[clMinIdx] > ts, mean(c(lowerBound, previousTargets$estimate[clMinIdx])),
                           ifelse(previousTargets$high[clMaxIdx] < ts, mean(c(previousTargets$estimate[clMaxIdx], upperBound)),
                                  mean(c(previousTargets$estimate[clMinIdx], previousTargets$estimate[clMaxIdx]))))
      }
    }

    estimate <- estimate * (1 - accuracyPercent) + ts * accuracyPercent

    # Manage previousTargets efficiently
    if (numPresentedTargets < memoryLength) {
      previousTargets <- rbind(previousTargets, data.frame(target = ts, estimate = estimate, low = (numberSensitivity * ts), high = ((2 - numberSensitivity) * ts), trial = trial))
    } else {
      previousTargets <- rbind(previousTargets[-1,], data.frame(target = ts, estimate = estimate, low = (numberSensitivity * ts), high = ((2 - numberSensitivity) * ts), trial = trial))
    }

    df.data$fEst[i] <- estimate
    trial <- trial + 1
  }

  return(df.data)
}
#' Runs the ordinalNumberLine simulation in parallel with flexible parameters.
#'
#' This function executes the ordinalNumberLine simulation multiple times in parallel
#' using the specified parameters to generate estimates for a set of target values.
#' Parallel processing can significantly reduce computation time for a large number of loops.
#'
#' @param targets A vector of numbers representing the values to be estimated.
#' @param parList A list containing parameter names and their values for the
#'   `ordinalNumberLine` analysis. Required elements are: `firstEstimate`,
#'   `upperBound`, `lowerBound`, `rangeLength`, `memoryLength`,
#'   `numberSensitivity`, `accuracyPercent`, `pIncludeConceptualPoints`,
#'   `visibleReferencePoints`, `conceptualReferencePoints`, and `targetOrder`.
#' @param loops An integer specifying the number of simulation runs to perform.
#'   Each run can be executed in parallel, potentially speeding up the overall
#'   estimation process. Higher values generally lead to more stable and precise
#'   estimates but increase computation time. Default = 1000.
#' @param verbose A logical value indicating whether to print intermediate steps
#'   for debugging purposes. Default = FALSE.
#' @param numCores An integer specifying the number of CPU cores to use for parallel
#'   execution. If `NULL` (default), it will use all available cores minus one.
#'
#' @return A dataframe containing the target value (`target`) and the mean of the
#'   predicted estimates (`fEst`) across all parallel simulation runs.
#'
#' @keywords ordinalNumberline parallel ordinal number-line
#' @export
#' @importFrom foreach %dopar%
#' @examples
#' \dontrun{
#'   # Example with 2 cores
#'   my_targets <- c(2, 3, 4, 5, 6)
#'   my_parList <- list(
#'     firstEstimate = 400,
#'     upperBound = 100,
#'     lowerBound = 0,
#'     rangeLength = 5,
#'     memoryLength = 3,
#'     accuracyPercent = 0.5,
#'     numberSensitivity = 1,
#'     pIncludeConceptualPoints = 0.2,
#'     visibleReferencePoints = c(0, 50, 100),
#'     conceptualReferencePoints = c(25, 75),
#'     targetOrder = "random"
#'   )
#'   parallel_results <- ordinalNumberLineFlex_mc(
#'     targets = my_targets,
#'     parList = my_parList,
#'     loops = 100,
#'     verbose = FALSE,
#'     numCores = 2
#'   )
#'   print(parallel_results)
#'
#'   # Example using default number of cores
#'   parallel_results_default_cores <- ordinalNumberLineFlex_mc(
#'     targets = my_targets,
#'     parList = my_parList,
#'     loops = 100,
#'     verbose = FALSE
#'   )
#'   print(parallel_results_default_cores)
#' }

ordinalNumberLineFlex_mc <- function (targets, parList, loops = 1000, verbose = FALSE, numCores = NULL) {

	#numTargets <- length(targets)
	df.data <- NULL

	# Determine the number of cores to use
	if(is.null(numCores)) {
		numCores <- parallel::detectCores() - 1 # Use all but one core by default
	}
	cl <- parallel::makeCluster(numCores, outfile='log.txt')
	doParallel::registerDoParallel(cl)
#	doParallel::registerDoParallel(cores = numCores)

	# Handle single or fixed target order outside the loop
	if(parList[["targetOrder"]] == "single")	{
		targetSeq <- sample(targets)
		loops <- 1
	} else {
		targetSeq <- targets
	}
	if(parList[["targetOrder"]] == "fixed") {
		loops <- 1
	}


	results <- foreach::foreach(lps = 1:loops, .combine = chutils::ch.rbind, .packages = c("dplyr", "chutils")) %dopar% {
		# if(parList[["targetOrder"]] == "random") {
		# 	current_targets <- sample(targets)
		# } else {
		# 	current_targets <- targetSeq # Use the pre-determined sequence
		# }

		# Assuming ordinalNumberLine is in the same package or its dependencies
		ordinalNumberLine(targets = targetSeq,
						  firstEstimate = parList[["firstEstimate"]],
						  upperBound = parList[["upperBound"]],
						  lowerBound = parList[["lowerBound"]],
						  rangeLength = parList[["rangeLength"]],
						  memoryLength = parList[["memoryLength"]],
						  accuracyPercent = parList[["accuracyPercent"]],
						  numberSensitivity = parList[["numberSensitivity"]],
						  pIncludeConceptualPoints = parList[["pIncludeConceptualPoints"]],
						  visibleReferencePoints = parList[["visibleReferencePoints"]],
						  conceptualReferencePoints = parList[["conceptualReferencePoints"]],
							targetOrder = parList[["targetOrder"]],
						  verbose = verbose)
	}

	parallel::stopCluster(cl)
	rm(cl)

	df.sum <- results %>% dplyr::group_by (target) %>% dplyr::summarize (fEst = mean(fEst, na.rm = T))
	return(df.sum)

}

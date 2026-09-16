# the parameters a search can vary, with the domain each one lives in. A domain is
# a property of the model, not of a run: a proportion cannot leave [0, 1] and a
# memory window cannot be shorter than one estimate or longer than the set of
# trials it is drawn from. A user-supplied range may narrow a domain; it may not
# widen it, so a range outside the domain is an error rather than a clipped range
# the caller never hears about.
nlFitDomains <- function(nTargets) {
	proportion <- function() list(lower = 0, upper = 1, step = 0.001, integer = FALSE,
								  description = "a proportion in [0, 1]")
	list(firstEstimate = proportion(),
		 memoryLength = list(lower = 1, upper = nTargets, step = 1, integer = TRUE,
							 description = paste0("an integer in [1, ", nTargets, "]")),
		 accuracyPercent = proportion(),
		 numberSensitivity = proportion(),
		 pConceptual = proportion())
}


# the order the parameters are reported in. It is the order the package documents
# them in, so a fit reads the same way as the argument list.
nlFitParameterOrder <- function() {
	c("firstEstimate", "memoryLength", "accuracyPercent", "numberSensitivity", "pConceptual")
}


# the search settings behind search = "full" and search = "quick". "full" is the
# effort a reported fit is run at; "quick" is small enough for an example or a
# check that the call is wired up correctly, and its parameter estimates are
# coarse. loops is the number of simulation runs the objective averages over;
# finalLoops is the number the fit report is recomputed at.
nlFitPreset <- function(search) {
	switch(search,
		   full = list(numLoops = 250, numIntervals = 200, optParamListN = 20,
					   optBoundLoops = 20, loops = 500, finalLoops = 500,
					   multicore = TRUE, multicorePackages = "nlFit"),
		   quick = list(numLoops = 40, numIntervals = 25, optParamListN = 6,
						optBoundLoops = 3, loops = 60, finalLoops = 200,
						multicore = TRUE, multicorePackages = "nlFit")
	)
}


# the settings control may name: everything smartGridSearch() takes except the
# arguments nlFit() computes itself, plus the two loop counts. A name outside the
# set is a mistake the caller wants to hear about, not a setting to drop.
nlFitControlNames <- function() {
	searchArgs <- names(formals(smartGridSearch2::smartGridSearch))
	searchArgs <- setdiff(searchArgs, c("fn", "parsUpper", "parsLower", "parsMinInt",
										"otherParamList", "..."))
	c(searchArgs, "loops", "finalLoops")
}


# one range from free, checked against its parameter's domain. A range is
# c(lower, upper) or c(lower, upper, step); NULL means the whole domain at the
# default step. Every failure names the parameter and the domain it has to sit in.
nlFitResolveRange <- function(parameter, range, domain) {
	if(is.null(range)) {
		return(list(lower = domain$lower, upper = domain$upper, step = domain$step))
	}
	if(!is.numeric(range) || length(range) < 2 || length(range) > 3 || any(!is.finite(range))) {
		stop("nlFit: the range for '", parameter,
			 "' must be a numeric c(lower, upper) or c(lower, upper, step) of finite values.")
	}
	lower <- range[1]
	upper <- range[2]
	if(lower >= upper) {
		stop("nlFit: the range for '", parameter, "' has lower ", lower, " and upper ", upper,
			 ", so there is nothing to search. A parameter held at one value is fixed: give it through its own argument and leave it out of free.")
	}
	if(lower < domain$lower || upper > domain$upper) {
		stop("nlFit: the range c(", lower, ", ", upper, ") for '", parameter,
			 "' is outside its domain. ", parameter, " must be ", domain$description,
			 ". A range may narrow the domain; it may not widen it.")
	}
	if(domain$integer) {
		if(any(range != round(range))) {
			stop("nlFit: the bounds for '", parameter, "' must be whole numbers. ",
				 parameter, " must be ", domain$description, ".")
		}
		if(length(range) == 3) {
			stop("nlFit: the step for '", parameter,
				 "' is fixed at 1 and cannot be set. Give the range as c(lower, upper).")
		}
		return(list(lower = lower, upper = upper, step = domain$step))
	}
	step <- if(length(range) == 3) range[3] else domain$step
	if(step <= 0 || step > (upper - lower)) {
		stop("nlFit: the step for '", parameter, "' must be greater than 0 and no wider than the range, but it is ", step, ".")
	}
	list(lower = lower, upper = upper, step = step)
}


# free, in either of its two forms, turned into the three bound lists the search
# takes. A character vector searches each named parameter over its whole domain; a
# named list searches each named parameter over the range given. Presence in free
# is what makes a parameter free, so a name that is not a parameter of the model,
# or a name given twice, is an error.
nlFitResolveFree <- function(free, nTargets) {
	domains <- nlFitDomains(nTargets)

	if(is.null(free) || length(free) == 0) {
		stop("nlFit: free names no parameters, so there is nothing to fit. To score a set of parameters you already have, use outputNLfitStats().")
	}
	if(is.character(free)) {
		ranges <- vector("list", length(free))
		names(ranges) <- free
	} else if(is.list(free)) {
		ranges <- free
	} else {
		stop("nlFit: free must be a character vector of parameter names or a named list of ranges, not ", class(free)[1], ".")
	}

	parameters <- names(ranges)
	if(is.null(parameters) || any(is.na(parameters)) || any(!nzchar(parameters))) {
		stop("nlFit: every element of free must be named with a parameter name. The parameters that can be searched are: ",
			 paste(names(domains), collapse = ", "), ".")
	}
	if(anyDuplicated(parameters)) {
		stop("nlFit: free names '", parameters[anyDuplicated(parameters)], "' more than once.")
	}
	unknown <- setdiff(parameters, names(domains))
	if(length(unknown) > 0) {
		stop("nlFit: free names ", paste0("'", unknown, "'", collapse = ", "),
			 ", which the model has no parameter for. The parameters that can be searched are: ",
			 paste(names(domains), collapse = ", "), ".")
	}

	resolved <- lapply(parameters, function(p) nlFitResolveRange(p, ranges[[p]], domains[[p]]))
	names(resolved) <- parameters
	resolved
}


# the message a fixed value that is out of its domain earns. The package's own
# validator answers TRUE or FALSE, which is what a search needs; a caller who has
# just typed a value needs to be told which one it was.
nlFitStopOnBadFixed <- function(parList, nTargets) {
	domains <- nlFitDomains(nTargets)
	bad <- character(0)
	for(parameter in names(domains)) {
		value <- parList[[parameter]]
		if(is.null(value)) next
		domain <- domains[[parameter]]
		outside <- !is.numeric(value) || length(value) != 1 || !is.finite(value) ||
			value < domain$lower || value > domain$upper ||
			(domain$integer && value != round(value))
		if(outside) bad <- c(bad, paste0("'", parameter, "' must be ", domain$description))
	}
	if(length(bad) > 0) {
		stop("nlFit: a fixed parameter value is outside its domain. ", paste(bad, collapse = "; "), ".")
	}
	stop("nlFit: the fixed parameter values are not a valid parameter set. Check upperBound, lowerBound and targetOrder.")
}


#' Fit the ordinal number-line model to one participant's data.
#'
#' Fits the ordinal number-line model to a participant's estimates in one call. It
#' searches the parameters named in `free`, holds every other parameter at the
#' value given through its own argument, and returns the fitted parameters, the
#' fit statistics and the fitted values. The three steps it wraps remain available
#' on their own: `getOrdinalNumberlineFit()` is the objective the search minimizes,
#' `smartGridSearch()` is the search, and `outputNLfitStats()` recomputes the fit
#' at the best parameters and writes the report.
#'
#' The data are fitted as they are given. One row per target is an averaged fit;
#' one row per trial with a presentation column is a trial-level fit. A trial-level
#' fit has more rows per participant at the same number of free parameters, so its
#' BIC and AIC are on a different scale and are not comparable with those of an
#' averaged fit. Fit one participant per call.
#'
#' The search is in the smartGridSearch2 package, which is not on CRAN. `nlFit()`
#' stops with the install line when it is not available.
#'
#' @param data A dataframe that must contain the following columns: target; estimate. It may hold one row per target, the usual case, or one row per trial with a presentation column, in which case the fit is trial level.
#' @param upperBound A number that identifes the upper point beyond which the partcipant cannot respond. In the bounded task this is the upper end of the number line and is also passed as a visible reference point. In the universal task it is the upper screen edge. This is required; there is no default.
#' @param lowerBound A number that identifes the lower point beyond which the partcipant cannot respond. In the bounded task this is the lower end of the number line and is also passed as a visible reference point. In the universal task it is the lower screen edge. DEFAULT = 0.
#' @param visibleReferencePoints A vector of numbers that specify the visible (displayed) points that identify the value of positions on the number line. In the bounded task these are the upper and lower bound; in the universal task they are the labelled values and the bounds are the screen edges. This is required; there is no default.
#' @param conceptualReferencePoints A vector of numbers that specify any conceptual points that the participant may use to identify the value of positions on the number line. Default is NULL.
#' @param free The parameters the search varies. Either a character vector of parameter names, each searched over its whole domain at the default step, or a named list whose elements are `c(lower, upper)` or `c(lower, upper, step)`, each searched over the range given. Presence in `free` is what makes a parameter free; a parameter that is not named is held fixed at its own argument. A range may narrow a parameter's domain but may not widen it, and a range outside the domain is an error. The parameters that can be searched are firstEstimate, memoryLength, accuracyPercent, numberSensitivity and pConceptual. The default ranges are 0 to 1 in steps of 0.001 for the proportions, and 1 to the number of rows of data in steps of 1 for memoryLength, whose step cannot be changed. DEFAULT = c("firstEstimate", "accuracyPercent", "numberSensitivity").
#' @param firstEstimate A proportion between 0 and 1 that identifies the spatial bias applied whenever a target is not bracketed by a remembered estimate, that is, whenever both of its anchors are reference points. If NULL, the midpoint of the region is used. Ignored, with a warning, when firstEstimate is named in `free`. DEFAULT = NULL
#' @param accuracyPercent A proportion between 0 and 1 that specifies the weight given to accurate responding: 0 = no weight, 1 = perfect responding. Ignored, with a warning, when accuracyPercent is named in `free`. DEFAULT = 0
#' @param numberSensitivity A proportion between 0 and 1 that specifies the range of equivelance. When numberSensitivity=1, every number is discriminated from every other. Ignored, with a warning, when numberSensitivity is named in `free`. DEFAULT = 1
#' @param memoryLength An integer that specifies the number of previous estimates that are remembered. NULL is the number of rows of data, which is the package's default. Ignored, with a warning, when memoryLength is named in `free`. DEFAULT = NULL
#' @param pConceptual A proportion between 0 and 1 that specifies the probability that a conceptualReferencePoint is available on a run. Ignored, with a warning, when pConceptual is named in `free`. DEFAULT = 0
#' @param search A string naming the effort the search is run at: "full", the effort a reported fit is run at, or "quick", which is small enough for an example and gives coarse estimates. DEFAULT = "full".
#' @param control A named list overriding single search settings. It may name `loops` (the simulation runs the objective averages over), `finalLoops` (the runs the fit report is recomputed at), and any argument of `smartGridSearch()` other than the ones this function builds. An unrecognised name is an error. DEFAULT = list()
#' @param targetOrder A string specifying whether to keep the order in targets fixed ("fixed"), to randomize the order for every iteration of the loop ("random"), or to randomize it once and then use that order for all the loops ("single"). Default is "random".
#' @param minimizeStat A string that specifies which statistic to minimize when optimizing the model fit. The options are: "BIC", "AIC", or "R_Square". Default is "BIC".
#' @param dataTargetCol A string that identifies the name of the column in data that contains the target values. The default is "target"
#' @param dataEstimateCol A string that identifies the name of the column in data that contains the participant's estimate values. The default is "estimate"
#' @param dataPresentationCol A string that identifies the name of the column in data that numbers the occurrences of a repeated target value, counted in the order they were presented. When data has that column the fit is matched to the simulation trial by trial. The default is "presentation".
#' @param sinkFilename A string that identifies the name of file (.txt) in which the fit statistics will be saved. The default is NULL, whereby the statistics are not written anywhere.
#' @param plotFileName A string that identifies the name of file (.pdf) in which the data plot will be saved. The default is NULL, whereby no plot is drawn and no graphics device is opened. Use plot() on the result to draw it.
#' @param verbose A boolean that specifies whether to print intermediate steps. This is used for debugging. Default is FALSE.
#''
#' @return An object of class "nlFit": a list with `parameters` (a dataframe with one row per model parameter giving its value, whether it was free, and the range it was searched over), `fitStats` (r2, BIC, AIC, the number of free parameters, the number of rows fitted, and the statistic minimized), `fitted` (the data with the fitted estimate attached as fEst), `search` (the raw search result), `line` (the number-line settings), `columns` (the column names used), `call` and `version`. The function writes nothing unless it is given a file name.
#' @keywords ordinalNumberline fit number-line
#' @export
#' @importFrom utils packageVersion modifyList
#' @examples
#' \donttest{
#' df <- data.frame(target = c(3, 8, 15, 22, 30, 45, 60, 80),
#'                  estimate = c(9, 14, 21, 26, 33, 47, 63, 82))
#' if(requireNamespace("smartGridSearch2", quietly = TRUE)) {
#'   fit <- nlFit(df, upperBound = 100, lowerBound = 0,
#'                visibleReferencePoints = c(0, 100),
#'                free = c("firstEstimate", "accuracyPercent"),
#'                search = "quick",
#'                control = list(multicore = FALSE, numLoops = 8, numIntervals = 5,
#'                               optParamListN = 3, optBoundLoops = 1,
#'                               loops = 5, finalLoops = 5))
#'   print(fit)
#'   as.data.frame(fit)
#' }
#' }

nlFit <- function(data, upperBound, lowerBound = 0,
				  visibleReferencePoints, conceptualReferencePoints = NULL,
				  free = c("firstEstimate", "accuracyPercent", "numberSensitivity"),
				  firstEstimate = NULL, accuracyPercent = 0, numberSensitivity = 1,
				  memoryLength = NULL, pConceptual = 0,
				  search = c("full", "quick"), control = list(),
				  targetOrder = "random", minimizeStat = "BIC",
				  dataTargetCol = "target", dataEstimateCol = "estimate",
				  dataPresentationCol = "presentation",
				  sinkFilename = NULL, plotFileName = NULL, verbose = FALSE) {

	thisCall <- match.call()

	#the search lives in a package that is not on CRAN, so say how to get it rather
	#than failing on a missing function part way through.
	if(!requireNamespace("smartGridSearch2", quietly = TRUE)) {
		stop("nlFit: the search needs the smartGridSearch2 package, which is not on CRAN. Install it with:\n",
			 "    remotes::install_github(\"ccpluncw/ccpl_R_smartGridSearch2\")")
	}

	#the bounds and the reference points describe the line the participant responded
	#on. Nothing about the fit is defined without them.
	if(missing(upperBound) || is.null(upperBound)) {
		stop("nlFit: upperBound is required. Pass the upper end of the number line, or the upper screen edge for a universal number line.")
	}
	if(missing(visibleReferencePoints) || is.null(visibleReferencePoints)) {
		stop("nlFit: visibleReferencePoints is required. Pass the bounds for a bounded number line, or the labelled values for a universal number line.")
	}

	nlCheckFitData(data, dataTargetCol, dataEstimateCol)
	search <- match.arg(search)
	nTargets <- nrow(data)

	#which parameters the search varies, and over what
	resolved <- nlFitResolveFree(free, nTargets)
	freeNames <- names(resolved)
	pars.n <- length(freeNames)

	#a value given for a parameter the search is going to vary cannot be honoured
	#and cannot be silently kept either, because the caller would read the returned
	#value as the one they set.
	supplied <- c(firstEstimate = !missing(firstEstimate), memoryLength = !missing(memoryLength),
				  accuracyPercent = !missing(accuracyPercent),
				  numberSensitivity = !missing(numberSensitivity),
				  pConceptual = !missing(pConceptual))
	conflicted <- intersect(freeNames, names(supplied)[supplied])
	if(length(conflicted) > 0) {
		warning("nlFit: ", paste0("'", conflicted, "'", collapse = ", "),
				" is named in free and also given a value. The value is ignored and the parameter is searched.",
				call. = FALSE)
	}

	#the fixed values, with the defaults the package documents. memoryLength's
	#default is the number of rows fitted, so it is resolved here and reported as
	#the number it is; firstEstimate's default is the midpoint of the region, which
	#is a placement rule rather than a number, so it stays NULL.
	fixed <- list(firstEstimate = firstEstimate, memoryLength = memoryLength,
				  accuracyPercent = accuracyPercent, numberSensitivity = numberSensitivity,
				  pConceptual = pConceptual)
	fixed[freeNames] <- rep(list(NULL), length(freeNames))
	if(!("memoryLength" %in% freeNames) && is.null(fixed$memoryLength)) {
		fixed$memoryLength <- nTargets
	}

	#the fixed values are checked once, before a search that can take hours starts.
	fixedParList <- c(list(upperBound = upperBound, lowerBound = lowerBound,
						   targetOrder = targetOrder),
					  fixed[!vapply(fixed, is.null, logical(1))])
	if(!validateNumberlineParameters(fixedParList)) nlFitStopOnBadFixed(fixedParList, nTargets)

	#the search settings: a preset, then whatever control names
	settings <- nlFitPreset(search)
	if(length(control) > 0) {
		if(is.null(names(control)) || any(!nzchar(names(control)))) {
			stop("nlFit: every element of control must be named.")
		}
		unknown <- setdiff(names(control), nlFitControlNames())
		if(length(unknown) > 0) {
			stop("nlFit: control names ", paste0("'", unknown, "'", collapse = ", "),
				 ", which is not a search setting. control may name: ",
				 paste(sort(nlFitControlNames()), collapse = ", "), ".")
		}
		settings <- utils::modifyList(settings, control)
	}
	loops <- settings$loops
	finalLoops <- settings$finalLoops
	settings$loops <- NULL
	settings$finalLoops <- NULL

	otherParamList <- c(list(data = data, upperBound = upperBound, lowerBound = lowerBound,
							 visibleReferencePoints = visibleReferencePoints,
							 conceptualReferencePoints = conceptualReferencePoints,
							 loops = loops, targetOrder = targetOrder,
							 minimizeStat = minimizeStat, pars.n = pars.n,
							 dataTargetCol = dataTargetCol, dataEstimateCol = dataEstimateCol,
							 dataPresentationCol = dataPresentationCol, verbose = verbose),
					   fixed[!vapply(fixed, is.null, logical(1))])

	searchResult <- do.call(smartGridSearch2::smartGridSearch,
							c(list(fn = getOrdinalNumberlineFit,
								   parsUpper = lapply(resolved, function(r) r$upper),
								   parsLower = lapply(resolved, function(r) r$lower),
								   parsMinInt = lapply(resolved, function(r) r$step),
								   otherParamList = otherParamList),
							  settings))

	#the fitted values of the searched parameters. memoryLength counts estimates, so
	#it is reported and used as a whole number whatever the search returned.
	fittedPars <- fixed
	for(parameter in freeNames) {
		value <- searchResult$final[[parameter]]
		if(parameter == "memoryLength") value <- max(1, min(nTargets, round(value)))
		fittedPars[[parameter]] <- value
	}

	statList <- c(list(upperBound = upperBound, lowerBound = lowerBound,
					   visibleReferencePoints = visibleReferencePoints,
					   conceptualReferencePoints = conceptualReferencePoints,
					   targetOrder = targetOrder),
				  fittedPars)

	out <- outputNLfitStats(data, statList, dataTargetCol = dataTargetCol,
							dataEstimateCol = dataEstimateCol, pars.n = pars.n,
							loops = finalLoops, sinkFilename = sinkFilename,
							plotFileName = plotFileName,
							dataPresentationCol = dataPresentationCol)

	parameterOrder <- nlFitParameterOrder()
	parameters <- data.frame(
		parameter = parameterOrder,
		value = vapply(parameterOrder, function(p) {
			value <- fittedPars[[p]]
			if(is.null(value)) NA_real_ else as.numeric(value)
		}, numeric(1)),
		free = parameterOrder %in% freeNames,
		lower = vapply(parameterOrder, function(p) {
			if(p %in% freeNames) as.numeric(resolved[[p]]$lower) else NA_real_
		}, numeric(1)),
		upper = vapply(parameterOrder, function(p) {
			if(p %in% freeNames) as.numeric(resolved[[p]]$upper) else NA_real_
		}, numeric(1)),
		row.names = NULL, stringsAsFactors = FALSE)

	fitStats <- list(r2 = out$runStats$fitStats$r2, BIC = out$runStats$fitStats$BIC,
					 AIC = out$runStats$fitStats$AIC, pars.n = pars.n,
					 n = nrow(out$df.fitted), minimizeStat = minimizeStat)

	structure(list(parameters = parameters,
				   fitStats = fitStats,
				   fitted = out$df.fitted,
				   search = searchResult,
				   line = list(upperBound = upperBound, lowerBound = lowerBound,
							   visibleReferencePoints = visibleReferencePoints,
							   conceptualReferencePoints = conceptualReferencePoints,
							   targetOrder = targetOrder),
				   columns = list(target = dataTargetCol, estimate = dataEstimateCol,
								  presentation = dataPresentationCol,
								  trialLevel = dataPresentationCol %in% names(data)),
				   call = thisCall,
				   version = as.character(packageVersion("nlFit"))),
			  class = "nlFit")
}


#' Print a fitted ordinal number-line model.
#'
#' Prints the version the fit was made under, how many parameters were searched
#' and how many rows were fitted, then one line per model parameter and one line
#' of fit statistics. A parameter that was searched shows the range it was
#' searched over; a parameter whose value is the midpoint placement rule rather
#' than a number prints as NULL.
#'
#' @param x An object of class "nlFit".
#' @param ... Further arguments, ignored.
#''
#' @return `x`, invisibly.
#' @keywords ordinalNumberline fit print
#' @method print nlFit
#' @export

print.nlFit <- function(x, ...) {
	rowLabel <- if(isTRUE(x$columns$trialLevel)) "trials" else "targets"
	cat("nlFit ", x$version, ": ", x$fitStats$pars.n, " free parameter",
		if(x$fitStats$pars.n == 1) "" else "s", ", ", x$fitStats$n, " ", rowLabel, "\n", sep = "")

	for(i in seq_len(nrow(x$parameters))) {
		value <- x$parameters$value[i]
		shown <- if(is.na(value)) "NULL (midpoint)" else format(value)
		range <- if(x$parameters$free[i]) {
			paste0("free over [", format(x$parameters$lower[i]), ", ", format(x$parameters$upper[i]), "]")
		} else {
			"fixed"
		}
		cat("  ", formatC(x$parameters$parameter[i], width = -18), formatC(shown, width = -16),
			range, "\n", sep = "")
	}

	cat("  r2 = ", x$fitStats$r2, "   BIC = ", x$fitStats$BIC, "   AIC = ", x$fitStats$AIC,
		"   (minimized ", x$fitStats$minimizeStat, ")\n", sep = "")

	invisible(x)
}


#' Plot a fitted ordinal number-line model.
#'
#' Draws the participant's estimates against the targets, the identity line, the
#' visible reference points as faint verticals, and the fitted estimates as a
#' line. The axes span the bounds and the values plotted.
#'
#' @param x An object of class "nlFit".
#' @param ... Further arguments passed to `plot()`.
#''
#' @return `x`, invisibly.
#' @keywords ordinalNumberline fit plot
#' @method plot nlFit
#' @export
#' @importFrom graphics abline lines

plot.nlFit <- function(x, ...) {
	fitted <- x$fitted
	estimate <- fitted[[x$columns$estimate]]
	ordered <- order(fitted$target)

	xlim <- c(min(x$line$lowerBound, fitted$target), max(x$line$upperBound, fitted$target))
	ylim <- c(min(x$line$lowerBound, estimate, fitted$fEst),
			  max(x$line$upperBound, estimate, fitted$fEst))

	plot(fitted$target, estimate, xlim = xlim, ylim = ylim,
		 xlab = "target", ylab = "estimate", ...)
	abline(0, 1, col = "grey70")
	abline(v = x$line$visibleReferencePoints, col = "grey85")
	lines(fitted$target[ordered], fitted$fEst[ordered], col = "blue")

	invisible(x)
}


#' Turn a fitted ordinal number-line model into one row.
#'
#' Returns a one-row dataframe holding the five model parameters and the fit
#' statistics. Rows from several fits can be stacked with `rbind()` to give one
#' row per participant. A parameter that was left at the midpoint placement rule
#' rather than a number is NA.
#'
#' @param x An object of class "nlFit".
#' @param row.names A row name for the returned row, or NULL. Default is NULL.
#' @param optional Ignored; present for consistency with the generic. Default is FALSE.
#' @param ... Further arguments, ignored.
#''
#' @return A dataframe with one row: the five model parameters, r2, BIC, AIC and pars.n.
#' @keywords ordinalNumberline fit dataframe
#' @method as.data.frame nlFit
#' @export

as.data.frame.nlFit <- function(x, row.names = NULL, optional = FALSE, ...) {
	values <- as.list(x$parameters$value)
	names(values) <- x$parameters$parameter

	data.frame(c(values, list(r2 = x$fitStats$r2, BIC = x$fitStats$BIC,
							  AIC = x$fitStats$AIC, pars.n = x$fitStats$pars.n)),
			   row.names = row.names, stringsAsFactors = FALSE)
}

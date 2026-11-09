#numberSensitivity on one axis from -1 to 1, landmarks drawn on every trial
library(nlFit)

nsp <- asNamespace("nlFit")

lines <- list(
	bounded = list(targets = c(75, 775, 825, 225, 575, 275, 925, 875, 725, 325),
				   line = list(upperBound = 1000, lowerBound = 0, firstEstimate = 0.5,
							   memoryLength = NULL, visibleReferencePoints = c(0, 1000),
							   conceptualReferencePoints = NULL, pConceptual = 0)),
	universal = list(targets = c(625, 5875, 3375, 1875, 4875, 2625, 6375, 125, 4125, 3875),
					 line = list(upperBound = 7000, lowerBound = 0, firstEstimate = 0.5,
								 memoryLength = NULL, visibleReferencePoints = c(1500, 4562),
								 conceptualReferencePoints = NULL, pConceptual = 0)))

one <- function(seed, f, ns, ap, ...) {
	set.seed(seed)
	do.call(ordinalNumberLine, c(list(targets = f$targets, targetOrder = "fixed",
									  numberSensitivity = ns, accuracyPercent = ap),
								 modifyList(f$line, list(...))))$fEst
}

for(f in lines) {
	#at 0 accuracy acts as 0
	stopifnot(identical(one(2, f, 0, 0.6), one(2, f, 0, 0)))
	#at -1 accuracy is gone and no draw is left
	stopifnot(identical(one(3, f, -1, 0.6),
						one(4, f, -1, 0)))
	#at or below 0 a conceptual point is never used
	for(ns in c(0, -0.5)) {
		stopifnot(identical(one(4, f, ns, 0, pConceptual = 0,
								conceptualReferencePoints = 500),
							one(4, f, ns, 0, pConceptual = 1,
								conceptualReferencePoints = 500)))
	}
	#a bound stays a response limit when its label is not used
	est <- one(5, f, -0.7, 0)
	stopifnot(all(est >= f$line$lowerBound), all(est <= f$line$upperBound))
}

#with no label used the estimates differ from using every label
f <- lines$universal
stopifnot(!identical(one(3, f, -1, 0),
					 one(3, f, 0, 0)))

#a conceptual point anchors but does not set the bands
set.seed(9)
f$targets <- sample(seq(125, 6875, 250))
withPoint <- one(5, f, 0.6, 0, pConceptual = 1,
				 conceptualReferencePoints = 3000)
f$line$visibleReferencePoints <- c(1500, 3000, 4562)
inBands <- one(5, f, 0.6, 0)
stopifnot(!identical(withPoint, inBands))

#the conceptual gate
stopifnot(nsp$nlEffectiveConceptual(0, 0.4) == 0,
		  nsp$nlEffectiveConceptual(0.1, 0.4) == 0.4)

#the sim averages loops over draws and collapses them with none left
p <- modifyList(lines$bounded$line, list(numberSensitivity = -0.5, accuracyPercent = 0,
										 targetOrder = "fixed"))
set.seed(6)
a <- ordinalNumberLineSim(lines$bounded$targets, p, loops = 200)
stopifnot(all(is.finite(a$fEst)))
p$numberSensitivity <- -1
set.seed(6)
b1 <- ordinalNumberLineSim(lines$bounded$targets, p, loops = 1)
set.seed(7)
b2 <- ordinalNumberLineSim(lines$bounded$targets, p, loops = 50)
stopifnot(isTRUE(all.equal(b1$fEst, b2$fEst)))

errorFrom <- function(expr) tryCatch({ force(expr); NA_character_ },
									 error = function(e) conditionMessage(e))

#ns is valid from -1 to 1
stopifnot(validateNumberlineParameters(list(upperBound = 10, numberSensitivity = -0.5)),
		  !validateNumberlineParameters(list(upperBound = 10, numberSensitivity = -1.5)))

if(requireNamespace("smartGridSearch2", quietly = TRUE)) {
	settings <- list(multicore = FALSE, numLoops = 8, numIntervals = 5, optParamListN = 3,
					 optBoundLoops = 1, loops = 5, finalLoops = 20)
	fitLine <- function(d, upper, visible, ...) {
		kept <- NULL
		invisible(capture.output(kept <- nlFit(d, upperBound = upper,
											   visibleReferencePoints = visible,
											   targetOrder = "fixed", search = "quick",
											   control = settings, ...)))
		kept
	}

	#a label off the bounds searches ns over [-1, 1]
	d <- data.frame(target = lines$universal$targets,
					estimate = one(8, lines$universal, 0.5, 0.2))
	report <- tempfile(fileext = ".txt")
	fit <- fitLine(d, 7000, c(1500, 4562), free = c("firstEstimate", "numberSensitivity"),
				   sinkFilename = report)
	nsRow <- fit$parameters[fit$parameters$parameter == "numberSensitivity", ]
	shown <- capture.output(print(fit))
	row <- as.data.frame(fit)
	stopifnot(nsRow$lower == -1,
			  !any(grepl("pVisible", shown)), !("pVisible" %in% names(row)),
			  !any(grepl("pVisible", readLines(report))),
			  fit$parameters$se[1] >= 0)
	unlink(report)

	#only the bounds visible searches ns over [0, 1]
	d <- data.frame(target = lines$bounded$targets,
					estimate = one(8, lines$bounded, 0.5, 0.2))
	fit <- fitLine(d, 1000, c(0, 1000), free = c("firstEstimate", "numberSensitivity"))
	nsRow <- fit$parameters[fit$parameters$parameter == "numberSensitivity", ]
	stopifnot(nsRow$lower == 0, nsRow$value >= 0)

	#and a range below 0 there is outside the domain
	msg <- errorFrom(fitLine(d, 1000, c(0, 1000),
							 free = list(firstEstimate = c(0, 1), numberSensitivity = c(-1, 1))))
	stopifnot(grepl("numberSensitivity", msg), grepl("domain", msg))

}

cat("test-number-sensitivity-axis.R passed\n")

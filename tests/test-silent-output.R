## outputNLfitStats() writes only what it is given a file name for. With no
## plotFileName it must open no graphics device, which is what would leave an
## Rplots.pdf behind in a script, and with no sinkFilename it must print nothing.
library(nlFit)

df <- data.frame(target = c(10, 30, 50, 70, 90), estimate = c(20, 35, 48, 66, 88))
statList <- list(upperBound = 100, lowerBound = 0, firstEstimate = 0.5,
                 memoryLength = 5, numberSensitivity = 0.8, accuracyPercent = 0.2,
                 pIncludeConceptualPoints = 0, visibleReferencePoints = c(0, 100),
                 conceptualReferencePoints = NULL, targetOrder = "fixed")

hadPlotFile <- file.exists("Rplots.pdf")
printed <- capture.output(out <- outputNLfitStats(df, statList, pars.n = 3, loops = 10))

## nothing printed and no plot file created
stopifnot(length(printed) == 0)
stopifnot(hadPlotFile == file.exists("Rplots.pdf"))

## the numbers are still returned
stopifnot(is.finite(out$runStats$fitStats$BIC), is.finite(out$runStats$fitStats$r2))
stopifnot(nrow(out$df.fitted) == nrow(df))

## with the file names given, both files are written
plotFile <- tempfile(fileext = ".pdf")
sinkFile <- tempfile(fileext = ".txt")
invisible(capture.output(outputNLfitStats(df, statList, pars.n = 3, loops = 10,
                                          plotFileName = plotFile, sinkFilename = sinkFile)))
stopifnot(file.exists(plotFile), file.exists(sinkFile))
stopifnot(any(grepl("nlFit Statistics", readLines(sinkFile))))
unlink(c(plotFile, sinkFile))

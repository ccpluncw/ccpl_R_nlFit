# Internal machinery for the trial-by-trial simulation.
#
# The remembered set is a list of plain numeric vectors held in value order, so a
# trial costs a handful of vector operations rather than a data-frame subset. The
# fields are:
#   value     the number the row occupies on the line (a bound can be relabelled)
#   label     the number the row stands for, used by the accuracy component
#   estimate  where the row sits in space
#   low, high the ends of the row's range of equivalent numbers
#   trial     the trial the row came from; 0 marks a reference point
#   key       the row's position in the caller's data, which breaks ties in value
# value and label move together and differ only if a caller hands getEstimateNL()
# two different columns for them.
#
# Each theoretical step of the model is one helper below and the trial loop in
# nlRunTrials() is the sequence of those steps.


# the remembered set: every reference point plus the estimates from the last
# memoryLength trials.
nlRememberedSet <- function(mem, trialNumber, memoryLength) {
	lowEdge <- trialNumber - memoryLength
	#nothing has dropped out yet, so the whole store is the remembered set
	if(lowEdge <= 1) return(mem)
	keep <- mem$trial == 0 | mem$trial >= lowEdge
	lapply(mem, function(x) x[keep])
}


# the anchors: the remembered values immediately below and above the target. The
# nearest remembered value by absolute distance is found first, taking the first
# of any duplicates so the earlier trial wins a tie, and a target equal to a
# remembered value brackets with the item below it.
#
# A target at or beyond a bound has no remembered value on that side. The bound
# is then relabelled to the target, that is, the participant treats the end of
# the line as that number. The relabel moves the value and the label only: the
# bound's estimate and its range of equivalent numbers stay where they were, so a
# relabelled bound can never satisfy an equivalence test. The two guards are not
# symmetric - the low one also fires when the target equals the lower bound, the
# high one does not fire when the target equals the upper bound.
nlBracket <- function(rem, target) {
	adjacent <- which.min(abs(rem$value - target))
	nRemembered <- length(rem$value)
	relabel <- NULL

	lowIdx <- if(target > rem$value[adjacent]) adjacent else adjacent - 1L
	if(lowIdx < 1L) {
		lowIdx <- 1L
		relabel <- rem$value[adjacent]
		rem$value[adjacent] <- target
		rem$label[adjacent] <- target
	}

	highIdx <- if(target > rem$value[adjacent]) adjacent + 1L else adjacent
	if(highIdx > nRemembered) {
		highIdx <- nRemembered
		relabel <- rem$value[adjacent]
		rem$value[adjacent] <- target
		rem$label[adjacent] <- target
	}

	list(remembered = rem, low = lowIdx, high = highIdx, relabel = relabel)
}


# a relabelled bound keeps its new value for the rest of the run, so the relabel
# is written back to the store and the store is put back into value order.
nlRelabelReference <- function(mem, fromValue, toValue) {
	hit <- mem$trial == 0 & mem$value == fromValue
	if(!any(hit)) return(mem)
	mem$value[hit] <- toValue
	mem$label[hit] <- toValue
	o <- order(mem$value, mem$key)
	lapply(mem, function(x) x[o])
}


# the target and an anchor are treated as the same number when the target falls
# inside the anchor's range of equivalent numbers, or, for a remembered estimate,
# when the anchor's value falls inside the target's range. Each number therefore
# keeps its own resolution and the comparison does not depend on which was seen
# first. A reference point is always discriminable from the target, so the
# reverse test skips reference rows.
#
# Matching one anchor collapses both anchors onto it. Matching both leaves the
# anchors alone and aims the accuracy component at the mean of their labels.
nlEquivalence <- function(rem, lowIdx, highIdx, target, targetLabel, targetLow, targetHigh) {
	sameNumber <- function(i) {
		(target >= rem$low[i] && target <= rem$high[i]) ||
			(rem$trial[i] > 0 && rem$value[i] >= targetLow && rem$value[i] <= targetHigh)
	}
	hitLow <- sameNumber(lowIdx)
	hitHigh <- sameNumber(highIdx)

	if(hitLow && !hitHigh) highIdx <- lowIdx
	if(!hitLow && hitHigh) lowIdx <- highIdx

	if(hitLow && hitHigh) accuracyTarget <- mean(c(rem$label[lowIdx], rem$label[highIdx]))
		else if(hitLow) accuracyTarget <- rem$label[lowIdx]
		else if(hitHigh) accuracyTarget <- rem$label[highIdx]
		else accuracyTarget <- targetLabel

	list(low = lowIdx, high = highIdx, hitLow = hitLow, hitHigh = hitHigh, accuracyTarget = accuracyTarget)
}


# where in the region between the anchors the target lands. With no remembered
# estimate bracketing it - both anchors are reference points - firstEstimate sets
# the position; otherwise the target lands midway between the two anchors.
nlPlacement <- function(rem, lowIdx, highIdx, firstEstimate) {
	eLow <- rem$estimate[lowIdx]
	eHigh <- rem$estimate[highIdx]
	unanchored <- rem$trial[lowIdx] == 0 && rem$trial[highIdx] == 0
	if(unanchored && !is.null(firstEstimate)) eLow + (firstEstimate * abs(eHigh - eLow))
		else mean(c(eLow, eHigh))
}


# the response: the ordinal placement blended with accurate responding, then held
# inside the bounds.
nlResponse <- function(placement, accuracyTarget, accuracyPercent, lowerBound, upperBound) {
	response <- placement * (1 - accuracyPercent) + (accuracyTarget * accuracyPercent)
	if(response > upperBound) upperBound
		else if(response < lowerBound) lowerBound
		else response
}


# the new estimate joins the store at its rank: after any earlier trial that
# shares its value and before any reference point that does.
nlRemember <- function(mem, value, label, estimate, low, high, trial, key) {
	at <- sum(mem$value < value) + sum(mem$value == value & mem$key < key)
	list(value = append(mem$value, value, after = at),
		 label = append(mem$label, label, after = at),
		 estimate = append(mem$estimate, estimate, after = at),
		 low = append(mem$low, low, after = at),
		 high = append(mem$high, high, after = at),
		 trial = append(mem$trial, trial, after = at),
		 key = append(mem$key, key, after = at))
}


# the reference points as a remembered-set store. The bounds are reference points
# too, and are added only when they are not already visible.
nlReferenceStore <- function(lowerBound, upperBound, visibleReferencePoints, conceptualIncluded, keyOffset) {
	bounds <- c(lowerBound, upperBound)
	values <- c(bounds[!(bounds %in% visibleReferencePoints)], visibleReferencePoints)
	#a conceptual point that is already visible adds nothing
	if(!is.null(conceptualIncluded)) values <- unique(c(values, conceptualIncluded))

	key <- keyOffset + seq_along(values)
	o <- order(values, key)
	values <- values[o]
	list(value = values, label = values, estimate = values, low = values, high = values,
		 trial = rep(0, length(values)), key = key[o])
}


# the store built from a data frame of reference rows, for callers that supply
# their own reference estimates and ranges.
nlStoreFromRows <- function(value, label, estimate, low, high, trial, key) {
	o <- order(value, key)
	list(value = value[o], label = label[o], estimate = estimate[o], low = low[o],
		 high = high[o], trial = trial[o], key = key[o])
}


# run the trials in order. targets, labels, low, high and keys are one element
# per trial, in presentation order; reference is the store the run starts from.
nlRunTrials <- function(targets, labels, low, high, keys, reference, memoryLength,
						accuracyPercent, firstEstimate, lowerBound, upperBound, verbose = FALSE) {

	mem <- reference
	estimates <- numeric(length(targets))

	for(trialNumber in seq_along(targets)) {
		target <- targets[trialNumber]

		remembered <- nlRememberedSet(mem, trialNumber, memoryLength)
		bracket <- nlBracket(remembered, target)
		if(!is.null(bracket$relabel)) mem <- nlRelabelReference(mem, bracket$relabel, target)
		remembered <- bracket$remembered

		equivalence <- nlEquivalence(remembered, bracket$low, bracket$high, target,
									 labels[trialNumber], low[trialNumber], high[trialNumber])
		placement <- nlPlacement(remembered, equivalence$low, equivalence$high, firstEstimate)
		estimates[trialNumber] <- nlResponse(placement, equivalence$accuracyTarget,
											 accuracyPercent, lowerBound, upperBound)

		mem <- nlRemember(mem, target, labels[trialNumber], estimates[trialNumber],
						  low[trialNumber], high[trialNumber], trialNumber, keys[trialNumber])

		if(verbose) nlTraceTrial(trialNumber, target, remembered, bracket, equivalence,
								 placement, accuracyPercent, estimates[trialNumber])
	}

	list(estimate = estimates, store = mem)
}


# a trial-by-trial trace of the steps above, for debugging.
nlTraceTrial <- function(trialNumber, target, remembered, bracket, equivalence, placement, accuracyPercent, estimate) {
	cat("**** trial", trialNumber, "target", target, "****\n")
	print(data.frame(value = remembered$value, label = remembered$label,
					 estimate = remembered$estimate, low = remembered$low,
					 high = remembered$high, trial = remembered$trial))
	cat("bracket:", bracket$low, bracket$high,
		if(is.null(bracket$relabel)) "" else paste("(relabelled", bracket$relabel, "->", target, ")"), "\n")
	cat("equivalence:", equivalence$low, equivalence$high,
		"hits:", equivalence$hitLow, equivalence$hitHigh,
		"accuracy target:", equivalence$accuracyTarget, "\n")
	cat("placement:", placement, "accuracyPercent:", accuracyPercent, "estimate:", estimate, "\n\n")
}


# the k-th occurrence of each value, counted in the order the values are given.
nlPresentationIndex <- function(values) {
	uniqueValues <- unique(values)
	key <- match(values, uniqueValues)
	counts <- tabulate(key, nbins = length(uniqueValues))
	if(all(counts == 1L)) return(rep(1L, length(values)))
	presentation <- integer(length(values))
	presentation[order(key)] <- sequence(counts)
	presentation
}

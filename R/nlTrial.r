#store: vectors in value order, trial 0 is a reference, key breaks value ties


#the remembered set: every reference point plus the last memoryLength estimates
nlRememberedSet <- function(mem, trialNumber, memoryLength) {
	lowEdge <- trialNumber - memoryLength
	#nothing has dropped out yet, so the whole store is the remembered set
	if(lowEdge <= 1) return(mem)
	keep <- mem$trial == 0 | mem$trial >= lowEdge
	lapply(mem, function(x) x[keep])
}


#find the nearest value, earlier trial first, an equal target brackets below
nlBracket <- function(rem, target) {
	adjacent <- which.min(abs(rem$value - target))
	nRemembered <- length(rem$value)
	relabel <- NULL

	lowIdx <- if(target > rem$value[adjacent]) adjacent else adjacent - 1L
	#no remembered value below: relabel value and label, not estimate or range
	if(lowIdx < 1L) {
		lowIdx <- 1L
		relabel <- rem$value[adjacent]
		rem$value[adjacent] <- target
		rem$label[adjacent] <- target
	}

	highIdx <- if(target > rem$value[adjacent]) adjacent + 1L else adjacent
	#unlike the low guard this one does not fire at equality with the bound
	if(highIdx > nRemembered) {
		highIdx <- nRemembered
		relabel <- rem$value[adjacent]
		rem$value[adjacent] <- target
		rem$label[adjacent] <- target
	}

	list(remembered = rem, low = lowIdx, high = highIdx, relabel = relabel)
}


#relabel reference rows only, a relabelled trial row holds for this trial alone
nlRelabelReference <- function(mem, fromValue, toValue) {
	hit <- mem$trial == 0 & mem$value == fromValue
	if(!any(hit)) return(mem)
	mem$value[hit] <- toValue
	mem$label[hit] <- toValue
	o <- order(mem$value, mem$key)
	lapply(mem, function(x) x[o])
}


#either number in the other's range matches, whichever was seen first
nlEquivalence <- function(rem, lowIdx, highIdx, target, targetLabel, targetLow, targetHigh) {
	#a reference point is always discriminable, so the reverse test skips it
	sameNumber <- function(i) {
		(target >= rem$low[i] && target <= rem$high[i]) ||
			(rem$trial[i] > 0 && rem$value[i] >= targetLow && rem$value[i] <= targetHigh)
	}
	hitLow <- sameNumber(lowIdx)
	hitHigh <- sameNumber(highIdx)

	#one hit collapses the anchors, two aim accuracy at the mean of the labels
	if(hitLow && !hitHigh) highIdx <- lowIdx
	if(!hitLow && hitHigh) lowIdx <- highIdx

	if(hitLow && hitHigh) accuracyTarget <- mean(c(rem$label[lowIdx], rem$label[highIdx]))
		else if(hitLow) accuracyTarget <- rem$label[lowIdx]
		else if(hitHigh) accuracyTarget <- rem$label[highIdx]
		else accuracyTarget <- targetLabel

	list(low = lowIdx, high = highIdx, hitLow = hitLow, hitHigh = hitHigh, accuracyTarget = accuracyTarget)
}


#firstEstimate places a target with no remembered anchor, others land midway
nlPlacement <- function(rem, lowIdx, highIdx, firstEstimate) {
	eLow <- rem$estimate[lowIdx]
	eHigh <- rem$estimate[highIdx]
	unanchored <- rem$trial[lowIdx] == 0 && rem$trial[highIdx] == 0
	if(unanchored && !is.null(firstEstimate)) eLow + (firstEstimate * abs(eHigh - eLow))
		else mean(c(eLow, eHigh))
}


#ordinal placement blended with accurate responding, held inside the bounds
nlResponse <- function(placement, accuracyTarget, accuracyPercent, lowerBound, upperBound) {
	response <- placement * (1 - accuracyPercent) + (accuracyTarget * accuracyPercent)
	if(response > upperBound) upperBound
		else if(response < lowerBound) lowerBound
		else response
}


#a new estimate goes after earlier equal trials, before an equal reference
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


#reference store: the bounds are added unless they are already visible
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


#store from reference rows, where a caller's label may differ from its value
nlStoreFromRows <- function(value, label, estimate, low, high, trial, key) {
	o <- order(value, key)
	list(value = value[o], label = label[o], estimate = estimate[o], low = low[o],
		 high = high[o], trial = trial[o], key = key[o])
}


#one element per trial in presentation order, reference is the starting store
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


#a trial-by-trial trace of the steps above, for debugging
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


#the k-th occurrence of each value, counted in the order the values are given
nlPresentationIndex <- function(values) {
	uniqueValues <- unique(values)
	key <- match(values, uniqueValues)
	counts <- tabulate(key, nbins = length(uniqueValues))
	if(all(counts == 1L)) return(rep(1L, length(values)))
	presentation <- integer(length(values))
	presentation[order(key)] <- sequence(counts)
	presentation
}

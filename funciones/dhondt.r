dhondt = function(dataf,totalSeats,partyNamesColumn = 1,votesColumn = 2,minThreshold = NULL) {

	dataToUse =  dataf
	dataToUse$relativeVotes = dataToUse[,votesColumn]/sum(dataToUse[,votesColumn])
		
	if (!is.null(minThreshold)) {
		if (minThreshold>0.1) {
			stop("The minimum threshold cannot be higher than 10%")
		}
		dataToUse = dataToUse[dataToUse$relativeVotes>=minThreshold,]
	}
	
	quotientMatrix = round(matrix(dataToUse$relativeVotes,ncol=1)%*%matrix(1/(1:totalSeats),nrow=1),8)
	
	smallerSeat = sort(unique(as.vector(quotientMatrix)),decreasing = T)[totalSeats]
	
	# In the event of a tie that would have more quotient get a seat that there are seats available, randomize if the last seat is the problem.
	if (sum(quotientMatrix>=smallerSeat) == totalSeats) {
		seats = apply(quotientMatrix,1,FUN=function(x) sum(x>=smallerSeat))
	} else {
		checkQuotientSeat = totalSeats
		checkSmallerSeat = smallerSeat
			while (sum(quotientMatrix>=checkSmallerSeat) > totalSeats) {
				checkQuotientSeat = checkQuotientSeat - 1
				checkSmallerSeat = sort(unique(quotientMatrix),decreasing = T)[checkQuotientSeat]
			}
			seats = apply(quotientMatrix,1,FUN=function(x) sum(x>=checkSmallerSeat))
			remainingSeats = totalSeats - sum(quotientMatrix>=checkSmallerSeat)
			if (remainingSeats > 0) {
				tiedSeat = sort(unique(as.vector(quotientMatrix)),decreasing = T)[checkQuotientSeat-1]
				tiedParties = which(apply(quotientMatrix, 1,FUN =  function(x) any(x == tiedSeat)))
				getSeats = sample(tiedParties,remainingSeats)
				print(paste0("The allocation of ",remainingSeats," has been randomized due to ties. A seat has gone to party ",dataToUse[getSeats,partyNamesColumn]))
				addedSeats = numeric(length(seats))
				addedSeats[getSeats] = 1
				seats = seats + addedSeats
			}
	}
	
	names(seats) = dataToUse[,partyNamesColumn]
	
	return(seats)
}



diputados = function(dataf,totalSeats = 99,regionColumn,partyNamesColumn = 1,votesColumn = 2,levelsParty = NULL,invalidVotesRows = NULL,printGraph = F,saveGraph = F,allowDiffDepts = F,externalDeptDistribution = NULL,minDipDept = 2,detail = F) {

# https://portal.factum.uy/ediciones-anteriores/estpol/sispol/sip90001.html
# sección 3.2
# https://www.gub.uy/corte-electoral/sites/corte-electoral/files/2023-07/ACTA_N_9416_DIPUTADOS_2015_2020.pdf

	if (!is.data.frame(dataf)) {
		warning("data argument is not data.frame. Passing to data.frame")
		dataf = as.data.frame(dataf)
	}
	
	dataf$rowID = 1:nrow(dataf)
	
	if (is.character(regionColumn)) {
		regionColumn = match(regionColumn,colnames(dataf))
		if (is.na(regionColumn)) {
			stop("regionColumn not found in dataset.")
		} else {
			colnames(dataf)[regionColumn] = "dpto"
		}
	}
	
	if (is.character(partyNamesColumn)) {
		partyNamesColumn = match(partyNamesColumn,colnames(dataf))
		if (is.na(votesColumn)) {
			warning(paste0("partyNamesColumn not found in dataset. Using ",colnames(dataf)[1]))
			partyNamesColumn = 1
		}
	}
	colnames(dataf)[partyNamesColumn] = "party"
		
	if (is.character(votesColumn)) {
		votesColumn = match(votesColumn,colnames(dataf))
		if (is.na(votesColumn)) {
			warning(paste0("votesColumn not found in dataset. Using ",colnames(dataf)[2]))
			votesColumn = 2
		}
	}
	colnames(dataf)[votesColumn] = "votesPartyDpto"

	
	if (length(unique(c(regionColumn,partyNamesColumn,votesColumn))) < 3) {
		stop("Have to declare partyNamesColumn or votesColumn")
	}
			
	if (is.null(levels)) {nLevels = 1} else {
		nLevels = length(levels)
	}
	
	if (!is.null(invalidVotesRows)) {
		dataToUse = dataf[!dataf$party%in%invalidVotesRows,]
	} else {
		dataToUse = dataf
	}
	
	if (!is.null(externalDeptDistribution)) {
		if (sum(externalDeptDistribution[,2]) == totalSeats) {
			seatsByDepartmentDF = externalDeptDistribution[,c(1,2)]
		} else {
			seatsByDepartmentR1 = dhondt(externalDeptDistribution,totalSeats)
			if (any(seatsByDepartmentR1<minDipDept)) {
				seatsByDepartmentRound2 = seatsByDepartmentR1
				seatsByDepartmentRound2[which(seatsByDepartmentR1<minDipDept)] = minDipDept
				seatsAvailableRound2 = totalSeats - sum(seatsByDepartmentR1<minDipDept)*minDipDept
				subsetDataRound2 = externalDeptDistribution[!(seatsByDepartmentR1<minDipDept),]
				seatsByDepartmentOnSubsetRound2 = dhondt(subsetDataRound2,seatsAvailableRound2)
				seatsAvailableRound2[!(seatsByDepartmentR1<minDipDept),2] = seatsByDepartmentOnSubsetRound2
				seatsByDepartment = seatsAvailableRound2
				seatsByDepartmentDF = cbind(externalDeptDistribution[,1],seatsByDepartment)
			}
		}
		if (any(!externalDeptDistribution[,1]%in%dataf[,regionColumn]) | any(!dataf[,regionColumn]%in%externalDeptDistribution[,1])) {
			stop("Some department name in the external seat distribution column1 does not match the department name in the votes data")
		}
	} else {
		votesByDepartment = aggregate(dataf[,"votesPartyDpto"], by = list(dataf[, "dpto"]), FUN = sum)
		seatsByDepartmentR1 = dhondt(votesByDepartment,totalSeats)
			if (any(seatsByDepartmentR1<minDipDept)) {
				seatsByDepartmentRound2 = seatsByDepartmentR1
				seatsByDepartmentRound2[which(seatsByDepartmentR1<minDipDept)] = minDipDept
				seatsAvailableRound2 = totalSeats - sum(seatsByDepartmentR1<minDipDept)*minDipDept
				subsetDataRound2 = votesByDepartment[!(seatsByDepartmentR1<minDipDept),]
				seatsByDepartmentOnSubsetRound2 = dhondt(subsetDataRound2,seatsAvailableRound2)
				seatsAvailableRound2[!(seatsByDepartmentR1<minDipDept),2] = seatsByDepartmentOnSubsetRound2
				seatsByDepartment = seatsAvailableRound2
				seatsByDepartmentDF = cbind(votesByDepartment[,1],seatsByDepartment)
			}
	}
	
	colnames(seatsByDepartmentDF) = c("dpto","seatsByDpto")
	
	
	# SUBSET ON VALID VOTES
	
	if (!is.null(invalidVotesRows)) {
		dataToUse = dataf[!dataf$party%in%invalidVotesRows,]
	} else {
		dataToUse = dataf
	}
	
	# ROUND 1: WHOLE SEATS
	
	validVotesByDepartment = aggregate(votesPartyDpto~dpto,dataToUse, FUN = sum)
	validVotesByDepartment = merge(validVotesByDepartment,seatsByDepartmentDF,all.x=T)
	validVotesByDepartment$votesBySeatDpto = validVotesByDepartment$votesPartyDpto/validVotesByDepartment$seatsByDpto

	r1DF = merge(dataToUse,validVotesByDepartment[,c("dpto","votesBySeatDpto")])
	r1DF = r1DF[order(r1DF$rowID,decreasing=F),]
	r1DF$quotientR1 = r1DF$votesPartyDpto/r1DF$votesBySeatDpto

	dataToUse$r1Seats = floor(r1DF$quotientR1)
	dataToUse$immovableSeats = floor(r1DF$quotientR1)
	dataToUse$assignedSeats = dataToUse$immovableSeats
	seatsLeftToAssign = totalSeats -  sum(dataToUse$assignedSeats)
	
	# ROUND 2: REPRESENTATION FOR SMALLER DEPARTAMENTS
	
	if (seatsLeftToAssign > 0) {
		overallVotesByParty = aggregate(votesPartyDpto~party,dataToUse,FUN = sum)
		overallPartySeats = dhondt(overallVotesByParty,totalSeats)
		overallPartySeatsDF = data.frame(overallVotesByParty,overallPartySeats)
		
		overallSeatsByPartyR1 = aggregate(assignedSeats~party,dataToUse, FUN = sum)
		overallPartySeatsDF = merge(overallPartySeatsDF,overallSeatsByPartyR1,all.x=T)
		overallPartySeatsDF$leftAfterR1 = overallPartySeatsDF$overallPartySeats - overallPartySeatsDF$assignedSeats
		colnames(overallPartySeatsDF)[colnames(overallPartySeatsDF)=="assignedSeats"] = "assignedSeatsByParty"

		overallSeatsByDptoR1 = aggregate(assignedSeats~dpto,dataToUse, FUN = sum)
		overallSeatsByDptoR1 = merge(overallSeatsByDptoR1,seatsByDepartmentDF)
		overallSeatsByDptoR1$leftAfterR1 = overallSeatsByDptoR1$seatsByDpto - overallSeatsByDptoR1$assignedSeats
		colnames(overallSeatsByDptoR1)[colnames(overallSeatsByDptoR1)=="assignedSeats"] = "assignedSeatsByDpto"
		
		r2DF = dataToUse
		r2DF = merge(r2DF,overallSeatsByDptoR1[,c("dpto","assignedSeatsByDpto","seatsByDpto")])
		r2DF = merge(r2DF,overallPartySeatsDF[,c("party","assignedSeatsByParty","overallPartySeats")])
		r2DF$divisorR2 = (r2DF$assignedSeats+1)
		r2DF$quotientR2 = r2DF$votesPartyDpto/r2DF$divisorR2
		r2DF = r2DF[order(r2DF$quotientR2,decreasing=T),]
		r2DF$quotientSeats = 0

	
		while (any(r2DF$assignedSeatsByDpto<2)) {
			rowAddSeat = min(which(with(r2DF,(assignedSeatsByDpto<2)&assignedSeatsByParty<overallPartySeats)))
			r2DF$immovableSeats[rowAddSeat] = r2DF$immovableSeats[rowAddSeat] + 1
			r2DF$assignedSeats[rowAddSeat] = r2DF$assignedSeats[rowAddSeat] + 1
			r2DF$quotientSeats[rowAddSeat] = r2DF$quotientSeats[rowAddSeat] + 1
			r2DF$assignedSeatsByDpto[r2DF$dpto == r2DF$dpto[rowAddSeat]] = r2DF$assignedSeatsByDpto[r2DF$dpto == r2DF$dpto[rowAddSeat]] + 1
			r2DF$assignedSeatsByParty[r2DF$party == r2DF$party[rowAddSeat]] = r2DF$assignedSeatsByParty[r2DF$party == r2DF$party[rowAddSeat]] + 1
			r2DF$divisorR2 = (r2DF$assignedSeats+1)
			r2DF$quotientR2 = r2DF$votesPartyDpto/r2DF$divisorR2
			r2DF = r2DF[order(r2DF$quotientR2,decreasing=T),]			
		}
		
	seatsLeftToAssign = totalSeats -  sum(r2DF$assignedSeats)

	}
	
	# ROUND 3: ASSIGN REST OF OUTSTANDING SEATS
	
	if (seatsLeftToAssign > 0) {
	
		while (seatsLeftToAssign>0) {
			rowAddSeat = min(which(with(r2DF,(assignedSeatsByDpto<seatsByDpto)&assignedSeatsByParty<overallPartySeats & quotientSeats ==0)))
			if (rowAddSeat == Inf) {
				rowAddSeat = min(which(with(r2DF,(assignedSeatsByDpto<seatsByDpto)&assignedSeatsByParty<overallPartySeats)))
			}
			r2DF$quotientSeats[rowAddSeat] = 1
			r2DF$assignedSeats[rowAddSeat] = r2DF$assignedSeats[rowAddSeat] + 1
			r2DF$assignedSeatsByDpto[r2DF$dpto == r2DF$dpto[rowAddSeat]] = r2DF$assignedSeatsByDpto[r2DF$dpto == r2DF$dpto[rowAddSeat]] + 1
			r2DF$assignedSeatsByParty[r2DF$party == r2DF$party[rowAddSeat]] = r2DF$assignedSeatsByParty[r2DF$party == r2DF$party[rowAddSeat]] + 1
			r2DF$divisorR2 = (r2DF$assignedSeats+1)
			r2DF$quotientR2 = r2DF$votesPartyDpto/r2DF$divisorR2
			r2DF = r2DF[order(r2DF$quotientR2,decreasing=T),]			
			seatsLeftToAssign = seatsLeftToAssign - 1
		}
	}
	
	# ROUND 4: INTRA PARTY CHECKS
	
	r2DF$competingQuotient = ifelse(r2DF$votesPartyDpto==0,0,ifelse(r2DF$assignedSeats==0,r2DF$votesPartyDpto,r2DF$votesPartyDpto/(r2DF$assignedSeats)))
	r2DF = r2DF[order(r2DF$competingQuotient,decreasing=T),]
	
	partiesToCheck = overallPartySeatsDF$party[overallPartySeatsDF$overallPartySeats>0]
	
	for (i in seq_along(partiesToCheck)) {
		auxDFR4 = subset(r2DF,party==partiesToCheck[i])
		notRepresented = which(auxDFR4$assignedSeats==0)
		keepGoing = T
		while (length(notRepresented)>0 & keepGoing) {
			highestNotRepresented = min(notRepresented)
			lowestCandidateTake = max(which((auxDFR4$assignedSeats-auxDFR4$immovableSeats)>0))
			rowAddSeat = which(r2DF$party == partiesToCheck[i] & r2DF$dpto == auxDFR4$dpto[highestNotRepresented])
			rowTakeSeat = which(r2DF$party == partiesToCheck[i] & r2DF$dpto == auxDFR4$dpto[lowestCandidateTake])
			if (highestNotRepresented < lowestCandidateTake) {
				r2DF[rowAddSeat,c("assignedSeats")] = r2DF[rowAddSeat,c("assignedSeats")] + 1
				r2DF[rowTakeSeat,c("assignedSeats")] = r2DF[rowTakeSeat,c("assignedSeats")] - 1
				print(paste0(partiesToCheck[i]," seat changed from ",r2DF$dpto[rowTakeSeat]," to ",r2DF$dpto[rowAddSeat]))
				r2DF$competingQuotient = ifelse(r2DF$votesPartyDpto==0,0,ifelse(r2DF$assignedSeats==0,r2DF$votesPartyDpto,r2DF$votesPartyDpto/(r2DF$assignedSeats)))
				r2DF = r2DF[order(r2DF$competingQuotient,decreasing=T),]
				auxDFR4 = subset(r2DF,party==partiesToCheck[i])
				notRepresented = which(auxDFR4$assignedSeats==0)}
			else {
				keepGoing = F
			}
		}
	}
	
	r2DF$competingQuotient = ifelse(r2DF$votesPartyDpto==0,0,ifelse(r2DF$assignedSeats==0,r2DF$votesPartyDpto,r2DF$votesPartyDpto/(r2DF$assignedSeats)))
	
	r2DF = r2DF[order(r2DF$votesPartyDpto,decreasing=T),]
	r2DF = r2DF[order(r2DF$dpto),]


	if (detail) {
		return(r2DF)
	} else {
		outDF = subset(r2DF,assignedSeats>0)
		outDF = outDF[,c("party","dpto","votesPartyDpto","assignedSeats")]
		return(outDF)
	}
}


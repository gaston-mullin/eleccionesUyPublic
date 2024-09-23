senado = function(dataf,totalSeats = 30,partyNamesColumn = 1,votesColumn = 2,levels = NULL,invalidVotesRows = NULL,printGraph = F,colsGraph = NULL,orderGraph = NULL) {

	if (!is.data.frame(dataf)) {
		warning("data argument is not data.frame. Passing to data.frame")
		dataf = as.data.frame(dataf)
	}
	
	if (is.character(partyNamesColumn)) {
		partyNamesColumn = match(partyNamesColumn,colnames(dataf))
		if (is.na(votesColumn)) {
			warning(paste0("partyNamesColumn not found in dataset. Using ",colnames(dataf)[1]))
			partyNamesColumn = 1
		}
	}
		
	if (is.character(votesColumn)) {
		votesColumn = match(votesColumn,colnames(dataf))
		if (is.na(votesColumn)) {
			warning(paste0("votesColumn not found in dataset. Using ",colnames(dataf)[2]))
			votesColumn = 2
		}
	}
			
	if (is.null(levels)) {nLevels = 1} else {
		nLevels = length(levels)
	}
	
	if (!is.null(invalidVotesRows)) {
		dataToUse = dataf[!dataf$partyNamesColumn%in%invalidVotesRows,]
	} else {
		dataToUse = dataf
	}
	
	seatsSenado = dhondt(dataf = dataToUse,totalSeats = totalSeats,partyNamesColumn = partyNamesColumn,votesColumn = votesColumn)
	
	if (printGraph & totalSeats == 30) {

	windows(width=8,height=6)

		positionSeats = data.frame(posX = numeric(30),posY = numeric(30))
		positionSeats$posX[1:14] = 1.3*cos(pi*(seq(15,165,length=14)/180))
		positionSeats$posY[1:14] = 1.3*sin(pi*(seq(15,165,length=14)/180))
		positionSeats$posX[15:30] = 1.7*cos(pi*(seq(15,165,length=16)/180))
		positionSeats$posY[15:30] = 1.7*sin(pi*(seq(15,165,length=16)/180))
		
		positionSeats$orderSeats = c((1:6)*2,(7:14)*2 + 1,(1:6)*2 - 1,13,(7:15)*2)
		positionSeats = positionSeats[order(positionSeats$orderSeats,decreasing=T),]
		
		seatsSenadoDF = data.frame(seats = seatsSenado)
		
		seatsSenadoDF$colsGraph = colsGraph

		if (is.null(orderGraph)) {
			seatsSenadoDF$orderGraph = 1:nrow(seatsSenado)
		} else {
			seatsSenadoDF$orderGraph = orderGraph
		}
		
		seatsSenadoDF = seatsSenadoDF[order(seatsSenadoDF$orderGraph),]
		
		colsSeats = unlist(sapply(1:nrow(seatsSenadoDF),FUN=function(x) rep(seatsSenadoDF$colsGraph[x],times=seatsSenadoDF$seats[x])))

		plot(positionSeats$posX,positionSeats$posY,xlab="",ylab="",bty="n",main="",xaxt="n",yaxt="n",cex=5,pch=21,xlim=range(positionSeats$posX),ylim=c(range(positionSeats$posY) + diff(range(positionSeats$posY))*.1*c(-1,1)),bg = colsSeats)

		
	}
	
	return(seatsSenado)

}

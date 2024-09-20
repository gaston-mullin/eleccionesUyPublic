senado = function(dataf,totalSeats = 30,partyNamesColumn = 1,votesColumn = 2,levels = NULL,invalidVotesRows = NULL,printGraph = F,saveGraph = F) {

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
	
	if (printGraph) {
		barplot(seatsSenado,names = names(seatsSenado))
	}
	
	return(seatsSenado)

}

library(openxlsx)
library(reshape2)

functionsToSource = list.files(path = "funciones", pattern = "\\.r$", full.names = TRUE)
lapply(functionsToSource, source)

allData1999 = read.xlsx("data/lemaPorDpto1999.xlsx")

data1999 = melt(allData1999)

colnames(data1999) = c("dpto","party","votesPartyDpto")
data1999 = data1999[,c("dpto","party","votesPartyDpto")]

data1999$dpto = tolower(iconv(data1999$dpto, from = "UTF-8", to = "ASCII//TRANSLIT"))
data1999$party = gsub("\\."," ",data1999$party)

data1999$party[data1999$party!="Frente Amplio"] = substr(data1999$party[data1999$party!="Frente Amplio"],nchar("Partido ")+1,nchar(data1999$party[data1999$party!="Frente Amplio"]))

dataVotesTotalByParty1999 = aggregate(votesPartyDpto~party,data1999,sum)

# unique(dataVotesTotalByParty1999$party) [1] "Colorado"      "Frente Amplio" "Nacional"      "Nuevo Espacio" "Union Cívica" 


colsGraph1999 = c(rgb(1,.2,.2),rgb(.2,.2,1),rgb(.3,1,1),rgb(29/255,43/255,141/255),rgb(7/255,66/255,146/255))
orderGraph1999 = c(3,1,4,2,5)

senado1999 = senado(dataVotesTotalByParty1999,30,printGraph=T,colsGraph = colsGraph1999,orderGraph = orderGraph1999)

# distribucion diputados

#https://portal.factum.uy/ediciones-anteriores/estpol/anapol/1999/anp99043.html

seatsByDpto1999 = read.table("data/distributionDiputadosDpto1999.txt",sep="\t",header=T)

diputados1999 = diputados(data1999,regionColumn = "dpto",partyNamesColumn = "party",votesColumn = "votesPartyDpto",externalDeptDistribution = seatsByDpto1999,detail=T) 

save(diputados1999,file="simulaciones/asignacionDiputados1999.rdata") 

### SIMULACION

nSim = 1000
set.seed(598)

senadoSim1999 = matrix(NA,nSim,length(unique(dataVotesTotalByParty1999$party)))
diputadosDptoSim1999 = matrix(NA,nSim,19)
bancasSim1999 = matrix(NA,nSim,99)

for (i in 1:nSim) {
	draw = rnorm(nrow(data1999),sd=.3)
	centeredDraw = numeric(nrow(data1999))
	centeredDraw[draw>=0] = draw[draw>=0] + 1
	centeredDraw[draw<0] = 1/(1 - draw[draw<0])
	simulatedData = data1999
	simulatedData$randomVotes = round(simulatedData$votesPartyDpto*centeredDraw)
	sumByDpto = aggregate(list(simulatedData$votesPartyDpto,simulatedData$randomVotes),by=list(simulatedData$dpto),sum)
	colnames(sumByDpto) = c("dpto","original","random")
	sumByDpto$scalingFactor = sumByDpto$random/sumByDpto$original
	simulatedData = merge(simulatedData,sumByDpto[,c("dpto","scalingFactor")])
	simulatedData$randomVotes = round(simulatedData$randomVotes/simulatedData$scalingFactor)
	simulatedData$votesPartyDpto = simulatedData$randomVotes 
	
	dataVotesTotalByParty1999 = aggregate(votesPartyDpto~party,simulatedData,sum)

	senadoSim1999[i,] = senado(dataVotesTotalByParty1999,30)
	
	auxDiputados = invisible(diputados(simulatedData,regionColumn = "dpto",partyNamesColumn = "party",votesColumn = "votesPartyDpto",externalDeptDistribution = seatsByDpto1999))
	auxDiputados = subset(auxDiputados,assignedSeats>0)
	aggAuxDiputados = aggregate(assignedSeats~dpto,auxDiputados,sum)
	diputadosDptoSim1999[i,] = aggAuxDiputados[,2]
	bancasSim1999[i,] = unlist(sapply(1:nrow(auxDiputados),FUN=function(x) paste(auxDiputados$party[x],auxDiputados$dpto[x],1:auxDiputados$assignedSeats[x],sep="-")))
	
}

save(bancasSim1999,file="simulaciones/simDiputados1999.rdata")


# PRINT BARPLOT DE DIPUTADOS POR DEPARTAMENTO PARA CADA PARTIDO

uniqueDptos = sort(unique(data1999$dpto))
uniqueParties = unique(dataVotesTotalByParty1999$party)

pdf("casos/graficos/simulacionDiputadosDptoPartidos1999.pdf",width=9,height=6)

for (i in seq_along(uniqueDptos)) {
	whichParties = which(sapply(uniqueParties,FUN=function(x) sum(grepl(paste0(x,"-",uniqueDptos[i]),bancasSim1999)))>0)
	nRowChart = ifelse(length(whichParties)>6,3,2)
	nColChart = ifelse(length(whichParties)>4,3,2)
	nCharts = nRowChart*nColChart + 1
	
	layout(matrix(c(rep(1,nColChart),2:nCharts), ncol = nColChart, byrow = TRUE),heights = c(1,rep(8/nRowChart,nRowChart)))
	par(mar=c(1,1,1,1))
	plot(NULL,xlim=c(0,1),ylim=c(0,1),bty="n",xlab="",ylab="",xaxt="n",yaxt="n")
	text(.5,.5,label=toupper(uniqueDptos[i]),cex=2)
	
	for (j in whichParties) {
		tableDptoParty = table(apply(bancasSim1999,1,FUN=function(x) sum(grepl(paste0(uniqueParties[j],"-",uniqueDptos[i]),x))))
		borderVal = rep(NA,length(tableDptoParty))
		colVal = rep(adjustcolor(colsGraph1999[j],alpha.f = .7),length(tableDptoParty))
		
		trueVal = diputados1999$assignedSeats[diputados1999$dpto==uniqueDptos[i]&diputados1999$party==uniqueParties[j]]
		borderVal[which(names(tableDptoParty)==trueVal)] = 1
		colVal[which(names(tableDptoParty)==trueVal)] = colsGraph1999[j]
		
		par(mar=c(5.1,4.1,4.1,2.1))

		barplot(tableDptoParty,col=colVal,border = borderVal,main = uniqueParties[j])
			}
}

dev.off()


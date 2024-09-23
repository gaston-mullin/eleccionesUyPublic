library(openxlsx)
library(reshape2)

functionsToSource = list.files(path = "funciones", pattern = "\\.r$", full.names = TRUE)
lapply(functionsToSource, source)

data2019 = read.xlsx("data/lemaPorCircuito2019.xlsx")

# votos

data2019$DEPTO = tolower(iconv(data2019$DEPTO, from = "UTF-8", to = "ASCII//TRANSLIT"))
data2019 = data2019[,c(3,14:ncol(data2019))] # keep relevant columns

data2019melted = melt(data2019)
colnames(data2019melted) = c("dpto","party","votesPartyDpto")

dataByDepto2019 = aggregate(votesPartyDpto~dpto + party,data2019melted,sum)
dataVotesTotalByParty = aggregate(votesPartyDpto~party,dataByDepto2019,sum)

# distribucion senado

#  uniqueParties
#  [1] Partido.Frente.Amplio                   
#  [2] Partido.Nacional                        
#  [3] Partido.Colorado                        
#  [4] Partido.Independiente                   
#  [5] Partido.Asamblea.Popular                
#  [6] Partido.de.los.Trabajadores             
#  [7] Partido.Ecologista.Radical.Intransigente
#  [8] Partido.de.la.Gente                     
#  [9] Partido.Verde.Animalista                
# [10] Partido.Digital                         
# [11] Partido.Cabildo.Abierto
colsGraph = c(rgb(.2,.2,1),rgb(.3,1,1),rgb(1,.2,.2),rgb(.6,.1,.6),rgb(.5,.1,.1),NA,rgb(.4,.9,.4),rgb(.2,.9,.2),rgb(.5,1,.5),NA,rgb(.9,.9,.05))
orderGraph = c(3,9,8,7,2,1,4,10,5,6,11)

senado(dataVotesTotalByParty,30,printGraph=T,colsGraph = colsGraph,orderGraph = orderGraph)



# distribucion diputados

seatsByDpto = read.table("data/distributionDiputadosDpto2019.txt",sep="\t",header=T)

tcd = diputados(dataByDepto2019,regionColumn = "dpto",partyNamesColumn = "party",votesColumn = "votesPartyDpto",externalDeptDistribution = seatsByDpto) 

subset(tcd,party=="Partido.Colorado")

### SIMULACION

nSim = 1000

senadoSim = matrix(NA,nSim,11)
diputadosDptoSim = matrix(NA,nSim,19)
bancasSim = matrix(NA,nSim,99)

for (i in 1:nSim) {
	draw = rnorm(nrow(dataByDepto2019),sd=.3)
	centeredDraw = numeric(nrow(dataByDepto2019))
	centeredDraw[draw>=0] = draw[draw>=0] + 1
	centeredDraw[draw<0] = 1/(1 - draw[draw<0])
	simulatedData = dataByDepto2019
	simulatedData$randomVotes = round(simulatedData$votesPartyDpto*centeredDraw)
	sumByDpto = aggregate(list(simulatedData$votesPartyDpto,simulatedData$randomVotes),by=list(simulatedData$dpto),sum)
	colnames(sumByDpto) = c("dpto","original","random")
	sumByDpto$scalingFactor = sumByDpto$random/sumByDpto$original
	simulatedData = merge(simulatedData,sumByDpto[,c("dpto","scalingFactor")])
	simulatedData$randomVotes = round(simulatedData$randomVotes/simulatedData$scalingFactor)
	simulatedData$votesPartyDpto = simulatedData$randomVotes 
	
	dataVotesTotalByParty = aggregate(votesPartyDpto~party,simulatedData,sum)

	senadoSim[i,] = senado(dataVotesTotalByParty,30)
	
	auxDiputados = diputados(simulatedData,regionColumn = "dpto",partyNamesColumn = "party",votesColumn = "votesPartyDpto",externalDeptDistribution = seatsByDpto)
	auxDiputados = subset(auxDiputados,assignedSeats>0)
	aggAuxDiputados = aggregate(assignedSeats~dpto,auxDiputados,sum)
	diputadosDptoSim[i,] = aggAuxDiputados[,2]
	bancasSim[i,] = unlist(sapply(1:nrow(auxDiputados),FUN=function(x) paste(auxDiputados$party[x],auxDiputados$dpto[x],1:auxDiputados$assignedSeats[x],sep="-")))
	
}

for (i in 1:ncol(diputadosDptoSim)) {
	barplot(table(diputadosDptoSim[,i]),main=aggAuxDiputados[i,1])
	readline()
}

# PRINT BARPLOT DE DIPUTADOS POR DEPARTAMENTO PARA CADA Partido

uniqueDptos = unique(dataByDepto2019$dpto)
uniqueParties = unique(dataByDepto2019$party)
uniquePartiesLabel = gsub("\\."," ",uniqueParties)
uniquePartiesLabel = substr(uniquePartiesLabel,9,nchar(uniquePartiesLabel))

pdf("casos/graficos/simulacionDiputadosDptoPartidos.pdf",width=9,height=6)

for (i in seq_along(uniqueDptos)) {
	whichParties = which(sapply(uniqueParties,FUN=function(x) sum(grepl(paste0(x,"-",uniqueDptos[i]),bancasSim)))>0)
	nRowChart = ifelse(length(whichParties)>6,3,2)
	nColChart = ifelse(length(whichParties)>4,3,2)
	nCharts = nRowChart*nColChart + 1
	
	layout(matrix(c(rep(1,nColChart),2:nCharts), ncol = nColChart, byrow = TRUE),heights = c(1,rep(8/nRowChart,nRowChart)))
	par(mar=c(1,1,1,1))
	plot(NULL,xlim=c(0,1),ylim=c(0,1),bty="n",xlab="",ylab="",xaxt="n",yaxt="n")
	text(.5,.5,label=toupper(uniqueDptos[i]),cex=2)
	
	for (j in whichParties) {
		tableDptoParty = table(apply(bancasSim,1,FUN=function(x) sum(grepl(paste0(uniqueParties[j],"-",uniqueDptos[i]),x))))
		borderVal = rep(NA,length(tableDptoParty))
		colVal = rep(adjustcolor(colsGraph[j],alpha.f = .7),length(tableDptoParty))
		
		trueVal = tcd$assignedSeats[tcd$dpto==uniqueDptos[i]&tcd$party==uniqueParties[j]]
		borderVal[which(names(tableDptoParty)==trueVal)] = 1
		colVal[which(names(tableDptoParty)==trueVal)] = colsGraph[j]
		
		par(mar=c(5.1,4.1,4.1,2.1))

		barplot(tableDptoParty,col=colVal,border = borderVal,main = uniquePartiesLabel[j])
			}
}

dev.off()



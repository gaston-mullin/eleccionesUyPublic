library(openxlsx)
library(reshape2)

functionsToSource = list.files(path = "funciones", pattern = "\\.r$", full.names = TRUE)
lapply(functionsToSource, source)

allData2009 = read.xlsx("data/lemaPorDpto2009.xlsx")

data2009 = melt(allData2009[,1:(ncol(allData2009)-1)])

colnames(data2009) = c("dpto","party","votesPartyDpto")
data2009 = data2009[,c("dpto","party","votesPartyDpto")]

data2009$dpto = tolower(iconv(data2009$dpto, from = "UTF-8", to = "ASCII//TRANSLIT"))
data2009$party = gsub("\\."," ",data2009$party)

data2009$party[data2009$party!="Frente Amplio"] = substr(data2009$party[data2009$party!="Frente Amplio"],nchar("Partido ")+1,nchar(data2009$party[data2009$party!="Frente Amplio"]))

dataVotesTotalByParty2009 = aggregate(votesPartyDpto~party,data2009,sum)

# unique(dataVotesTotalByParty2009$party) [1] "Asamblea Popular" "Colorado"         "Frente Amplio"    "Independiente"    "Nacional"  

colsGraph2009 = c(rgb(.5,.1,.1),rgb(1,.2,.2),rgb(.2,.2,1),rgb(.6,.1,.6),rgb(.3,1,1))
orderGraph2009 = c(1,4,2,3,5)

senado2009 = senado(dataVotesTotalByParty2009,30,printGraph=T,colsGraph = colsGraph2009,orderGraph = orderGraph2009)

# distribucion diputados

seatsByDpto2009 = read.table("data/distributionDiputadosDpto2009.txt",sep="\t",header=T)

diputados2009 = diputados(data2009,regionColumn = "dpto",partyNamesColumn = "party",votesColumn = "votesPartyDpto",externalDeptDistribution = seatsByDpto2009,detail=T)

save(diputados2009,file="simulaciones/asignacionDiputados2009.rdata") 

### SIMULACION

nSim = 1000
set.seed(598)

senadoSim2009 = matrix(NA,nSim,length(unique(dataVotesTotalByParty2009$party)))
diputadosDptoSim2009 = matrix(NA,nSim,19)
bancasSim2009 = matrix(NA,nSim,99)

for (i in 1:nSim) {
	draw = rnorm(nrow(data2009),sd=.3)
	centeredDraw = numeric(nrow(data2009))
	centeredDraw[draw>=0] = draw[draw>=0] + 1
	centeredDraw[draw<0] = 1/(1 - draw[draw<0])
	simulatedData = data2009
	simulatedData$randomVotes = round(simulatedData$votesPartyDpto*centeredDraw)
	sumByDpto = aggregate(list(simulatedData$votesPartyDpto,simulatedData$randomVotes),by=list(simulatedData$dpto),sum)
	colnames(sumByDpto) = c("dpto","original","random")
	sumByDpto$scalingFactor = sumByDpto$random/sumByDpto$original
	simulatedData = merge(simulatedData,sumByDpto[,c("dpto","scalingFactor")])
	simulatedData$randomVotes = round(simulatedData$randomVotes/simulatedData$scalingFactor)
	simulatedData$votesPartyDpto = simulatedData$randomVotes 
	
	dataVotesTotalByParty2009 = aggregate(votesPartyDpto~party,simulatedData,sum)

	senadoSim2009[i,] = senado(dataVotesTotalByParty2009,30)
	
	auxDiputados = invisible(diputados(simulatedData,regionColumn = "dpto",partyNamesColumn = "party",votesColumn = "votesPartyDpto",externalDeptDistribution = seatsByDpto2009))
	auxDiputados = subset(auxDiputados,assignedSeats>0)
	aggAuxDiputados = aggregate(assignedSeats~dpto,auxDiputados,sum)
	diputadosDptoSim2009[i,] = aggAuxDiputados[,2]
	bancasSim2009[i,] = unlist(sapply(1:nrow(auxDiputados),FUN=function(x) paste(auxDiputados$party[x],auxDiputados$dpto[x],1:auxDiputados$assignedSeats[x],sep="-")))
	
}

save(bancasSim2009,file="simulaciones/simDiputados2009.rdata")

# PRINT BARPLOT DE DIPUTADOS POR DEPARTAMENTO PARA CADA PARTIDO

uniqueDptos = sort(unique(data2009$dpto))
uniqueParties = unique(dataVotesTotalByParty2009$party)

pdf("casos/graficos/simulacionDiputadosDptoPartidos2009.pdf",width=9,height=6)

for (i in seq_along(uniqueDptos)) {
	whichParties = which(sapply(uniqueParties,FUN=function(x) sum(grepl(paste0(x,"-",uniqueDptos[i]),bancasSim2009)))>0)
	nRowChart = ifelse(length(whichParties)>6,3,2)
	nColChart = ifelse(length(whichParties)>4,3,2)
	nCharts = nRowChart*nColChart + 1
	
	layout(matrix(c(rep(1,nColChart),2:nCharts), ncol = nColChart, byrow = TRUE),heights = c(1,rep(8/nRowChart,nRowChart)))
	par(mar=c(1,1,1,1))
	plot(NULL,xlim=c(0,1),ylim=c(0,1),bty="n",xlab="",ylab="",xaxt="n",yaxt="n")
	text(.5,.5,label=toupper(uniqueDptos[i]),cex=2)
	
	for (j in whichParties) {
		tableDptoParty = table(apply(bancasSim2009,1,FUN=function(x) sum(grepl(paste0(uniqueParties[j],"-",uniqueDptos[i]),x))))
		borderVal = rep(NA,length(tableDptoParty))
		colVal = rep(adjustcolor(colsGraph2009[j],alpha.f = .7),length(tableDptoParty))
		
		trueVal = diputados2009$assignedSeats[diputados2009$dpto==uniqueDptos[i]&diputados2009$party==uniqueParties[j]]
		borderVal[which(names(tableDptoParty)==trueVal)] = 1
		colVal[which(names(tableDptoParty)==trueVal)] = colsGraph2009[j]
		
		par(mar=c(5.1,4.1,4.1,2.1))

		barplot(tableDptoParty,col=colVal,border = borderVal,main = uniqueParties[j])
			}
}

dev.off()


library(openxlsx)
library(reshape2)

functionsToSource = list.files(path = "funciones", pattern = "\\.r$", full.names = TRUE)
lapply(functionsToSource, source)

allData2004 = read.xlsx("data/lemaPorDpto2004.xlsx")

data2004 = melt(allData2004[,1:(ncol(allData2004)-1)])

colnames(data2004) = c("dpto","party","votesPartyDpto")
data2004 = data2004[,c("dpto","party","votesPartyDpto")]

data2004$dpto = tolower(iconv(data2004$dpto, from = "UTF-8", to = "ASCII//TRANSLIT"))
data2004$party = gsub("\\."," ",data2004$party)

data2004$party[data2004$party!="Frente Amplio"] = substr(data2004$party[data2004$party!="Frente Amplio"],nchar("Partido ")+1,nchar(data2004$party[data2004$party!="Frente Amplio"]))

dataVotesTotalByParty2004 = aggregate(votesPartyDpto~party,data2004,sum)

# unique(dataVotesTotalByParty2004$party) [1] "Colorado"            "de los Trabajadores" "Frente Amplio"      
#[4] "Independiente"       "Intransingente"      "Liberal"            
#[7] "Nacional"            "Unión Cívica"

colsGraph2004 = c(rgb(1,.2,.2),rgb(.4,.1,.1),rgb(.2,.2,1),rgb(.6,.1,.6),rgb(0,0,0),rgb(255/255,233/255,7/255),rgb(.3,1,1),rgb(7/255,66/255,146/255))
orderGraph2004 = c(5,1,2,3,4,6,7,8)

senado2004 = senado(dataVotesTotalByParty2004,30,printGraph=T,colsGraph = colsGraph2004,orderGraph = orderGraph2004)

# distribucion diputados

seatsByDpto2004 = read.table("data/distributionDiputadosDpto2004.txt",sep="\t",header=T)

diputados2004 = diputados(data2004,regionColumn = "dpto",partyNamesColumn = "party",votesColumn = "votesPartyDpto",externalDeptDistribution = seatsByDpto2004,detail=T) 

save(diputados2004,file="simulaciones/asignacionDiputados2004.rdata") 


### SIMULACION

nSim = 1000
set.seed(598)

senadoSim2004 = matrix(NA,nSim,length(unique(dataVotesTotalByParty2004$party)))
diputadosDptoSim2004 = matrix(NA,nSim,19)
bancasSim2004 = matrix(NA,nSim,99)

for (i in 1:nSim) {
	draw = rnorm(nrow(data2004),sd=.3)
	centeredDraw = numeric(nrow(data2004))
	centeredDraw[draw>=0] = draw[draw>=0] + 1
	centeredDraw[draw<0] = 1/(1 - draw[draw<0])
	simulatedData = data2004
	simulatedData$randomVotes = round(simulatedData$votesPartyDpto*centeredDraw)
	sumByDpto = aggregate(list(simulatedData$votesPartyDpto,simulatedData$randomVotes),by=list(simulatedData$dpto),sum)
	colnames(sumByDpto) = c("dpto","original","random")
	sumByDpto$scalingFactor = sumByDpto$random/sumByDpto$original
	simulatedData = merge(simulatedData,sumByDpto[,c("dpto","scalingFactor")])
	simulatedData$randomVotes = round(simulatedData$randomVotes/simulatedData$scalingFactor)
	simulatedData$votesPartyDpto = simulatedData$randomVotes 
	
	dataVotesTotalByParty2004 = aggregate(votesPartyDpto~party,simulatedData,sum)

	senadoSim2004[i,] = senado(dataVotesTotalByParty2004,30)
	
	auxDiputados = invisible(diputados(simulatedData,regionColumn = "dpto",partyNamesColumn = "party",votesColumn = "votesPartyDpto",externalDeptDistribution = seatsByDpto2004))
	auxDiputados = subset(auxDiputados,assignedSeats>0)
	aggAuxDiputados = aggregate(assignedSeats~dpto,auxDiputados,sum)
	diputadosDptoSim2004[i,] = aggAuxDiputados[,2]
	bancasSim2004[i,] = unlist(sapply(1:nrow(auxDiputados),FUN=function(x) paste(auxDiputados$party[x],auxDiputados$dpto[x],1:auxDiputados$assignedSeats[x],sep="-")))
	
}

save(bancasSim2004,file="simulaciones/simDiputados2004.rdata")


# PRINT BARPLOT DE DIPUTADOS POR DEPARTAMENTO PARA CADA PARTIDO

uniqueDptos = sort(unique(data2004$dpto))
uniqueParties = unique(dataVotesTotalByParty2004$party)

pdf("casos/graficos/simulacionDiputadosDptoPartidos2004.pdf",width=9,height=6)

for (i in seq_along(uniqueDptos)) {
	whichParties = which(sapply(uniqueParties,FUN=function(x) sum(grepl(paste0(x,"-",uniqueDptos[i]),bancasSim2004)))>0)
	nRowChart = ifelse(length(whichParties)>6,3,2)
	nColChart = ifelse(length(whichParties)>4,3,2)
	nCharts = nRowChart*nColChart + 1
	
	layout(matrix(c(rep(1,nColChart),2:nCharts), ncol = nColChart, byrow = TRUE),heights = c(1,rep(8/nRowChart,nRowChart)))
	par(mar=c(1,1,1,1))
	plot(NULL,xlim=c(0,1),ylim=c(0,1),bty="n",xlab="",ylab="",xaxt="n",yaxt="n")
	text(.5,.5,label=toupper(uniqueDptos[i]),cex=2)
	
	for (j in whichParties) {
		tableDptoParty = table(apply(bancasSim2004,1,FUN=function(x) sum(grepl(paste0(uniqueParties[j],"-",uniqueDptos[i]),x))))
		borderVal = rep(NA,length(tableDptoParty))
		colVal = rep(adjustcolor(colsGraph2004[j],alpha.f = .7),length(tableDptoParty))
		
		trueVal = diputados2004$assignedSeats[diputados2004$dpto==uniqueDptos[i]&diputados2004$party==uniqueParties[j]]
		borderVal[which(names(tableDptoParty)==trueVal)] = 1
		colVal[which(names(tableDptoParty)==trueVal)] = colsGraph2004[j]
		
		par(mar=c(5.1,4.1,4.1,2.1))

		barplot(tableDptoParty,col=colVal,border = borderVal,main = uniqueParties[j])
			}
}

dev.off()


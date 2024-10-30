library(openxlsx)
library(reshape2)

functionsToSource = list.files(path = "funciones", pattern = "\\.r$", full.names = TRUE)
lapply(functionsToSource, source)

allData2024 = read.xlsx("data/lemaPorDpto2024_primario.xlsx")

data2024 = melt(allData2024[,1:(ncol(allData2024))])

colnames(data2024) = c("party","dpto","votesPartyDpto")
data2024 = data2024[,c("dpto","party","votesPartyDpto")]

data2024$dpto = tolower(iconv(data2024$dpto, from = "UTF-8", to = "ASCII//TRANSLIT"))
data2024$party = gsub("\\."," ",data2024$party)
data2024$dpto = gsub("\\."," ",data2024$dpto)

data2024$party = substr(data2024$party,nchar("partido ")+1,nchar(data2024$party))

dataVotesTotalByParty2024 = aggregate(votesPartyDpto~party,data2024,sum)


#unique(dataVotesTotalByParty2024$party)
# [1] "Asamblea Popular"                 "Avanzar Republicano"              "Cabildo Abierto"                  "Colorado"                         "Constitucional Ambientalista"     "Ecologista Radical Intransigente"
# [7] "Frente Amplio"                    "Identidad Soberana"               "Independiente"                    "Nacional"                         "Por Los Cambios Necesarios (PCN)"

colsGraph2024 = c(rgb(.5,.1,.1),rgb(255,129,0,maxColorValue = 255),rgb(.9,.9,.05),rgb(1,.2,.2),rgb(.2,.9,.2),rgb(.4,.9,.4),rgb(.2,.2,1),rgb(0,48,254,maxColorValue = 255),rgb(.6,.1,.6),rgb(.3,1,1),rgb(.5,1,.5))
orderGraph2024 = c(1,6,10,5,8,4,2,11,3,7,9)

senado2024 = senado(dataVotesTotalByParty2024,30,printGraph=T,colsGraph = colsGraph2024,orderGraph = orderGraph2024)

# distribucion diputados

seatsByDpto2024 = read.table("data/distributionDiputadosDpto2024.txt",sep="\t",header=T)

diputados2024 = diputados(data2024,regionColumn = "dpto",partyNamesColumn = "party",votesColumn = "votesPartyDpto",externalDeptDistribution = seatsByDpto2024,detail=T)

save(diputados2024,file="simulaciones/asignacionDiputados2024.rdata") 

### SIMULACION

nSim = 1000
set.seed(598)

senadoSim2024 = matrix(NA,nSim,length(unique(dataVotesTotalByParty2024$party)))
diputadosDptoSim2024 = matrix(NA,nSim,19)
bancasSim2024 = matrix(NA,nSim,99)

for (i in 1:nSim) {
	draw = rnorm(nrow(data2024),sd=.3)
	centeredDraw = numeric(nrow(data2024))
	centeredDraw[draw>=0] = draw[draw>=0] + 1
	centeredDraw[draw<0] = 1/(1 - draw[draw<0])
	simulatedData = data2024
	simulatedData$randomVotes = round(simulatedData$votesPartyDpto*centeredDraw)
	sumByDpto = aggregate(list(simulatedData$votesPartyDpto,simulatedData$randomVotes),by=list(simulatedData$dpto),sum)
	colnames(sumByDpto) = c("dpto","original","random")
	sumByDpto$scalingFactor = sumByDpto$random/sumByDpto$original
	simulatedData = merge(simulatedData,sumByDpto[,c("dpto","scalingFactor")])
	simulatedData$randomVotes = round(simulatedData$randomVotes/simulatedData$scalingFactor)
	simulatedData$votesPartyDpto = simulatedData$randomVotes 
	
	dataVotesTotalByParty2024 = aggregate(votesPartyDpto~party,simulatedData,sum)

	senadoSim2024[i,] = senado(dataVotesTotalByParty2024,30)
	
	auxDiputados = invisible(diputados(simulatedData,regionColumn = "dpto",partyNamesColumn = "party",votesColumn = "votesPartyDpto",externalDeptDistribution = seatsByDpto2024))
	auxDiputados = subset(auxDiputados,assignedSeats>0)
	aggAuxDiputados = aggregate(assignedSeats~dpto,auxDiputados,sum)
	diputadosDptoSim2024[i,] = aggAuxDiputados[,2]
	bancasSim2024[i,] = unlist(sapply(1:nrow(auxDiputados),FUN=function(x) paste(auxDiputados$party[x],auxDiputados$dpto[x],1:auxDiputados$assignedSeats[x],sep="-")))
	
}

save(bancasSim2024,file="simulaciones/simDiputados2024.rdata")

# PRINT BARPLOT DE DIPUTADOS POR DEPARTAMENTO PARA CADA PARTIDO

uniqueDptos = sort(unique(data2024$dpto))
uniqueParties = unique(dataVotesTotalByParty2024$party)

pdf("casos/graficos/simulacionDiputadosDptoPartidos2024.pdf",width=9,height=6)

for (i in seq_along(uniqueDptos)) {
	whichParties = which(sapply(uniqueParties,FUN=function(x) sum(grepl(paste0(x,"-",uniqueDptos[i]),bancasSim2024)))>0)
	nRowChart = ifelse(length(whichParties)>6,3,2)
	nColChart = ifelse(length(whichParties)>4,3,2)
	nCharts = nRowChart*nColChart + 1
	
	layout(matrix(c(rep(1,nColChart),2:nCharts), ncol = nColChart, byrow = TRUE),heights = c(1,rep(8/nRowChart,nRowChart)))
	par(mar=c(1,1,1,1))
	plot(NULL,xlim=c(0,1),ylim=c(0,1),bty="n",xlab="",ylab="",xaxt="n",yaxt="n")
	text(.5,.5,label=toupper(uniqueDptos[i]),cex=2)
	
	for (j in whichParties) {
		tableDptoParty = table(apply(bancasSim2024,1,FUN=function(x) sum(grepl(paste0(uniqueParties[j],"-",uniqueDptos[i]),x))))
		borderVal = rep(NA,length(tableDptoParty))
		colVal = rep(adjustcolor(colsGraph2024[j],alpha.f = .7),length(tableDptoParty))
		
		trueVal = diputados2024$assignedSeats[diputados2024$dpto==uniqueDptos[i]&diputados2024$party==uniqueParties[j]]
		borderVal[which(names(tableDptoParty)==trueVal)] = 1
		colVal[which(names(tableDptoParty)==trueVal)] = colsGraph2024[j]
		
		par(mar=c(5.1,4.1,4.1,2.1))

		barplot(tableDptoParty,col=colVal,border = borderVal,main = uniqueParties[j])
			}
}

dev.off()


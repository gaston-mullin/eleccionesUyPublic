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
data2019melted$party = gsub("\\."," ",data2019melted$party)
data2019melted$party = substr(data2019melted$party,nchar("Partido ")+1,nchar(data2019melted$party))

dataByDepto2019 = aggregate(votesPartyDpto~dpto + party,data2019melted,sum)
dataVotesTotalByParty2019 = aggregate(votesPartyDpto~party,dataByDepto2019,sum)

# distribucion senado

# unique(dataVotesTotalByParty2019$party)
#  [1] "Asamblea Popular"                 "Cabildo Abierto"                 
#  [3] "Colorado"                         "de la Gente"                     
#  [5] "de los Trabajadores"              "Digital"                         
#  [7] "Ecologista Radical Intransigente" "Frente Amplio"                   
#  [9] "Independiente"                    "Nacional"                        
# [11] "Verde Animalista" 


colsGraph2019 = c(rgb(.5,.1,.1),rgb(.9,.9,.05),rgb(1,.2,.2),rgb(.2,.9,.2),NA,NA,rgb(.4,.9,.4),rgb(.2,.2,1),rgb(.6,.1,.6),rgb(.3,1,1),rgb(.5,1,.5))
orderGraph2019 = c(2,11,8,10,1,6,5,3,7,9,4)

senado2019 = senado(dataVotesTotalByParty2019,30,printGraph=T,colsGraph = colsGraph2019,orderGraph = orderGraph2019)

# distribucion diputados

seatsByDpto2019 = read.table("data/distributionDiputadosDpto2019.txt",sep="\t",header=T)

diputados2019 = diputados(dataByDepto2019,regionColumn = "dpto",partyNamesColumn = "party",votesColumn = "votesPartyDpto",externalDeptDistribution = seatsByDpto2019,detail=T) 

save(diputados2019,file="simulaciones/asignacionDiputados2019.rdata") 


### SIMULACION

nSim = 1000
set.seed(598)

senadoSim2019 = matrix(NA,nSim,11)
diputadosDptoSim2019 = matrix(NA,nSim,19)
bancasSim2019 = matrix(NA,nSim,99)

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
	
	dataVotesTotalByParty2019 = aggregate(votesPartyDpto~party,simulatedData,sum)

	senadoSim2019[i,] = senado(dataVotesTotalByParty2019,30)
	
	auxDiputados = invisible(diputados(simulatedData,regionColumn = "dpto",partyNamesColumn = "party",votesColumn = "votesPartyDpto",externalDeptDistribution = seatsByDpto2019))
	auxDiputados = subset(auxDiputados,assignedSeats>0)
	aggAuxDiputados = aggregate(assignedSeats~dpto,auxDiputados,sum)
	diputadosDptoSim2019[i,] = aggAuxDiputados[,2]
	bancasSim2019[i,] = unlist(sapply(1:nrow(auxDiputados),FUN=function(x) paste(auxDiputados$party[x],auxDiputados$dpto[x],1:auxDiputados$assignedSeats[x],sep="-")))
	
}

save(bancasSim2019,file="simulaciones/simDiputados2019.rdata")

# PRINT BARPLOT DE DIPUTADOS POR DEPARTAMENTO PARA CADA PARTIDO

uniqueDptos = unique(dataByDepto2019$dpto)
uniqueParties = unique(dataByDepto2019$party)

pdf("casos/graficos/simulacionDiputadosDptoPartidos2019.pdf",width=9,height=6)

for (i in seq_along(uniqueDptos)) {
	whichParties = which(sapply(uniqueParties,FUN=function(x) sum(grepl(paste0(x,"-",uniqueDptos[i]),bancasSim2019)))>0)
	nRowChart = ifelse(length(whichParties)>6,3,2)
	nColChart = ifelse(length(whichParties)>4,3,2)
	nCharts = nRowChart*nColChart + 1
	
	layout(matrix(c(rep(1,nColChart),2:nCharts), ncol = nColChart, byrow = TRUE),heights = c(1,rep(8/nRowChart,nRowChart)))
	par(mar=c(1,1,1,1))
	plot(NULL,xlim=c(0,1),ylim=c(0,1),bty="n",xlab="",ylab="",xaxt="n",yaxt="n")
	text(.5,.5,label=toupper(uniqueDptos[i]),cex=2)
	
	for (j in whichParties) {
		tableDptoParty = table(apply(bancasSim2019,1,FUN=function(x) sum(grepl(paste0(uniqueParties[j],"-",uniqueDptos[i]),x))))
		borderVal = rep(NA,length(tableDptoParty))
		colVal = rep(adjustcolor(colsGraph2019[j],alpha.f = .7),length(tableDptoParty))
		
		trueVal = diputados2019$assignedSeats[diputados2019$dpto==uniqueDptos[i]&diputados2019$party==uniqueParties[j]]
		borderVal[which(names(tableDptoParty)==trueVal)] = 1
		colVal[which(names(tableDptoParty)==trueVal)] = colsGraph2019[j]
		
		par(mar=c(5.1,4.1,4.1,2.1))

		barplot(tableDptoParty,col=colVal,border = borderVal,main = uniqueParties[j])
			}
}

dev.off()



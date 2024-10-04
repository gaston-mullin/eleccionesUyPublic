library(jsonlite)
library(reshape2)

functionsToSource = list.files(path = "funciones", pattern = "\\.r$", full.names = TRUE)
lapply(functionsToSource, source)

allData2014 = fromJSON("data/ResumenGeneral_D_DPTOS2014.json")
listData2014 = allData2014$EleccionNacional

data2014 = listData2014[[1]][,c("LemaNombre","Total")]
data2014$dpto = tolower(iconv(allData2014$DepNombre[1], from = "UTF-8", to = "ASCII//TRANSLIT"))

for (i in 2:nrow(allData2014)) {
	auxData = as.data.frame(listData2014[[i]])[,c("LemaNombre","Total")]
	auxData$dpto = tolower(iconv(allData2014$DepNombre[i], from = "UTF-8", to = "ASCII//TRANSLIT"))
	rownames(auxData) = (1:nrow(auxData))+nrow(data2014)
	data2014 = rbind(data2014,auxData)

}

colnames(data2014) = c("party","votesPartyDpto","dpto")
data2014 = data2014[,c("dpto","party","votesPartyDpto")]

data2014$party[data2014$party!="Frente Amplio"] = substr(data2014$party[data2014$party!="Frente Amplio"],nchar("Partido ")+1,nchar(data2014$party[data2014$party!="Frente Amplio"]))

dataVotesTotalByParty2014 = aggregate(votesPartyDpto~party,data2014,sum)

# unique(dataVotesTotalByParty2014$party)
#[1] "Asamblea Popular"                 "Colorado"                         "de los Trabajadores"             
#[4] "Ecologista Radical Intransigente" "Frente Amplio"                    "Independiente"                   
#[7] "Nacional" 


colsGraph2014 = c(rgb(.5,.1,.1),rgb(1,.2,.2),rgb(.4,.1,.1),rgb(.4,.9,.4),rgb(.2,.2,1),rgb(.6,.1,.6),rgb(.3,1,1))
orderGraph2014 = c(2,6,1,4,3,5,7)

senado2014 = senado(dataVotesTotalByParty2014,30,printGraph=T,colsGraph = colsGraph2014,orderGraph = orderGraph2014)

# distribucion diputados

seatsByDpto2014 = read.table("data/distributionDiputadosDpto2014.txt",sep="\t",header=T)

diputados2014 = diputados(data2014,regionColumn = "dpto",partyNamesColumn = "party",votesColumn = "votesPartyDpto",externalDeptDistribution = seatsByDpto2014,detail=T) 

save(diputados2014,file="simulaciones/asignacionDiputados2014.rdata") 

### SIMULACION

nSim = 1000
set.seed(598)

senadoSim2014 = matrix(NA,nSim,7)
diputadosDptoSim2014 = matrix(NA,nSim,19)
bancasSim2014 = matrix(NA,nSim,99)

for (i in 1:nSim) {
	draw = rnorm(nrow(data2014),sd=.3)
	centeredDraw = numeric(nrow(data2014))
	centeredDraw[draw>=0] = draw[draw>=0] + 1
	centeredDraw[draw<0] = 1/(1 - draw[draw<0])
	simulatedData = data2014
	simulatedData$randomVotes = round(simulatedData$votesPartyDpto*centeredDraw)
	sumByDpto = aggregate(list(simulatedData$votesPartyDpto,simulatedData$randomVotes),by=list(simulatedData$dpto),sum)
	colnames(sumByDpto) = c("dpto","original","random")
	sumByDpto$scalingFactor = sumByDpto$random/sumByDpto$original
	simulatedData = merge(simulatedData,sumByDpto[,c("dpto","scalingFactor")])
	simulatedData$randomVotes = round(simulatedData$randomVotes/simulatedData$scalingFactor)
	simulatedData$votesPartyDpto = simulatedData$randomVotes 
	
	dataVotesTotalByParty2014 = aggregate(votesPartyDpto~party,simulatedData,sum)

	senadoSim2014[i,] = senado(dataVotesTotalByParty2014,30)
	
	auxDiputados = invisible(diputados(simulatedData,regionColumn = "dpto",partyNamesColumn = "party",votesColumn = "votesPartyDpto",externalDeptDistribution = seatsByDpto2014))
	auxDiputados = subset(auxDiputados,assignedSeats>0)
	aggAuxDiputados = aggregate(assignedSeats~dpto,auxDiputados,sum)
	diputadosDptoSim2014[i,] = aggAuxDiputados[,2]
	bancasSim2014[i,] = unlist(sapply(1:nrow(auxDiputados),FUN=function(x) paste(auxDiputados$party[x],auxDiputados$dpto[x],1:auxDiputados$assignedSeats[x],sep="-")))
	
}

save(bancasSim2014,file="simulaciones/simDiputados2014.rdata")


# PRINT BARPLOT DE DIPUTADOS POR DEPARTAMENTO PARA CADA PARTIDO

uniqueDptos = unique(data2014$dpto)
uniqueParties = unique(dataVotesTotalByParty2014$party)

pdf("casos/graficos/simulacionDiputadosDptoPartidos2014.pdf",width=9,height=6)

for (i in seq_along(uniqueDptos)) {
	whichParties = which(sapply(uniqueParties,FUN=function(x) sum(grepl(paste0(x,"-",uniqueDptos[i]),bancasSim2014)))>0)
	nRowChart = ifelse(length(whichParties)>6,3,2)
	nColChart = ifelse(length(whichParties)>4,3,2)
	nCharts = nRowChart*nColChart + 1
	
	layout(matrix(c(rep(1,nColChart),2:nCharts), ncol = nColChart, byrow = TRUE),heights = c(1,rep(8/nRowChart,nRowChart)))
	par(mar=c(1,1,1,1))
	plot(NULL,xlim=c(0,1),ylim=c(0,1),bty="n",xlab="",ylab="",xaxt="n",yaxt="n")
	text(.5,.5,label=toupper(uniqueDptos[i]),cex=2)
	
	for (j in whichParties) {
		tableDptoParty = table(apply(bancasSim2014,1,FUN=function(x) sum(grepl(paste0(uniqueParties[j],"-",uniqueDptos[i]),x))))
		borderVal = rep(NA,length(tableDptoParty))
		colVal = rep(adjustcolor(colsGraph2014[j],alpha.f = .7),length(tableDptoParty))
		
		trueVal = diputados2014$assignedSeats[diputados2014$dpto==uniqueDptos[i]&diputados2014$party==uniqueParties[j]]
		borderVal[which(names(tableDptoParty)==trueVal)] = 1
		colVal[which(names(tableDptoParty)==trueVal)] = colsGraph2014[j]
		
		par(mar=c(5.1,4.1,4.1,2.1))

		barplot(tableDptoParty,col=colVal,border = borderVal,main = uniqueParties[j])
			}
}

dev.off()


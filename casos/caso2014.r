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


# distribucion senado

dataVotesTotalByParty = aggregate(votesPartyDpto~party,data2014,sum)
senado(dataVotesTotalByParty,30)

# distribucion diputados

seatsByDpto = read.table("data/distributionDiputadosDpto2014.txt",sep="\t",header=T)

tcd = diputados(data2014,regionColumn = "dpto",partyNamesColumn = "party",votesColumn = "votesPartyDpto",externalDeptDistribution = seatsByDpto) 

subset(tcd,party=="Partido.Colorado")

## ONE CHANGE NEEDED (SEE PARTIDO COLORADO SALTO -> SORIANO)

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

senado(dataVotesTotalByParty,30)

# distribucion diputados

seatsByDpto = read.table("data/distributionDiputadosDpto2019.txt",sep="\t",header=T)

tcd = diputados(dataByDepto2019,regionColumn = "dpto",partyNamesColumn = "party",votesColumn = "votesPartyDpto",externalDeptDistribution = seatsByDpto) 

subset(tcd,party=="Partido.Colorado")

## ONE CHANGE NEEDED (SEE PARTIDO COLORADO SALTO -> SORIANO)

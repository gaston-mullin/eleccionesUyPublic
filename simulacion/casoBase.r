
dataSimple = data.frame(partidos = LETTERS[1:3],votos = (1:3)*1000)
senado(dataSimple,30,printGraph= T)

dataVotes = data.frame(partidos = sample(LETTERS,6),votos = round(runif(6,1000,5000)))
senado(dataVotes,30,printGraph= T)

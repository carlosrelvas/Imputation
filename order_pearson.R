## Function that will receive pearson correlations and will return the variables   ##
## that have the biggest pearson correlation in absolute value with each variable. ##

## Reading data from hive ##

f <- file(description="stdin")
data <- read.table(f,sep="\t",header=F,stringsAsFactors=F,na.strings = "\\N")

## Parameters: "arq" is the name of the file that we want to generate and "qtd" is ##
## the number of the top correlations.                                             ##

args <- commandArgs(TRUE)
qtd <- as.numeric(args[1])
arq <- as.character(args[2])

linhas = unique(c(unique(data[,1]), unique(data[,2])))
saida = matrix("a", length(linhas), 2)
cont=1

for(j in linhas){
  id1 = which(data[,1]==j)
  id2 = which(data[,2]==j)
  data.aux = data[c(id1,id2),]
  data.aux[,3] = abs(data.aux[,3])
  data.aux = data.aux[order(-data.aux[,3]),]
  data.aux = data.aux[1:qtd,]
  colunas = c(data.aux[,1], data.aux[,2])
  colunas = colunas[!(colunas == j)]
  saida[cont,] = c(j, paste0(colunas, collapse="-"))
  cont=cont+1
}

write.table(saida, file=arq, sep="\t", row.names=F, col.names=F)
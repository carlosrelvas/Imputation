## Mapper function to impute the missing variables by a GAM model ##

## Reading data from hive ##

f <- file(description="stdin")
data <- read.table(f,sep="\t",header=F,stringsAsFactors=F,na.strings = "\\N")
for(i in 1:ncol(data)){
  data[,i] = as.numeric(data[,i])
}

## Parameters: "arq" is the name of the file that we read the variable to use in the model ##

args <- commandArgs(TRUE)
arq <- as.character(args[1])

order = read.table(arq, sep="\t",header=F,stringsAsFactors=F,na.strings = "\\N")

## We just send to reducers the variables that will be used in the model ##

for(i in order[,1]){
  vars = as.numeric(strsplit(order[order[,1]==i,2], "-")[[1]])
  write.table(cbind(i, data[,c(as.numeric(i), vars)]),
              stdout(),row.names=F,col.names=F,quote=F,sep="\t")
}

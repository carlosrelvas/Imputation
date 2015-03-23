## Mapper function to calculate all the pairwise Pearson Correlations ##

## Reading data from hive ##

f <- file(description="stdin")
data <- read.table(f,sep="\t",header=F,stringsAsFactors=F,na.strings = "\\N")

## Transforming all the variables in numeric ##

for(i in 1:ncol(data)){
  data[,i] = as.numeric(data[,i])
}

## Constructing all the pairs between the variables ##

comb = expand.grid(1:ncol(data), 1:ncol(data))
comb = comb[comb[,1]<comb[,2],]

## For each par, we send to the reducer function:                         ##
## - Summation of the two variables ignoring missing values               ##
## - Summation of the squared two variables ignoring missing values       ##
## - Summation of the mult. of thetwo variables ignoring missing values   ##
## - Number of observation ignoring the missing values                    ##

for(i in 1:nrow(comb)){
  key = paste0(comb[i,1], "-", comb[i,2])
  data.aux = na.omit(data[,c(comb[i,1], comb[i,2])])
  sum.x = sum(data.aux[,1])
  sum.y = sum(data.aux[,2])
  sum.x2 = sum(data.aux[,1]^2)
  sum.y2 = sum(data.aux[,2]^2)
  sum.xy = sum(data.aux[,1]*data.aux[,2])
  n = nrow(data.aux)
  write.table(cbind(key, sum.x, sum.y, sum.x2, sum.y2, sum.xy, n),
              stdout(),row.names=F,col.names=F,quote=F,sep="\t")
}




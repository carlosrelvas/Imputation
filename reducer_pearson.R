## Reducer function to calculate all the pairwise Pearson Correlations ##

## Reading data from hive ##

f <- file(description="stdin")
data <- read.table(f,sep="\t",header=F,stringsAsFactors=F,na.strings = "\\N")

## Aggregation of the summations of all mappers and calculating pearson correlation ##

for(i in unique(data[,1])){
  sum.x = sum(data[data[,1]==i,2])
  sum.y = sum(data[data[,1]==i,3])
  sum.x2 = sum(data[data[,1]==i,4])
  sum.y2 = sum(data[data[,1]==i,5])
  sum.xy = sum(data[data[,1]==i,6])
  n = sum(data[data[,1]==i,7])
  pearson = (n*sum.xy-sum.x*sum.y)/
    (sqrt(n*sum.x2-sum.x^2)*sqrt(n*sum.y2-sum.y^2))
  var1 = strsplit(i, "-")[[1]][1]
  var2 = strsplit(i, "-")[[1]][2]
  write.table(cbind(var1, var2, pearson),
              stdout(),row.names=F,col.names=F,quote=F,sep="\t")
}
## Reducer function to impute the missing variables by a GAM model ##

## Reading data from hive ##

f <- file(description="stdin")
data <- read.table(f,sep="\t",header=F,stringsAsFactors=F,na.strings = "\\N")
for(i in 1:ncol(data)){
  data[,i] = as.numeric(data[,i])
}

## Construction auxiliary variable names ##

qtd <- ncol(data)-1
names(data) = c("key",paste0("x",1:qtd))
result=NULL

## Read GAM package ##

library(gam)

## For each variable with missing values, we generated a gam model and replace the missing ##
## with the fitted values. ##

for(i in 1:max(data[,1])){
  data.aux = data[data[,1]==i,]
  if(any(is.na(data.aux[,"x1"]))){
    data.aux2 = data.aux[!is.na(data.aux[,"x1"]),]
    formula = paste0("x1 ~ ", paste0("s(x", 2:qtd, ",3)", collapse="+"))
    fit = gam(as.formula(formula), data=data.aux2,  na.action = na.gam.replace)
    fit.pred = predict(fit, newdata=data.aux)
  }
  fitted = ifelse(is.na(data.aux[,"x1"]), fit.pred, data.aux[,"x1"])
  aux = cbind(rep(i, length(fitted)), fitted)
  result = rbind(result, aux)
}

write.table(result, stdout(),row.names=F,col.names=F,quote=F,sep="\t")
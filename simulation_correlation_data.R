## Number of variables that we want to generate ##

num.vars = 10

## Generating the vector of means to be used in the simulation ##

set.seed(233)
mu = rnorm(num.vars)

## Generating a positive matrix to be used in the multivariate normal simulation ##

S <- toeplitz((10:1)/10)
set.seed(11)
R <- rWishart(1, 10, S)
sigma = matrix(R,10,10)

library(MASS)
data = mvrnorm(1000, mu, sigma)

write.table(data, sep="\t", row.names=F, col.names=F, 
            file="imputation_data_nomiss.txt")

set.seed(2532)
for(j in 1:ncol(data)){
  aux = runif(1)
  if(aux > 0.3){
    perc.missing = runif(1,0,0.2)
    id = sample(1:nrow(data), floor(perc.missing*nrow(data)), 
                replace=F)
    data[id,j]=NA
  }
}

write.table(data, sep="\t", row.names=F, col.names=F, 
            file="imputation_data.txt")

cor(data, use="pairwise.complete.obs")

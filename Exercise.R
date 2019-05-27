# required packages:
# install.packages("Rcpp")
# install.packages("RcppArmadillo")

## For tensorflow, you need anaconda 
# install.packages("tensorflow")
# tensorflow::install_tensorflow()

# euclidean distance

# Distance Matrix O(n*n)
n = 1000
var = 10
X = matrix(runif(n*var), n, var)

getTime = function(X, func, ...) return(system.time({func(X,...)}))


# Bad and slow R code
simple_R_distance_bad = function(X){
  results = NULL
  counter = 1
  for(i in 1:nrow(X)){
    tmp = NULL
    for(j in 1:nrow(X)){
      tmp[j] = sqrt(sum((X[i,] - X[j,])^2))
    }
    results = rbind(results, matrix(tmp, nrow = 1))
    
  }
  return(results)
}
n = 1000
var = 10
X = matrix(runif(n*var), n, var)

getTime(X, dist)
getTime(X, simple_R_distance_bad)

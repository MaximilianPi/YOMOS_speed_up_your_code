# Distance Matrix O(n*n)
n = 1000
var = 10
X = matrix(runif(n*var), n, var)

simple_R_distance = function(X){
  results = matrix(NA, nrow(X), nrow(X))
  for(i in 1:nrow(X)){
    for(j in i:nrow(X))
      results[j,i] = sqrt(sum((X[i,] - X[j,])^2))
  }
  return(results)
}

improved_R_distance = function(X){
  results = 
  sapply(1:nrow(X), function(i){  
    results = 
      sapply(i:nrow(X), function(j) sqrt(sum((X[i,] - X[j,])^2)))
    return(results)
  }, simplify = TRUE)
  return(results)
}




Sys.getenv()
Sys.setenv(PKG_CXXFLAGS = "-fopenmp -lgomp -std=c++14")

Rcpp::sourceCpp(file = "Dist.cpp",showOutput = TRUE,verbose = TRUE, rebuild = TRUE)

Rcpp::sourceCpp(file = "eigen.cpp",showOutput = TRUE,verbose = TRUE, rebuild = TRUE)
res = eigen_Dist(X)
res2 = Dist_cpp(X)

getTime = function(X, func, ...) return(system.time({func(X,...)}))

n = 5000
var = 500
X = matrix(runif(n*var), n, var)
getTime(X, simple_R_distance)
getTime(X, improved_R_distance)
getTime(X, Dist_cpp, n_threads = 4L)
getTime(X, eigen_Dist)


res2 = Dist_cpp(X, n_threads = 4L)


res2 = Dist_cpp(X, 4)
res = D$numpy()
(res[1:5,1:5])
res2[1:5, 1:5]



# library(tensorflow)
# tf$VERSION
# 
# tf$enable_eager_execution()
# 
# 
# X = matrix(runif(1e3*1e2), 1e3, 1e2)
# 
# 
# system.time({d = dist(X)})
# 
# 
library(tensorflow)
system.time({
  A = tf$constant(X)
  r = tf$reduce_sum(tf$multiply(A, A), 1L)
  r = tf$reshape(r, c(-1L, 1L))
  D = r - 2.0*tf$matmul(A, tf$transpose(A)) + tf$transpose(r)
  D = tf$sqrt(D)
})

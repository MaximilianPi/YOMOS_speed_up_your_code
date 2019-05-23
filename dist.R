# required packages:
# install.packages("Rcpp")
# install.packages("RcppArmadillo")

## For tensorflow, you need anaconda 
# install.packages("tensorflow")
# tensorflow::install_tensorflow()



# Distance Matrix O(n*n)
n = 1000
var = 10
X = matrix(runif(n*var), n, var)

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

# fast vanilla R
simple_R_distance = function(X){
  n_row = nrow(X)
  
  results = matrix(NA, n_row, n_row)
  for(i in 1:n_row){
    for(j in i:n_row)
      results[j,i] = sqrt(sum((X[i,] - X[j,])^2))
  }
  return(results)
}



library(tensorflow)
TF_distance = function(X){
  A = tf$constant(X)
  r = tf$reduce_sum(tf$multiply(A, A), 1L)
  r = tf$reshape(r, c(-1L, 1L))
  D = r - 2.0*tf$matmul(A, tf$transpose(A)) + tf$transpose(r)
  D = tf$sqrt(D)
  return(D$numpy())
}




getTime = function(X, func, ...) return(system.time({func(X,...)}))




n = 5000
var = 50
X = matrix(runif(n*var), n, var)
getTime(X, simple_R_distance_bad)
getTime(X, simple_R_distance, cl)


Sys.getenv()
Sys.setenv(PKG_CXXFLAGS = "-fopenmp -lgomp -std=c++14")
Rcpp::sourceCpp(file = "vanilla.cpp",showOutput = TRUE,verbose = TRUE, rebuild = TRUE)


Sys.setenv(PKG_CXXFLAGS = "-std=c++14")
Rcpp::sourceCpp(file = "arma.cpp",showOutput = TRUE,verbose = TRUE, rebuild = TRUE)
res = eigen_Dist(X)
res2 = Dist_cpp(X)


results = matrix(NA, nrow = 6, ncol = length(seq(1000, 1e4, by = 1000)))

counter = 1
for(i in seq(100, by = 100, length.out = 10L)){
  n = i
  var = floor(0.5*i)
  X = matrix(runif(n*var), n, var)
  results[1, counter] = getTime(X, simple_R_distance)[3]
  results[3, counter] = getTime(X, simple_R_distance_bad)[3]
  results[4, counter] = getTime(X, vanilla, n_threads = 1L)[3]
  results[5, counter] = getTime(X, vanilla, n_threads = 4L)[3]
  results[6, counter] = getTime(X, arma)[3]
  results[7, counter] = getTime(X, TF_distance)[3]
  counter = counter+1
}








#include <Rcpp.h>
#include <math.h>
#include <omp.h>
// [[Rcpp::plugins(openmp)]]
using namespace Rcpp;
// [[Rcpp::export]]

NumericMatrix Dist_cpp(NumericMatrix X, int n_threads = 4) {
  int n_row = X.rows();
  int n_col = X.cols();
  double tmp, tmp2;
  
  NumericMatrix Results(n_row, n_row);
  
  for(int i = 0; i < n_row; i++){
    for(int j = 0; j < n_row; j++)
      Results(i,j) = 0;
  }
  omp_set_dynamic(0);    
  omp_set_num_threads(n_threads);
  #pragma omp parallel for
  for(int i = 0; i < n_row; i++){
    for(int j = i; j < n_row; j++){
      tmp = 0;
      for(int c = 0; c < n_col; c++){
        tmp2 = X(i,c) - X(j,c);
        tmp2 *= tmp2;
        tmp+=tmp2;
      }
      Results(j,i) = sqrt(tmp);
    }
  }
  
  return Results;
}
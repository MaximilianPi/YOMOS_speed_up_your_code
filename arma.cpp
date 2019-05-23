#include <RcppArmadillo.h>
#include <math.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace arma;
// [[Rcpp::export]]

arma::mat arma(arma::mat M){
  arma::mat row_sum(M.n_rows, 1);
  arma::mat MM = M;
  MM.for_each([](auto &val) {val*=val;});

  arma::vec tmp = arma::sum(MM, 1);
  for(unsigned int i = 0; i<M.n_rows;i++) row_sum(i,0) = tmp.at(i);
  arma::mat Results(M.n_rows, M.n_rows);
  Results =  - 2.0 * (M*M.t()); 
  for (unsigned int i = 0; i < M.n_rows; i++){
    Results.row(i).for_each([&](auto &val){val+=tmp.at(i);});
    Results.row(i)+=row_sum.t();
  }
  Results.for_each([](auto &val){val = sqrt(val);});
  return Results;
}
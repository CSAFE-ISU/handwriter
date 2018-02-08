// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
using namespace arma;

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

void countBNeighbors(const arma::mat &img, uvec checkList, vec &toWhite);
void countWBTransitions(const arma::mat &img, uvec checkList, vec &toWhite);


// [[Rcpp::export]]
arma::mat thinImage_c(arma::mat img) {
  int n = img.n_rows;
  int m = img.n_cols;
  
  uvec black = find(img == 0);
  vec toWhite = zeros<vec>(black.n_elem);
  
  // Step 1
  countWBTransitions(img, black, toWhite);
  black = black.elem(find(toWhite == 0));
  toWhite = zeros<vec>(black.n_elem);
  
  
  
  
  Rcpp::Rcout << n << m;
  return(img);
}

void countBNeighbors(const arma::mat &img, uvec checkList, vec &toWhite)
{
  
}
void countWBTransitions(const arma::mat &img, uvec checkList, vec &toWhite)
{
  vec neighborInd = zeros<vec>(9);
  int n = img.n_rows;
  //int m = img.n_cols;
  int cell;
  int counter;
  for(int i = 0; i < checkList.n_elem; i++)
  {
    cell = checkList[i];
    neighborInd << cell - 1 << cell + n - 1 << cell + n << cell + n + 1 << cell + 1 
                << cell - n + 1 << cell - n << cell - n - 1 << cell - 1;
    counter = 0;
    for(int j = 0; j < 8; j++)
    {
      if(img[checkList[neighborInd[j]]] == 1 && img[checkList[neighborInd[j+1]]] == 0)
      {
        counter++;
      }
    }
    if(counter == 1)
      toWhite[i] = 1;
  }
}
// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//



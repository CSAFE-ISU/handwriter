// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
using namespace arma;

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

void countBNeighbors(const arma::mat &img, uvec checkList, vec &toWhite);
void countWBTransitions(const arma::mat &img, uvec checkList, vec &toWhite);
void count246(const arma::mat &img, uvec checkList, vec &toWhite);
void count468(const arma::mat &img, uvec checkList, vec &toWhite);
void count248(const arma::mat &img, uvec checkList, vec &toWhite);
void count268(const arma::mat &img, uvec checkList, vec &toWhite);

//' thinImage
//' 
//' This function returns a vector of locations for black pixels in the thinned image.
//' Thinning done using Zhang - Suen algorithm.
//'
//' @param img A binary matrix of the text that is to be thinned.
//' @export
// [[Rcpp::export]]
arma::uvec thinImage(arma::mat img) {
  
  uvec black;
  vec toWhite;
  
  bool changed = true;
  int startingNumBlack = 0;
 // int oldnum= 0;
  
  while(changed)
  {
    black = find(img == 0);
    toWhite = zeros<vec>(black.n_elem);
    
    startingNumBlack = black.n_elem;
    
    // Step 1

    countWBTransitions(img, black, toWhite);
    black = black.elem(find(toWhite == 1));
    toWhite = zeros<vec>(black.n_elem);
    
    countBNeighbors(img, black, toWhite);
    black = black.elem(find(toWhite == 1));
    toWhite = zeros<vec>(black.n_elem);
    
    count246(img, black, toWhite);
    black = black.elem(find(toWhite == 1));
    toWhite = zeros<vec>(black.n_elem);
    
    count468(img, black, toWhite);
    black = black.elem(find(toWhite == 1));

    img.elem(black) = ones<vec>(black.n_elem);

    // Step 2
    black = find(img == 0);
    toWhite = zeros<vec>(black.n_elem);

    countWBTransitions(img, black, toWhite);
    black = black.elem(find(toWhite == 1));
    toWhite = zeros<vec>(black.n_elem);
    
    countBNeighbors(img, black, toWhite);
    black = black.elem(find(toWhite == 1));
    toWhite = zeros<vec>(black.n_elem);
    
    count248(img, black, toWhite);
    black = black.elem(find(toWhite == 1));
    toWhite = zeros<vec>(black.n_elem);

    count268(img, black, toWhite);
    black = black.elem(find(toWhite == 1));

    img.elem(black) = ones<vec>(black.n_elem);
    black = find(img == 0);
    
    if(startingNumBlack == black.n_elem)
    {
      changed = false;
    }
  }
  
  return(black + 1);
}

void count246(const arma::mat &img, uvec checkList, vec &toWhite)
{
  vec neighbors = zeros<vec>(3);
  int n = img.n_rows;
  int cell;
  for(int i = 0; i < checkList.n_elem; i++)
  {
    cell = checkList[i];
    neighbors << img[cell - 1] << img[cell + n] << img[cell + 1];
    if(any(neighbors == 1))
      toWhite[i] = 1;
  }
}
void count468(const arma::mat &img, uvec checkList, vec &toWhite)
{
  vec neighborInd = zeros<vec>(3);
  int n = img.n_rows;
  int cell;
  for(int i = 0; i < checkList.n_elem; i++)
  {
    cell = checkList[i];
    neighborInd << img[cell + n] << img[cell + 1] << img[cell - n];
    if(any(neighborInd == 1))
      toWhite[i] = 1;
  }
}
void count248(const arma::mat &img, uvec checkList, vec &toWhite)
{
  vec neighborInd = zeros<vec>(3);
  int n = img.n_rows;
  int cell;
  for(int i = 0; i < checkList.n_elem; i++)
  {
    cell = checkList[i];
    neighborInd << img[cell - 1] << img[cell + n] << img[cell - n];
    if(any(neighborInd == 1))
      toWhite[i] = 1;
  }
}
void count268(const arma::mat &img, uvec checkList, vec &toWhite)
{
  vec neighborInd = zeros<vec>(3);
  int n = img.n_rows;
  int cell;
  for(int i = 0; i < checkList.n_elem; i++)
  {
    cell = checkList[i];
    neighborInd << img[cell - 1] << img[cell + 1] << img[cell - n];
    if(any(neighborInd == 1))
      toWhite[i] = 1;
  }
}
void countBNeighbors(const arma::mat &img, uvec checkList, vec &toWhite)
{
  vec neighborInd = zeros<vec>(8);
  int n = img.n_rows;
  int cell;
  int numBlack = 0;
  uvec isBlack;
  for(int i = 0; i < checkList.n_elem; i++)
  {
    cell = checkList[i];
    neighborInd << img[cell - 1] << img[cell + n - 1] << img[cell + n] << img[cell + n + 1] << img[cell + 1] 
                << img[cell - n + 1] << img[cell - n] << img[cell - n - 1];
    isBlack = find(neighborInd == 0);
    numBlack = isBlack.n_elem;
    if(numBlack <= 6 && numBlack >= 2)
    {
      toWhite[i] = 1;
    }
  }
}
void countWBTransitions(const arma::mat &img, uvec checkList, vec &toWhite)
{
  vec neighborInd = zeros<vec>(9);
  int n = img.n_rows;
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
      if(img[neighborInd[j]] == 1 && img[neighborInd[j+1]] == 0)
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
/*** R
data(message)
img = message
message = list()
message$image = crop(img)
message$thin = thinImage(message$image)
plotImageThinned(message$image, message$thin)
*/


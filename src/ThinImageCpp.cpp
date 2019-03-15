// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
using namespace arma;

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

arma::mat rgba2rgb(arma::mat img);
arma::uvec whichToFill(arma::mat img);
void countBNeighbors_7or8(const arma::mat &img, uvec white, uvec &toBlack);
arma::mat rgb2grayscale(arma::cube img);

arma::uvec thinImage(arma::mat img);
void countBNeighbors(const arma::mat &img, uvec checkList, vec &toWhite);
void countWBTransitions(const arma::mat &img, uvec checkList, vec &toWhite);
void count246(const arma::mat &img, uvec checkList, vec &toWhite);
void count468(const arma::mat &img, uvec checkList, vec &toWhite);
void count248(const arma::mat &img, uvec checkList, vec &toWhite);
void count268(const arma::mat &img, uvec checkList, vec &toWhite);

//' rgba2rgb
//' 
//' Removes alpha channel from png image.
//'
//' @param img A 3-d array with slices R, G, B, and alpha.
//' @export
// [[Rcpp::export]]
arma::cube rgba2rgb(arma::cube img)
{
  bool realAlphaFlag = arma::any(vectorise(img.slice(3)) < 1);
  
  if(realAlphaFlag)
  {
    for(int i = 0; i < img.n_rows; i++)
    {
      for(int j = 0; j < img.n_cols; j++)
      {
        img.at(i, j, 0) = img.at(i,j,0)*img.at(i,j,3);
        img.at(i, j, 1) = img.at(i,j,1)*img.at(i,j,3);
        img.at(i, j, 2) = img.at(i,j,2)*img.at(i,j,3);
      }
    }
  }
  
  return(img.slices(0,2));
}

//' rgba2rgb
//' 
//' Removes alpha channel from png image.
//'
//' @param img A 3-d array with slices R, G, B, and alpha.
//' @export
// [[Rcpp::export]]
arma::mat rgb2grayscale(arma::cube img)
{
  arma::mat grayImg(img.n_rows, img.n_cols, fill::ones);
  
  for(int i = 0; i < img.n_rows; i++)
  {
    for(int j = 0; j < img.n_cols; j++)
    {
      grayImg.at(i, j) = 0.2126*img.at(i,j,0) + 0.7152*img.at(i,j,1) + 0.0722*img.at(i,j,2);
    }
  }
  
  return(grayImg);
}

//' whichToFill
//' 
//' Finds pixels in the plot that shouldn't be white and makes them black. Quick and helpful cleaning for before the thinning algorithm runs.
//'
//' @param img A binary matrix.
//' @export
// [[Rcpp::export]]
arma::uvec whichToFill(arma::mat img)
{
  int n = img.n_rows;
  int p = img.n_cols;
  uvec flag(img.n_elem);
  for(int i = 0; i < img.n_elem; i++)
  {
    flag[i] = (img[i] == 1 && !(i % n == 0 || i % n == n-1 || i / n == 0 || i / n == p-1));
  }
  
  uvec white = find(flag);
  uvec toBlack = zeros<uvec>(white.n_elem);
  
  countBNeighbors_7or8(img, white, toBlack);
  
  return(white.elem(find(toBlack == 1)) + 1);
}

void countBNeighbors_7or8(const arma::mat &img, uvec white, uvec &toBlack)
{
  vec neighborInd = zeros<vec>(8);
  int n = img.n_rows;
  int cell;
  int numBlack = 0;
  uvec isBlack;
  for(int i = 0; i < white.n_elem; i++)
  {
    cell = white[i];
    neighborInd << img[cell - 1] << img[cell + n - 1] << img[cell + n] << img[cell + n + 1] << img[cell + 1] 
                << img[cell - n + 1] << img[cell - n] << img[cell - n - 1];
    isBlack = find(neighborInd == 0);
    numBlack = isBlack.n_elem;
    if(numBlack >= 7)
    {
      toBlack[i] = 1;
    }
  }
}
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
#data(message)
#img = message
#message = list()
#message$image = crop(img)
#message$thin = thinImage(message$image)
#plotImageThinned(message$image, message$thin)

test = png::readPNG("/Users/Nick/Documents/Projects/CSAFE/Handwriting/data/Writing_csafe_all.png")
dim(test)
oldTest = test[,,1:3]
test = rgba2rgb(test)
dim(test)
all(test[,,1:3] == oldTest[,,1:3])
*/


#include "Masks.h"
#include "RcppArmadillo.h"
using namespace arma;

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]


bool checkMask(arma::mat &img, int row, int col, arma::umat mask);
bool checkStep6(const arma::mat &img, int row, int col);

arma::mat cleanBinaryImage(arma::mat img);

//' cleanBinaryImage
//' 
//' Removes alpha channel from png image.
//'
//' @param img A matrix of 1s and 0s.
//' 
//' @return png image with the alpha channel removed
//' 
// [[Rcpp::export]]
arma::mat cleanBinaryImage(arma::mat img)
{
  int n = img.n_rows;
  int p = img.n_cols;
  
  arma::umat proposedFill = zeros<umat>(n, p);
  arma::umat proposedRemove = zeros<umat>(n, p);
  arma::umat matchH = zeros<umat>(n, p);

  //// Step 1
  for(int row = 0; row < n-2; row++)
  {
    for(int col = 0; col < p-2; col++)
    {
      if(checkMask(img, row, col, H1MASK))
      {
        matchH(row, col) = 1;
      }
    }
  }
  for(int row = 0; row < n-2; row++)
  {
    for(int col = 0; col < p-3; col++)
    {
      if(checkMask(img, row, col, H2MASK))
      {
        matchH(row, col) = 2;
      }
    }
  }
  for(int row = 0; row < n-3; row++)
  {
    for(int col = 0; col < p-2; col++)
    {
      if(checkMask(img, row, col, H3MASK))
      {
        matchH(row, col) = 3;
      }
    }
  }
  
  /// Steps 1-5
  for(int row = 0; row < n-2; row++)
  {
    for(int col = 0; col < p-2; col++)
    {
      if(matchH(row, col) == 1)
      {
        if(img(row, col) == 1)
        {
          proposedRemove(row, col+1) = 1;
          proposedRemove(row+1, col) = 1;
        }
        else if(img(row, col+2) == 1)
        {
          proposedRemove(row, col+1) = 1;
          proposedRemove(row+1, col+2) = 1;
        }
        else if(img(row+2, col+2) == 1)
        {
          proposedRemove(row+1, col+2) = 1;
          proposedRemove(row+2, col+1) = 1;
        }
        else if(img(row+2, col) == 1)
        {
          proposedRemove(row+1, col) = 1;
          proposedRemove(row+2, col+1) = 1;
        }
        else if(!checkMask(img, row-2, col-2, I1MASK))
        {
          proposedFill(row+1, col+1) = 1;
        }
      }
      else if(matchH(row,col) == 2)
      {
        if(img(row, col) == 1)
        {
          proposedRemove(row, col+1) = 1;
          proposedRemove(row+1, col) = 1;
        }
        else if(img(row, col+3) == 1)
        {
          proposedRemove(row, col+2) = 1;
          proposedRemove(row+1, col+3) = 1;
        }
        else if(img(row+2, col+3) == 1)
        {
          proposedRemove(row+1, col+3) = 1;
          proposedRemove(row+2, col+2) = 1;
        }
        else if(img(row+2, col) == 1)
        {
          proposedRemove(row+1, col) = 1;
          proposedRemove(row+2, col+1) = 1;
        }
        else if(!checkMask(img, row-2, col-2, I3MASK))
        {
          proposedFill(row+1, col+1) = 1;
          proposedFill(row+1, col+2) = 1;
        }
      }
      else if(matchH(row, col) == 3)
      {
        if(img(row, col) == 1)
        {
          proposedRemove(row, col+1) = 1;
          proposedRemove(row+1, col) = 1;
        }
        else if(img(row, col+2) == 1)
        {
          proposedRemove(row, col+1) = 1;
          proposedRemove(row+1, col+2) = 1;
        }
        else if(img(row+3, col+2) == 1)
        {
          proposedRemove(row+2, col+2) = 1;
          proposedRemove(row+3, col+1) = 1;
        }
        else if(img(row+3, col) == 1)
        {
          proposedRemove(row+2, col) = 1;
          proposedRemove(row+3, col+1) = 1;
        }
        else if(!checkMask(img, row-2, col-2, I2MASK))
        {
          proposedFill(row+1, col+1) = 1;
          proposedFill(row+2, col+1) = 1;
        }
      }
    }
  }
  
  for(int i = 0; i < n; i++)
  {
    for(int j = 0; j < p; j++)
    {
      if(proposedRemove(i, j) == 1)
      {
        img(i,j) = 1;
      }
      if(proposedFill(i,j) == 1)
      {
        img(i,j) = 0;
      }
    }
  }
  
  /// Step 6
  for(int row = 0; row < n-2; row++)
  {
    for(int col = 0; col < p-2; col++)
    {
      if(img(row+1, col+1) == 0)
      {
        if(checkStep6(img, row, col))
        {
          img(row+1,col+1) = 1;
        }
      }
    }
  }
  
  /// Step 7
  for(int row = n-5; row >= 0; row--)
  {
    for(int col = p-5; col >= 0; col--)
    {
      if(img(row+2, col+2) == 0)
      {
        if(img(row+1, col+2) == 1 && img(row, col+2) == 1)
        {
          if(checkMask(img, row, col, D1MASK) || checkMask(img, row, col, D2MASK) || checkMask(img, row, col, D3MASK) || checkMask(img, row, col, D4MASK) || checkMask(img, row, col, D5MASK))
          {
            img(row+2,col+2) = 1;
            if(checkMask(img, row+1, col, D1MASK) || checkMask(img, row+1, col, D2MASK) || checkMask(img, row+1, col, D3MASK))
            {
              img(row+3,col+2) = 1;
              if(checkMask(img, row+2, col, D1MASK))
              {
                img(row+4,col+2) = 1;
              }
            }
          }
        }
      }
    }
  }
  /// Step 7
  for(int row = 0; row < n-4; row++)
  {
    for(int col = 0; col < p-4; col++)
    {
      if(img(row+2, col+2) == 0)
      {
        if(img(row+3, col+2) == 1 && img(row+4, col+2) == 1)
        {
          if(checkMask(img, row, col, U1MASK) || checkMask(img, row, col, U2MASK) || checkMask(img, row, col, U3MASK) || checkMask(img, row, col, U4MASK) || checkMask(img, row, col, U5MASK))
          {
            img(row+2,col+2) = 1;
            if(checkMask(img, row-1, col, U1MASK) || checkMask(img, row-1, col, U2MASK) || checkMask(img, row-1, col, U3MASK))
            {
              img(row+1,col+2) = 1;
              if(checkMask(img, row-2, col, U1MASK))
              {
                img(row,col+2) = 1;
              }
            }
          }
        }
      }
    }
  }
  
  return(img);
}

bool checkMask(arma::mat &img, int row, int col, arma::umat mask)
{
  bool res = true;
  int rowCount = mask.n_rows;
  int colCount = mask.n_cols;
  
  if(row < 0 || col < 0 || row+rowCount >= img.n_rows || col+colCount >= img.n_cols)
    return(false);
  
  for(int i = 0; i < rowCount; i++)
  {
    for(int j = 0; j < colCount; j++)
    {
      res = res && (mask(i,j) == img(row + i, col + j) || mask(i,j) == 3);
    }
  }
  return(res);
}

bool checkStep6(const arma::mat &img, int row, int col)
{
  int connectivityCounter = 0;
  int blackNeighborCounter = 0;
  arma::vec ring( { img(row, col), img(row, col+1), img(row, col+2), img(row+1, col+2), img(row+2, col+2), img(row+2, col+1), img(row+2, col), img(row+1, col), img(row, col) });
  for(int i = 0; i < 8; i++)
  {
    if(ring(i) == 1 && ring(i+1) == 0)
      connectivityCounter++;
    if(ring(i) == 0)
      blackNeighborCounter++; 
  }
  
  if((connectivityCounter == 0 || connectivityCounter == 1) && (blackNeighborCounter >= 0 && blackNeighborCounter <= 2))
    return(true);
  else
    return(false);
}

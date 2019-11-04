// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "Rcpp.h"
using namespace Rcpp;

void rcpp_rcout(NumericVector v);
List addToFeatures(List FeatureList, List LetterList, IntegerVector dims);
double findEucDistances(int centroidIndex, NumericVector path, NumericVector dims);
double findDistanceBetweenTwoPoints(NumericVector p1, NumericVector p2);
List extractInfo(List FeatureSet, List LetterList, IntegerVector dims);
NumericVector convertIndextoXY(int centroidIndex, NumericVector dims);
int indexToRow(int index, int rows);
int indexToCol(int index, int rows);
//' @export
// [[Rcpp::export]]
List addToFeatures(List FeatureSet, List LetterList, IntegerVector dims){
  NumericVector distFromCentroid;
  List listTest;
  for(int i = 0; i < FeatureSet.size(); i++){
    
    //Extract Info Needed for Calcs ONCE
    NumericVector num = Rcpp::as<NumericVector>(Rcpp::as<Rcpp::List>(FeatureSet[i])["lHalf"]);
    NumericVector num1 = Rcpp::as<NumericVector>(Rcpp::as<Rcpp::List>(FeatureSet[i])["rHalf"]);
    NumericVector dims1 = Rcpp::as<NumericVector>(dims);
    NumericVector path = Rcpp::as<NumericVector>(Rcpp::as<Rcpp::List>(LetterList[i])["path"]);
    int centroidIndex = Rcpp::as<int >(Rcpp::as<Rcpp::List>(FeatureSet[i])["centroid_index"]);
    
    //FIND SUM OF EUC DISTANCES OF THIS SPECIFIC INDEX
    double averageDistFromCentroid = findEucDistances(centroidIndex, path, dims1);
    distFromCentroid.push_back(averageDistFromCentroid);

  }
  
  return Rcpp::List::create(Rcpp::Named("distFromCentroid") = distFromCentroid);
}

double findEucDistances(int centroidIndex, NumericVector path, NumericVector dims){
  double sumOfDistances = 0;
  NumericVector centroidXY = convertIndextoXY(centroidIndex, dims);
  for(int i = 0; i<path.size(); i++){
    NumericVector pointXY = convertIndextoXY(path[i], dims);
    sumOfDistances += findDistanceBetweenTwoPoints(centroidXY, pointXY); 
  }
  double averageDistFromCentroid = (1.0/((double)path.size()-1.0))*sumOfDistances;
  return averageDistFromCentroid;
}

double findDistanceBetweenTwoPoints(NumericVector p1, NumericVector p2){
  int x_b = p1[0]; 
  int x_a = p2[0];
  
  int y_b = p1[1];
  int y_a = p2[1];
  double distance = sqrt(pow((x_b-x_a), 2)+pow((y_b-y_a), 2));
  return distance;
}

//Maybe use, currently in handler function 'addToFeatures()'
List extractInfo(List FeatureSet, List LetterList, NumericVector dims){
  
  
  
  return FeatureSet;
}


NumericVector convertIndextoXY(int index, NumericVector dims){
  int numRows = dims[0];
  int rowIndex = indexToRow(index, numRows);
  int colIndex = indexToCol(index, numRows);
  NumericVector XY = NumericVector::create(rowIndex,colIndex);
  return XY;
}

int indexToRow(int index, int rows){
  return ((index-1)/rows)+1;
}

int indexToCol(int index, int rows){
  return ((index-1)%rows)+1;
}


void rcpp_rcout(NumericVector v){
  // printing value of vector
  Rcout << "Numeric Vector:"<< v << "\n";
}




// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//
/*** R

***/




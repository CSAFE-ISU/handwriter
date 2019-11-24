// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "Rcpp.h"
using namespace Rcpp;

void rcpp_rcout(NumericVector v);
List addToFeatures(List FeatureList, List LetterList, IntegerVector dims);
double findEucDistances(int centroidIndex, NumericVector path, NumericVector dims);
double findDistanceBetweenTwoPoints(NumericVector p1, NumericVector p2);
List extractInfo(List FeatureSet, List LetterList, IntegerVector dims);
NumericVector convertIndextoXY(int centroidIndex, NumericVector dims, int flipY = 0);
int indexToRow(int index, int rows);
int indexToCol(int index, int rows);
List findLoops(List allpaths, NumericVector dims);
List findLoopDims(List knownLoops, NumericVector dims);
NumericVector findCentroidofLoop(NumericVector loopPath, NumericVector dims);

//' @export
// [[Rcpp::export]]
List addToFeatures(List FeatureSet, List LetterList, IntegerVector vectorDims){
  NumericVector compactness;
  NumericVector loopCount;
  List loopDims;
  
  List listTest;
  for(int i = 0; i < FeatureSet.size(); i++){
    
    //Extract Info Needed for Calcs ONCE
    NumericVector num = Rcpp::as<NumericVector>(Rcpp::as<Rcpp::List>(FeatureSet[i])["lHalf"]);
    NumericVector num1 = Rcpp::as<NumericVector>(Rcpp::as<Rcpp::List>(FeatureSet[i])["rHalf"]);
    NumericVector dims = Rcpp::as<NumericVector>(vectorDims);
    NumericVector path = Rcpp::as<NumericVector>(Rcpp::as<Rcpp::List>(LetterList[i])["path"]);
    int centroidIndex = Rcpp::as<int >(Rcpp::as<Rcpp::List>(FeatureSet[i])["centroid_index"]);
    
    //FIND SUM OF EUC DISTANCES OF THIS SPECIFIC INDEX
    double averageDistFromCentroid = findEucDistances(centroidIndex, path, dims);
    compactness.push_back(averageDistFromCentroid);

    //FIND LOOPS
    List allPaths = Rcpp::as<Rcpp::List>(Rcpp::as<Rcpp::List>(LetterList[i])["allPaths"]);
    List loops = findLoops(allPaths, dims);
    List loopDimensions = findLoopDims(loops, dims);
    loopDims.push_back(loopDimensions);
    int loopCounter = loops.size();
    loopCount.push_back(loopCounter);
  }
  
  //Currently returns compactness of a letter and loopCount of a letter
  return Rcpp::List::create(Rcpp::Named("compactness") = compactness, Rcpp::Named("loopCount") = loopCount);
}

//Return the list of loop dimensions for the letter
List findLoopDims(List knownLoops, NumericVector dims){
  List loopDims = Rcpp::List::create();
  //For every loop
  for(int i = 0; i<knownLoops.size(); i++){
    //Find point (or approximated point) of where loop connects
    NumericVector pathConsidering = Rcpp::as<NumericVector>(knownLoops[i]);
  
    //Determine centroid of loop
    NumericVector centroid = findCentroidofLoop(pathConsidering, dims);
    
    //Determine longest line through the centroid
    NumericVector longestLinethroughCentroid;
    //int longestLineDistance = 10000000;
    
    //For every point on the loop
    for(int j = 0; j<pathConsidering.size(); j++){
      NumericVector pointXY = convertIndextoXY(pathConsidering[j], dims);
      //Find slope between point and centroid
      //double slopeFromXYToCentroid = ((centroid[1]-pointXY[1])/(centroid[0]-pointXY[0]));
      //double yIntercept = (slopeFromXYToCentroid * pointXY[0]) - pointXY[1];//negatives could be wrong?
            
      //Rcout << "PointXY:"<< pointXY << "\n";
      //Rcout << "Centroid:"<< centroid << "\n";
      //Rcout << "Slope:"<< slopeFromXYToCentroid << "\n";
      //Rcout << "yInt:"<< yIntercept << "\n\n";


      //Go through every other point 
      for(int k = j; k<pathConsidering.size(); k++){
        NumericVector checkPointXY = convertIndextoXY(pathConsidering[k], dims);
        
        //Does it fall on the line?
        
        //if it falls on the line and the distance is bigger than previous, make this the new longest
        
      }
      
      
      
      
      //once you found the longest find its perpendicular line and use that as the shortest line
      if(j==0)break;
    }
    //save longest line and shortest line in a list
  }
  
  //return list of all dims (heightxwidth)
  return loopDims;
}

NumericVector findCentroidofLoop(NumericVector loopPath, NumericVector dims){
  int xTotal = 0;
  int yTotal = 0;
  int numPoints = loopPath.size();
  
  for(int i = 0; i< loopPath.size(); i++){
    NumericVector pointXY = convertIndextoXY(loopPath[i], dims); 
    xTotal += pointXY[0];
    yTotal += pointXY[1];
  }
  int xCentroid = xTotal/numPoints;
  int yCentroid = yTotal/numPoints;
  
  NumericVector XY = NumericVector::create(xCentroid,yCentroid);
  return XY;
  
}
//returns all known loops for the letters
List findLoops(List allpaths, NumericVector dims){
  List knownLoops = Rcpp::List::create();
  NumericVector pathConsidering;
  int count = 0;
  for(int i = 0; i < allpaths.size(); i++){
     pathConsidering = Rcpp::as<NumericVector>(allpaths[i]);
     if(pathConsidering.size() < 10) continue; //arbitarty value of 10 chosen, worked for every letter so far
     
     NumericVector startingPoint = convertIndextoXY(pathConsidering[0], dims);
     NumericVector endingPoint  = convertIndextoXY(pathConsidering[pathConsidering.size()-1], dims);
     double distanceBetweenPoints = findDistanceBetweenTwoPoints(startingPoint, endingPoint);
     if(distanceBetweenPoints < 6){
       count++;
       knownLoops.push_back(pathConsidering);
     }
  }    
  
  return knownLoops;
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


NumericVector convertIndextoXY(int index, NumericVector dims, int flipY){
  int numRows = dims[0];
  int rowIndex = indexToRow(index, numRows);
  int colIndex = indexToCol(index, numRows);
  if(flipY==1){
      colIndex = dims[0] - colIndex;
  }
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




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
List findLoopRatios(List loopDimensions, NumericVector dims);
NumericVector findCentroidofLoop(NumericVector loopPath, NumericVector dims);

//' addToFeatures
//' 
//' @export
// [[Rcpp::export]]
List addToFeatures(List FeatureSet, List LetterList, IntegerVector vectorDims){
  NumericVector compactness;
  NumericVector loopCount;
  List loopDims;
  List loopInfoList;
  
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
    List loopRatio = findLoopRatios(loopDimensions, dims);
    
    int loopCounter = loops.size();
    loopCount.push_back(loopCounter);
    
    List loopInfo = Rcpp::List::create(Rcpp::Named("loopRatio") = loopRatio, Rcpp::Named("loopPlottingInfo") = loopDimensions, Rcpp::Named("loopCount") = loopCounter);
    loopInfoList.push_back(loopInfo);
  }
  
  //Currently returns compactness of a letter, loopCount of a letter, and dimensions of the loop.
  return Rcpp::List::create(Rcpp::Named("compactness") = compactness, Rcpp::Named("loopInfo") = loopInfoList);
}

//Given a list of loop dimensions, return the list of ratios of height to width of each loop
List findLoopRatios(List loopDimensions, NumericVector dims){
  List loopRatios = Rcpp::List::create();
  
  for(int i = 0; i<loopDimensions.size(); i++){
     List loopToProcess = loopDimensions[i];
     List longestLine = loopToProcess[1];
     NumericVector longestLineP1 = longestLine[0];
     NumericVector longestLineP2 = longestLine[1];
     double longestLineDist = sqrt(pow(longestLineP2[0]-longestLineP1[0],2) + pow(longestLineP2[1]-longestLineP1[1],2.0));
     
     List shortestLine = loopToProcess[2];
     NumericVector shortestLineP1 = shortestLine[0];
     NumericVector shortestLineP2 = shortestLine[1];
     double shortestLineDist = sqrt(pow(shortestLineP2[0]-shortestLineP1[0],2) + pow(shortestLineP2[1]-shortestLineP1[1],2.0));
     
     double ratio = longestLineDist/shortestLineDist;
     String loopNum = "Loop ";
     loopRatios.push_back(ratio, loopNum += std::to_string(i+1));
   }
  
  
  return loopRatios;
}
//Given a list of loops, return a list of vectors that has the points of the longest line 
//through the centroid and the line perpendicular to it
List findLoopDims(List knownLoops, NumericVector dims){
  List loopDims = Rcpp::List::create();

  //For every known loop
  for(int i = 0; i<knownLoops.size(); i++){
    
    NumericVector pathConsidering = Rcpp::as<NumericVector>(knownLoops[i]);
    NumericVector centroid = findCentroidofLoop(pathConsidering, dims);
    centroid[1] = dims[0] - centroid[1]; //flip index so y makes sense

    List longestLine = Rcpp::List::create();
    int longestLineDistance = 0;

    //FIND THE LONGEST LINE OF A LOOP THROUGH THE CENTROID ------------------------------------------------------------
    
    //For every point on the loop
    for(int j = 0; j < pathConsidering.size(); j++){
      NumericVector pointXY = convertIndextoXY(pathConsidering[j], dims, 1);
      if (pointXY[0] == centroid[0] || pointXY[1] == centroid[1]) continue;

      //Find slope between point and centroid
      double slopeFromXYToCentroid = ((centroid[1]-pointXY[1])/(centroid[0]-pointXY[0]));
      double yIntercept = pointXY[1] - (slopeFromXYToCentroid * pointXY[0]);

      //Go through every other point
      for(int k = 0; k<pathConsidering.size(); k++){
        NumericVector checkPointXY = convertIndextoXY(pathConsidering[k], dims, 1);

      //Does it fall on the line?
      double distanceFromLine = abs(checkPointXY[1] - ((slopeFromXYToCentroid * checkPointXY[0]) + yIntercept));

      if(distanceFromLine < 2){//only check distance if it is more on the line that a previous one.
          //get distance between two points, if it is bigger than max then update max
          int distance = findDistanceBetweenTwoPoints(pointXY, checkPointXY);
          if (distance > longestLineDistance){
            longestLineDistance = distance;
            longestLine = Rcpp::List::create(pointXY, checkPointXY);
          }
        }
      }
    }
    
    NumericVector lengthPoint1 = longestLine[0];
    NumericVector lengthPoint2 = longestLine[1];

    //FIND A PERPENDICULAR LINE -------------------------------------------------------------------------

    //once you found the longest find its perpendicular line and use that as the shortest line
    double longestLineSlope = ((lengthPoint2[1] - lengthPoint1[1])/(lengthPoint2[0]-lengthPoint1[0]));
    double shortestLineSlope = -1.0/longestLineSlope;
    double yIntercept = centroid[1] - (shortestLineSlope * centroid[0]);

    //These variables will keep track of the closest matches as we go around
    //USE AFTER GET BASE WORKING --- THEN JUST FIND CLOSEST POINTS ON BOTH SIDES
    NumericVector closestPerpPoint1;
    double closestPerpPoint1Val = INFINITY;
    NumericVector closestPerpPoint2;
    double closestPerpPoint2Val = INFINITY;

    List shortestLine = Rcpp::List::create();
    //go through every point to find two points that fall on perp through centroid
    for(int j = 0; j < pathConsidering.size()/2; j++){
      NumericVector checkPointXY = convertIndextoXY(pathConsidering[j], dims, 1);
      
      double distanceFromPerpLine = abs(checkPointXY[1] - ((shortestLineSlope * checkPointXY[0]) + yIntercept));
      if(distanceFromPerpLine < closestPerpPoint1Val){
        closestPerpPoint1Val = distanceFromPerpLine;
        closestPerpPoint1 = checkPointXY;
        }
    }

    //look for second point
    for(int k = pathConsidering.size()/2; k < pathConsidering.size(); k++){
      NumericVector checkPointXY = convertIndextoXY(pathConsidering[k], dims, 1);

      double distanceFromPerpLine = abs(checkPointXY[1] - ((shortestLineSlope * checkPointXY[0]) + yIntercept));
      if(distanceFromPerpLine < closestPerpPoint2Val){
        closestPerpPoint2Val = distanceFromPerpLine;
        closestPerpPoint2 = checkPointXY;
      }
    }
    shortestLine = Rcpp::List::create(closestPerpPoint1, closestPerpPoint2);

    NumericVector widthPoint1 = shortestLine[0];
    NumericVector widthPoint2 = shortestLine[1];

    //save longest line and shortest line vector in a list
    //Before we return, need to convert all column values back to inverted
    centroid[1] = dims[0] - centroid[1];
    NumericVector invertedLP1 = longestLine[0];
    NumericVector invertedLP2 = longestLine[1];
    NumericVector invertedSP1 = shortestLine[0];
    NumericVector invertedSP2 = shortestLine[1];

    invertedLP1[1] = dims[0] - invertedLP1[1];
    invertedLP2[1] = dims[0] - invertedLP2[1];
    invertedSP1[1] = dims[0] - invertedSP1[1];
    invertedSP2[1] = dims[0] - invertedSP2[1];
    
    longestLine = Rcpp::List::create(Rcpp::Named("P1")=invertedLP1, Rcpp::Named("P2")=invertedLP2);
    shortestLine = Rcpp::List::create(Rcpp::Named("P1")=invertedSP1, Rcpp::Named("P2")=invertedSP2);

    List loopInfoToAdd = Rcpp::List::create(Rcpp::Named("centroid") = centroid, Rcpp::Named("longestLine")=longestLine, Rcpp::Named("shortestLine")= shortestLine);
    String loopName = "Loop ";
    loopDims.push_back(loopInfoToAdd, loopName += std::to_string(i+1));
  }
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




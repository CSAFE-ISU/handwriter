// force return as a vector rather than single column matrix
#define RCPP_ARMADILLO_RETURN_ANYVEC_AS_VECTOR
// https://stackoverflow.com/a/73979115
#include <RcppArmadillo.h>

#include <algorithm>
// [[Rcpp::depends(RcppArmadillo)]]

void graphInfo_subpart1(Rcpp::List imageList, bool isProto, int &numPaths,
                        int &letterSize) {
    // Find the sum of the lengths of paths in graph 1
    if (!isProto) {
        auto allPaths = Rcpp::as<Rcpp::List>(imageList["allPaths"]);
        numPaths = static_cast<int>(allPaths.size());
        // this should be equivalent to length(unlist(x))?
        int sz = allPaths.size();
        for (int i = 0; i < sz; i++) {
            letterSize += Rcpp::as<Rcpp::IntegerVector>(allPaths[i])
                              .size();  // typecast??
        }
    } else {
        auto pathEnds = Rcpp::as<Rcpp::IntegerMatrix>(imageList["pathEnds"]);
        numPaths = static_cast<int>(
            Rcpp::as<Rcpp::Dimension>(pathEnds.attr("dim"))[0]);
        auto lengths = Rcpp::as<arma::Col<int>>(imageList["lengths"]);
        letterSize = arma::sum(lengths);
    }
}

arma::Mat<int> pathToRC(arma::Col<int> &pvec, Rcpp::Dimension dims) {
    arma::Mat<int> res(pvec.size(), 2);
    int d0 = static_cast<int>(dims[0]);
    res.col(0) = pvec - 1;
    res.col(1) = pvec - 1;
    for (int i = 0; i < pvec.size(); ++i) {
        res.at(i, 0) = 1 + (res.at(i, 0) / d0);
        res.at(i, 1) = d0 - (res.at(i, 1) % d0);
    }
    return res;
}

void graphInfo_subpart2(Rcpp::List imageList, bool isProto, int numPaths,
                        int numPathCuts, int pathCheckNum,
                        arma::Cube<double> &pq, arma::Cube<double> &pe,
                        arma::Cube<double> &cent, arma::Col<int> &len) {
    if (isProto) {
        auto lengths = Rcpp::as<arma::Col<int>>(imageList["lengths"]);
        auto pathEnds = Rcpp::as<arma::Mat<double>>(imageList["pathEnds"]);
        auto pathQuarters =
            Rcpp::as<arma::Mat<double>>(imageList["pathQuarters"]);
        auto pathCenter = Rcpp::as<arma::Mat<double>>(imageList["pathCenter"]);
        // numPaths <= pathCheckNum, so we can
        // skip the bounds check inside the loop
        // and just have numPaths outside
        for (int i = 0; i < numPaths; ++i) {
            len[i] = lengths[i];
            pe.slice(i) = arma::reshape(pathEnds.row(i), 2, 2).t();
            cent.subcube(0, 0, i, 0, 1, i) = pathCenter.row(i);
            pq.slice(i) =
                arma::reshape(pathQuarters.row(i), 2, numPathCuts - 1).t();
        }
    } else {
        auto centroid = Rcpp::as<arma::rowvec>(imageList["centroid"]);
        auto imagedim = Rcpp::as<Rcpp::Dimension>(
            Rcpp::as<Rcpp::IntegerMatrix>(imageList["image"]).attr("dim"));
        auto pathEndsrc = Rcpp::as<arma::Cube<double>>(imageList["pathEndsrc"]);
        auto allPaths = Rcpp::as<Rcpp::List>(imageList["allPaths"]);
        for (int i = 0; i < numPaths; ++i) {
            auto pvec = Rcpp::as<arma::Col<int>>(allPaths[i]);
            int l = pvec.size();
            len[i] = l;
            pe.slice(i) = pathEndsrc.slice(i);  // iffy
            auto pathRC = arma::conv_to<arma::Mat<double>>::from(pathToRC(pvec, imagedim));
            cent.subcube(0, 0, i, 0, 1, i) =
                arma::mean(pathRC, 0) - centroid;
            // the Rfast::eachrow
            for (int j = 0; j < numPathCuts - 1; j++) {
                // ceil but 0-indexed -> floor
                int ind = std::floor(l / ((1.0 * numPathCuts) / (j + 1)));
                pq.subcube(j, 0, i, j, 1, i) = pathRC.row(ind) - centroid;
            }
        }
    }
}

//' getGraphInfo_cpp
//'
//' Gather and format the parameter values need to calculate the distance
// between ' two graphs.
//'
//' @param imageList1 A graph
//' @param imageList2 A graph
//' @param isProto1 True or false. Is the graph information in prototype format?
//' @param isProto2 True or false. Is the graph information in prototype format?
//' @param numPathCuts An integer number of cuts to make when comparing segments
// of paths ' @return List of formatted parameters
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::List getGraphInfo_cpp(Rcpp::List imageList1, Rcpp::List imageList2,
                            bool isProto1, bool isProto2, int numPathCuts) {
    int numPaths1 = 0;
    int letterSize1 = 0;
    graphInfo_subpart1(imageList1, isProto1, numPaths1, letterSize1);

    int numPaths2 = 0;
    int letterSize2 = 0;
    graphInfo_subpart1(imageList2, isProto2, numPaths2, letterSize2);

    int pathCheckNum = std::max(numPaths1, numPaths2);

    arma::Cube<double> pq1(numPathCuts - 1, 2, pathCheckNum, arma::fill::value(NA_REAL));
    arma::Cube<double> pe1(2, 2, pathCheckNum, arma::fill::value(NA_REAL));
    arma::Cube<double> cent1(1, 2, pathCheckNum, arma::fill::value(NA_REAL));
    arma::Col<int> len1(pathCheckNum);
    graphInfo_subpart2(imageList1, isProto1, numPaths1, numPathCuts,  //
                       pathCheckNum, pq1, pe1, cent1, len1);

    arma::Cube<double> pq2(numPathCuts - 1, 2, pathCheckNum, arma::fill::value(NA_REAL));
    arma::Cube<double> pe2(2, 2, pathCheckNum, arma::fill::value(NA_REAL));
    arma::Cube<double> cent2(1, 2, pathCheckNum, arma::fill::value(NA_REAL));
    arma::Col<int> len2(pathCheckNum);
    graphInfo_subpart2(imageList2, isProto2, numPaths2, numPathCuts,  //
                       pathCheckNum, pq2, pe2, cent2, len2);

    Rcpp::LogicalVector pathEndPointsMatch(pathCheckNum * pathCheckNum);
    pathEndPointsMatch.fill(true);

    Rcpp::NumericMatrix weights(pathCheckNum, pathCheckNum);
    weights.fill(NA_REAL);

    Rcpp::IntegerVector letterSize =
        Rcpp::IntegerVector::create(letterSize1, letterSize2);

    using Rcpp::_;
    return Rcpp::List::create(_["numPaths1"] = numPaths1,                    //
                              _["numPaths2"] = numPaths2,                    //
                              _["pe1"] = pe1,                                //
                              _["pe2"] = pe2,                                //
                              _["pq1"] = pq1,                                //
                              _["pq2"] = pq2,                                //
                              _["cent1"] = cent1,                            //
                              _["cent2"] = cent2,                            //
                              _["len1"] = len1,                              //
                              _["len2"] = len2,                              //
                              _["letterSize"] = letterSize,                  //
                              _["pathCheckNum"] = pathCheckNum,              //
                              _["pathEndPointsMatch"] = pathEndPointsMatch,  //
                              _["weights"] = weights);
}

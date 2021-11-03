#' createClusterTemplates
#'
#' Given an input folder, create templates 
#'
#' @param K Integer number of clusters. If numOutliers > 0, then
#' there will be K+1 clusters.
#' @param numberToRun Integer number of clustering templates to create
#' @param numCores Integer number of cores. Each clustering template will be created on
#' a different core
#' @param numDistCores Integer number of cores to use for distance calculations
#' @param numPathCuts Integer number of segments to cut each path into
#' @param iter.max Integer maximum number of iterations to allow cluster centers to converge
#' @param gamma Float parameter for calculating the outlier cutoff. If numOutliers is zero, gamma has no effect.
#' @param results_file String name of file without the file extension to save the clustering template
#' @param meanGraph String choice of algorithm for calculating mean graphs. The choices are 'basic',
#' 'slow_change' and 'kmeans'
#' @param meanGraphOrder Chose the order in which to add graphs to the mean 
#' graph calculations. The options are 'sequential' and 'random'
#' @param fillClusters String choice of how to choose which graph to reassign to an empty cluster. 
#' Choice are 'random' and 'farthest'
#' @return List of clustering template(s)
#'
#' @keywords ?
#' 
#' @export
createClusterTemplates = function(documentDirectory,
                                  logDirectory = "",
                                  dataDirectory = "",
                                  resultsFile = "",
                                  K = 40, 
                                  numberToRun = 1, 
                                  numCores = 1, 
                                  numDistCores = 1, 
                                  iter.max = 500, 
                                  gamma = 3,
                                  meanGraph = 'slow_change',
                                  meanGraphOrder = 'sequential',
                                  fillClusters = 'farthest')
{
  library(foreach)
  library(tidyverse)
  library(tidyr)
  library(dplyr)
  
  numPathCuts = 8
  
  #Get rds files from clustertemplate_docprocessing
  #processDocsForClustering(documentDirectory, logDirectory, dataDirectory, numCores)
  
  #MakeClusterTemplates will grab the rds files and create the cluster templates
  makeClusterTemplates(documentDirectory, dataDirectory, resultsFile, K, numberToRun, numDistCores, numPathCuts, iter.max, gamma, meanGraph, meanGraphOrder, fillClusters)
}

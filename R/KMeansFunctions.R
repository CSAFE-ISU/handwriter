#' runLetterKmeans
#'
#' Create and save one or more clustering templates.
#'
#' @param imList List graphs from handwriting samples
#' @param centers List of starting cluster centers
#' @param K Integer number of clusters. If numOutliers > 0, then
#' there will be K+1 clusters.
#' @param numberToRun Integer number of clustering templates to create
#' @param numCores Integer number of cores. Each clustering template will be created on
#' a different core
#' @param numDistCores Integer number of cores to use for distance calculations
#' @param numPathCuts Integer number of segments to cut each path into
#' @param iter.max Integer maximum number of iterations to allow cluster centers to converge
#' @param numOutliers Integer number of outliers to allow. If numOutliers is zero, no outliers will be
#' assigned.
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
runLetterKmeans = function(imList, 
                           centers, 
                           K, 
                           numberToRun, 
                           numCores = 1, 
                           numDistCores = 0, 
                           numPathCuts = 8, 
                           iter.max = 500, 
                           numOutliers = round(.25*length(imList)),
                           gamma = 3,
                           results_file,
                           meanGraph = 'slow_change',
                           meanGraphOrder = 'sequential',
                           fillClusters = 'farthest')
{ if (numCores > 1){
    doParallel::registerDoParallel(numCores)
    resList = foreach(i = 1:numberToRun) %dopar% {
      # Start log file
      outfile = file.path(documentDirectory, "logs", paste0("template_", results_file,"_", i, ".txt"))
      sink(file = outfile, append=TRUE)
      cat("Starting run ", outfile, ". \n \n")
      runAndSaveKmeans(imList = imList, 
                       centers = centers, 
                       K = K, 
                       iter_number=i, 
                       numDistCores = numDistCores, 
                       numPathCuts = numPathCuts, 
                       iter.max = iter.max,
                       numOutliers = numOutliers,
                       gamma = gamma,
                       results_file,
                       outfile = outfile, 
                       meanGraph = meanGraph,
                       meanGraphOrder = meanGraphOrder,
                       fillClusters = fillClusters)
    }
  } else {
    resList = list()
    for (i in 1:numberToRun) {
      # Start log file
      outfile = file.path("logs", paste0("template_", results_file,"_", i, ".txt"))
      sink(file = outfile, append=TRUE)
      cat("Starting run ", outfile, ". \n \n")
      resList[[i]] = runAndSaveKmeans(imList = imList, 
                                      centers = centers, 
                                      K = K, 
                                      iter_number=i, 
                                      numDistCores = numDistCores, 
                                      numPathCuts = numPathCuts, 
                                      iter.max = iter.max,
                                      numOutliers = numOutliers,
                                      gamma = gamma,
                                      results_file,
                                      outfile = outfile, 
                                      meanGraph = meanGraph,
                                      meanGraphOrder = meanGraphOrder,
                                      fillClusters = fillClusters)
    }
  }
  # Save all clustering templates in a single RDS file
  saveRDS(resList, file.path(documentDirectory, "rdata", "Kmeans", paste0("TemplateListFull_", results_file, ".RDS")))
  return(resList)
}


#' runAndSaveKmeans
#'
#' Create and save a signle clustering template.
#'
#' @param imList List graphs from handwriting samples
#' @param centers List of starting cluster centers
#' @param K Integer number of clusters. If numOutliers > 0, then
#' there will be K+1 clusters.
#' @param iter_number Integer iteration number. This will be used in the file name of the 
#' saved clustering template.
#' @param numberToRun Integer number of clustering templates to create
#' @param numDistCores Integer number of cores to use for distance calculations
#' @param numPathCuts Integer number of segments to cut each path into
#' @param iter.max Integer maximum number of iterations to allow cluster centers to converge
#' @param numOutliers Integer number of outliers to allow. If numOutliers is zero, no outliers will be
#' assigned.
#' @param gamma Float parameter for calculating the outlier cutoff. 
#' If numOutliers is zero, gamma has no effect.
#' @param outfile String file path and name of log file
#' @param meanGraph String choice of algorithm for calculating mean graphs. The choices are 'basic',
#' 'slow_change' and 'kmeans'
#' @param meanGraphOrder Chose the order in which to add graphs to the mean 
#' graph calculations. The options are 'sequential' and 'random'
#' @param fillClusters String choice of how to choose which graph to reassign to an empty cluster. 
#' Choice are 'random' and 'farthest'
#' @return Clustering template
#'
#' @keywords ?
runAndSaveKmeans = function(imList, 
                            centers, 
                            numDistCores = 0, 
                            K, 
                            iter_number, 
                            numPathCuts = 8, 
                            iter.max = 500, 
                            numOutliers = round(.25*length(imList)), 
                            gamma = 3,
                            results_file,
                            outfile,
                            meanGraph = 'slow_change',
                            meanGraphOrder = 'sequential',
                            fillClusters = 'farthest'
                            )
{ # Make clustering template
  template = letterKmeans(imList = imList, 
                          centers = centers, 
                          numDistCores = numDistCores, 
                          K = K, 
                          numPathCuts = numPathCuts, 
                          iter.max = iter.max, 
                          numOutliers = numOutliers, 
                          gamma = gamma,
                          outfile = outfile,
                          meanGraph = meanGraph,
                          meanGraphOrder = meanGraphOrder,
                          fillClusters = fillClusters)
  
  # Save and return clustering template
  saveRDS(template, file = file.path(documentDirectory, "rdata", "Kmeans", paste0("kmRes_", results_file, "_", iter_number, ".RDS")))
  return(template)
}


#' letterKmeans
#'
#' Run a Kmean algorithm with or without outliers to create a clustering template.
#'
#' @param imList List graphs from handwriting samples
#' @param centers List of starting cluster centers
#' @param K Integer number of clusters. If numOutliers > 0, then
#' there will be K+1 clusters.
#' @param iter_number Integer iteration number. This will be used in the file name of the 
#' saved clustering template.
#' @param numberToRun Integer number of clustering templates to create
#' @param numDistCores Integer number of cores to use for distance calculations
#' @param numPathCuts Integer number of segments to cut each path into
#' @param iter.max Integer maximum number of iterations to allow cluster centers to converge
#' @param numOutliers Integer number of outliers to allow. If numOutliers is zero, no outliers will be
#' assigned.
#' @param gamma Float parameter for calculating the outlier cutoff. 
#' If numOutliers is zero, gamma has no effect.
#' @param outfile String file path and name of log file
#' @param meanGraph String choice of algorithm for calculating mean graphs. The choices are 'basic',
#' 'slow_change' and 'kmeans'
#' @param meanGraphOrder Chose the order in which to add graphs to the mean 
#' graph calculations. The options are 'sequential' and 'random'
#' @param fillClusters String choice of how to choose which graph to reassign to an empty cluster. 
#' Choice are 'random' and 'farthest'
#' @return Clustering template
#'
#' @keywords ?
letterKmeans = function(imList, 
                        centers, 
                        numDistCores = 0, 
                        K, 
                        numPathCuts = 8, 
                        iter.max = 500, 
                        numOutliers = round(.25*length(imList)), 
                        gamma = 3, 
                        outfile,
                        meanGraph = 'slow_change',
                        meanGraphOrder = 'sequential',
                        fillClusters = 'farthest')
{ # Number of graphs in list
  n = length(imList)
  
  # Initialize
  oldCluster = rep(0, n)  # Cluster assignments
  cluster = rep(NA, n)  # Cluster assignments
  iters = 0  # current iteration
  WCD = rep(0, K)  # Within Cluster Distance
  dists = matrix(NA, ncol = K, nrow = n)  # dists[i,j] = distance between i-th graph and j-th cluster
  centerMoved = rep(TRUE, K)  # Whether cluster changed
  centerMovedOld = rep(TRUE, K)  # Whether cluster changed
  
  # If using outliers, initialize outlier parameters
  if (numOutliers > 0){
    outlierCutoff = Inf
    outliers = NULL
    potentialOutliers = NULL
    distsClust = rep(NA, length(imList))
  }
  
  # If running in parallel, register cores and initialize vector
  if (numDistCores > 1){
    doParallel::registerDoParallel(numDistCores)
    
    # Initialize. Will store distances between a single graph and each cluster.
    vecoflengthj = rep(0, K)
  }
  
  while(TRUE)
  { # Update
    iters = iters + 1
    
    # Add message in log
    cat("Starting iter ", iters, ".\n")
    
    # Store current cluster centers
    oldCenters = centers

    # For each graph i, and for each cluster j, if the cluster center moved, calculate the distance
    # between graph i and the new cluster center
    if (numDistCores == 0){ # Run sequentially  
      for(i in 1:length(imList))
      { 
        for(j in 1:K)
        { 
          if(centerMoved[j] | centerMovedOld[j])
          {
            dists[i,j] = getGraphDistance(imList[[i]], centers[[j]], isProto1 = FALSE, isProto2 = TRUE, numPathCuts = numPathCuts)$matching_weight
          }
        }
      }
    } else {  # Run in parallel
      listoflengthi = foreach(i = 1:length(imList)) %dopar% {
        for(j in 1:K)
        {
          if(centerMoved[j] | centerMovedOld[j])
          {
            vecoflengthj[j] = getGraphDistance(imList[[i]], centers[[j]], isProto1 = FALSE, isProto2 = TRUE, numPathCuts = numPathCuts)$matching_weight
          }
          else
            vecoflengthj[j] = dists[i,j]
        }
        return(vecoflengthj)
      }
      
      # Store distances in dists matrix
      dists = matrix(unlist(listoflengthi), nrow = length(imList), ncol = K, byrow = TRUE)
    }
    
    # Assign graphs to the nearest cluster
    cluster = apply(dists, 1, which.min)
    # Find the distance between each graph and its assigned cluster
    WCD = dists[cbind(1:n,cluster)]
    
    # If using outliers, assign outliers to the outlier cluster
    if (numOutliers != 0){
      # Find any graph that is further from its cluster than the outlierCutoff
      potentialOutliers = which(WCD > outlierCutoff)
      # Assign the (numOutliers-1) graphs furthest from their clusters to
      # the outlier cluster
      outliers = potentialOutliers[rank(-1*WCD[WCD > outlierCutoff]) < numOutliers]
      cluster[outliers] = -1
    }
    
    # If not using outliers
    if (numOutliers == 0){
      # While one or more clusters are empty
      while(length(unique(cluster)) < K)
      { # For each cluster
        for(i in 1:K)
        {
          # If the cluster doesn't contain any graphs
          if(sum(cluster == i) < 1)
          { r = sample(1:n, 1)
            cluster[r] = i
          }
        }
      }
    } # If using outliers
    else {
      # For each cluster
      for(i in 1:K)
      { # If cluster contains fewer than 2 graphs
        if(sum(cluster == i) < 2)
        { # Reassign the graph in cluster i to the outlier cluster
          cluster[which(cluster == i)] = -1
          switch(fillClusters,
                 'random' = {r = sample(1:n, 1)
                            cluster[r] = i},
                 'farthest' = {r = which.max(WCD[cluster != -1])
                              cluster[cluster != -1][r] = i
                 })
        }
      }
    }
    
    # Record the distance between each graph and its cluster center
    WCD = dists[cbind(1:n, cluster)]
    
    # Display current cluster assignments
    print(table(cluster))
    
    # Record which graphs changed clusters
    whichChanged = !(oldCluster == cluster)
    
    # Record which clusters lost or gained graphs
    centerMovedOld = centerMoved
    centerMoved = rep(FALSE, K)
    for(i in 1:K)
    {
      if(any(whichChanged[cluster == i]))
        centerMoved[i] = TRUE
    }
    
    # Recalculate the cluster center if the cluster lost or gained any graphs
    for(i in 1:K)
    {
      if(centerMoved[i])
      { switch(meanGraph,
               'basic' = {
                 centers[[i]] = meanGraphSet(imList[cluster == i], numPathCuts = numPathCuts, meanGraphOrder = meanGraphOrder)},
               'slow_change' = {
                 centers[[i]] = meanGraphSet_slowchange(imList[cluster == i][order(WCD[cluster == i])], numPathCuts = numPathCuts, meanGraphOrder = meanGraphOrder)},
               'kmeans' = {
                 centers[[i]] = meanGraphSet_Kmeans(imList[cluster == i], numPathCuts = numPathCuts, meanGraphOrder = meanGraphOrder)}
               )
      }
    }
    
    # If using outliers, update the outlier cutoff
    if (numOutliers != 0){
      outlierCutoff = gamma*sum(WCD[cluster > 0])/(n-sum(cluster < 0))
    }
    
    # Print the number of graphs that changed clusters
    cat("Changes in iter ", iters, ": ", sum(cluster != oldCluster), "\n")
    
    # End while loop if all graphs stayed in the same cluster or if 
    # the maximum number of iterations is reached
    if(all(cluster == oldCluster) | iters >= iter.max){
      break}

    # Prep for next iteration
    oldCluster = cluster
    
  }  # end while loop
  
  # Print comment to log
  cat("\n Finished run ", outfile, ". (", as.character(Sys.time()), ") \n \n")
  
  # Make template
  template = list(cluster = cluster, centers = centers, K = K, n = n, WithinClustDists = WCD, iters = iters)
  return(template)
}


#' meanGraphSet
#'
#' Calculate a mean graph from a set of input graphs
#'
#' @param imList List graphs
#' @param numPathCuts Integer number of segments to cut each path into
#' @param meanGraphOrder Chose the order in which to add graphs to the mean 
#' graph calculations. The options are 'sequential' and 'random'
#' @return Mean graph
#'
#' @keywords ?
meanGraphSet = function(imList, numPathCuts = 8, meanGraphOrder = 'sequential')
{ 
  # Stop if imList is empty
  if(length(imList) < 1){
    stop("Please specify more than 0 graphs to mean.")
  }
  
  # Choose order in which to add graphs to mean graph calculation
  indices = switch(meanGraphOrder,
                   'random' = sample.int(length(imList)),
                   'sequential' = 1:length(imList) 
                   
  )
  
  # Convert the first graph in the list to a prototype
  meanGraph1 = letterToPrototype(imList[[indices[1]]], numPathCuts = numPathCuts)
  
  # If there is more than one graph in the list
  if(length(imList) > 1)
  { 
    for(i in 2:length(imList))
    { # Calculate the weighted mean graph between the current mean graph and the next graph in the list
      meanGraph1 = weightedMeanGraphs(imList[[indices[i]]], meanGraph1, 1/i, isProto2 = TRUE, numPathCuts = numPathCuts)
    }
  }

  return(meanGraph1)
}


#' meanGraphSet_Kmeans
#'
#' Calculate a mean graph from a set of input graphs
#'
#' @param imList List graphs
#' @param numPathCuts Integer number of segments to cut each path into
#' @param meanGraphOrder Chose the order in which to add graphs to the mean 
#' graph calculations. The options are 'sequential' and 'random'
#' @return Mean graph
#'
#' @keywords ?
meanGraphSet_Kmeans = function(imList, numPathCuts = 8, meanGraphOrder = 'sequential')
{ # Stop if imList is empty
  if(length(imList) < 1){
    stop("Please specify more than 0 graphs to mean.")
  }
  
  # Choose order in which to add graphs to mean graph calculation
  indices = switch(meanGraphOrder,
                   'random' = sample.int(length(imList)),
                   'sequential' = 1:length(imList) 
         
  )
  
  # Initialize
  dists = rep(0, length(imList))
  
  # Convert the first graph in the list to a prototype
  meanGraph1 = letterToPrototype(imList[[indices[1]]], numPathCuts = numPathCuts)
  
  # If there is more than one graph in the list
  if(length(imList) > 1)
  {
    for(i in 2:length(imList))
    { # Calculate the weighted mean graph between the current mean graph and the next graph in the list
      meanGraph1 = weightedMeanGraphs(imList[[indices[i]]], meanGraph1, 1/i, isProto2 = TRUE, numPathCuts = numPathCuts)
    }
  }
  
  # Calculate the distance between each graph in the list and the mean graph
  for(i in 1:length(imList))
  {
    dists[i] = getGraphDistance(meanGraph1, imList[[i]], isProto1 = TRUE, numPathCuts = numPathCuts)$matching_weight
  }
  
  # If there is more than one graph in the list
  if(length(dists)>1)
  { # Find which graphs are closer to the mean graph than the average
    whichDists = which(dists <= mean(dists))
    # Find the distances of the graphs that are closer than the average
    distsShort = dists[whichDists]
  }
  else
  { 
    distsShort = dists
    whichDists = 1
  }
  
  
  probs = rep(0, length(distsShort))
  probs[which.min(distsShort)] = 1
  
  center = letterToPrototype(imList[whichDists][[sample(1:length(distsShort), size = 1, prob = probs)]], numPathCuts = numPathCuts)
  return(center)
}


#' meanGraphSet_slowchange
#'
#' Calculate a mean graph from a set of input graphs
#'
#' @param imList List graphs
#' @param numPathCuts Integer number of segments to cut each path into
#' @param meanGraphOrder Chose the order in which to add graphs to the mean 
#' graph calculations. The options are 'sequential' and 'random'
#' @return Mean graph
#'
#' @keywords ?
meanGraphSet_slowchange = function(imList, numPathCuts = 8, meanGraphOrder = 'sequential')
{
  # Stop if imList is empty
  if(length(imList) < 1){
    stop("Please specify more than 0 graphs to mean.")
  }
  
  # Choose order in which to add graphs to mean graph calculation
  indices = switch(meanGraphOrder,
                   'random' = sample.int(length(imList)),
                   'sequential' = 1:length(imList) 
                   
  )
  
  # Initialize 
  dists = rep(0, length(imList))
  
  # Convert first graph in list to prototype
  meanGraph1 = letterToPrototype(imList[[indices[1]]], numPathCuts = numPathCuts)
  
  # If there is more than one graph in the list
  if(length(imList) > 1)
  {
    for(i in 2:length(imList))
    { # Calculate the weighted mean graph between the current mean graph and the next graph in the list
      meanGraph1 = weightedMeanGraphs(imList[[indices[i]]], meanGraph1, 1/i, isProto2 = TRUE, numPathCuts = numPathCuts)
    }
  }
  
  # For each graph in the list
  for(i in 1:length(imList))
  { # Calculate the distance between the graph and the mean graph
    dists[i] = getGraphDistance(meanGraph1, imList[[i]], isProto1 = TRUE, isProto2 = FALSE, numPathCuts = numPathCuts)$matching_weight
  }
  
  #### IS THIS STATEMENT EVER FALSE?
  if(dists[1] >= quantile(dists, 0)){
    retindex = which.min(dists)
  } else {
    retindex = 1
  }
  
  # Return the chosen graph as a prototype
  center = letterToPrototype(imList[[retindex]], numPathCuts = numPathCuts)
  return(center)
}

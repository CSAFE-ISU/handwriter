### Graph Distance Measure functions

#' distXY
#'
#' Calculate the Euclidean distance between two points
#'
#' @param xy1 Vector of (x,y) coordinates of the first point
#' @param xy2 Vector of (x,y) coordinates of the second point
#' @return Euclidean distance between the two points
#'
#' @keywords ?
distXY = function(xy1, xy2)
{
  return(sqrt((xy2[1] - xy1[1]) ^ 2 + (xy2[2] - xy1[2]) ^ 2))
}


#' dist_loc
#'
#' Calculate the distances between endpoints of one path with the endpoints of a
#' second path. Label the endpoints of the first path p1e1 and p1e2. Label the
#' endpoints of the second path p2e1 and p2e2. Match endpoints in the plus
#' direction: find the Euclidean distance between p1e1 and p2e1 and between p1e2
#' and p2e2. Also match endpoints in the minus direction: find the Euclidean
#' distance between p1e1 and p2e2 and between p1e2 and p2e1. Return the shortest
#' distance in the plus direction and the shortest distance in the minus
#' direction.
#'
#' @param p1e1 Vector of (x,y) coordinates of the first endpoint of the first
#'   path
#' @param p1e2 Vector of (x,y) coordinates of the second endpoint of the first
#'   path
#' @param p2e1 Vector of (x,y) coordinates of the first endpoint of the second
#'   path
#' @param p2e2 Vector of (x,y) coordinates of the second endpoint of the second
#'   path
#' @return Distances between endpoints
#'
#' @keywords ?
dist_loc = function(p1e1, p1e2, p2e1, p2e2)
{
  # Find minimum distance between the two pairs of endpoints using direction 1
  plus_pair1 = distXY(p1e1, p2e1)
  plus_pair2 = distXY(p1e2, p2e2)
  d_loc_plus = min(plus_pair1, plus_pair2)
  
  # Find minimum distance between the two pairs of endpoints using direction 2
  minus_pair1 = distXY(p1e1, p2e2)
  minus_pair2 = distXY(p1e2, p2e1)
  d_loc_minus = min(minus_pair1, minus_pair2)
  
  d_loc = c(d_loc_plus, d_loc_minus)
  return (d_loc)
}


#' dist_sld
#'
#' Calculate the difference between the straigt line distances of two paths.
#' The straightline distance for a path is the Euclidean distance between its
#' two endpoints.
#'
#' @param p1e1 Vector of (x,y) coordinates of the first endpoint of the first
#'   path
#' @param p1e2 Vector of (x,y) coordinates of the second endpoint of the first
#'   path
#' @param p2e1 Vector of (x,y) coordinates of the first endpoint of the second
#'   path
#' @param p2e2 Vector of (x,y) coordinates of the second endpoint of the second
#'   path
#' @return Difference between the straight line distances of the two paths
#'
#' @keywords ?
dist_sld = function(p1e1, p1e2, p2e1, p2e2)
{
  # Straight line distance of the first path
  p1_length = distXY(p1e1, p1e2)
  
  # Straight line distance of the second path
  p2_length = distXY(p2e1, p2e2)
  
  # The difference in the straight line distances
  d_sld = abs(p1_length - p2_length)
  return(d_sld)
}


#' dist_sh
#'
#' Calculate the difference in shapes of two paths. Measure the shape of a path
#' by cutting it into numPathCuts segments of equal length. Call the cutpoints on
#' the path edge-points. Also cut the straight line between the path's endpoints
#' into numPathCuts segments of equal length. Call the cut points on the straight
#' line line-points. Subtact the line-points from the edge-points to get the
#' shape-points for the path. Find the Euclidean distances between the
#' shape-points of the two paths by pairing the shape-points in the plus
#' direction: find the distance between the i-th shape-point of the first path
#' and the i-th shape-point of the second path, for i=1,2,...,numPathCuts-1. Also
#' find the Euclidean distances between the shape-points of the two paths in the
#' minus direction: find the distance between the i-th shape-point of the first path
#' and the (numPathCuts-i)-th shape-point of the second path, for i=1,2,...,numPathCuts-1.
#' Return the shape distances in the plus and minus directions.
#'
#' @param graphInfo List of info created by getGraphInfo function
#' @param numPathCuts Integer number of segments to cut the paths into
#' @param path_ii Integer path number of the first path
#' @param path_jj Integer path number of the second path
#' @return Shape distances between the two paths
#'
#' @keywords ?
dist_sh = function(graphInfo, numPathCuts, path_ii, path_jj)
{
  # Initialize
  path_ii_sh = graphInfo$pq1[,,path_ii]
  path_jj_sh = graphInfo$pq2[,,path_jj]
  graphInfo_pe1 = graphInfo$pe1[, , path_ii]
  graphInfo_pe2 = graphInfo$pe2[, , path_jj]

  # For each cut in the path
  # Find the shape points for the ii-th path. Subtract the i-th
    # cutpoint on the straight line between the ii-th path's endpoints
    # from the i-th cutpoint on the path itself. Denoted s_i^(e_1) in
    # paper.
  tabii = t((graphInfo_pe1[2,]-graphInfo_pe1[1,]) %*% matrix(1:(numPathCuts - 1)/numPathCuts,1))
  path_ii_sh  = path_ii_sh - (tabii+graphInfo_pe1[1,col(tabii)])
  # # Find the shape points on the jj-th path. Subtract the i-th cutpoint
  #   # on the straight line between the jj-th path's endpoints from the
  #   # i-th cutpoint on the path itself. Denoted s_i^(e_2) in
  #   # paper.
  tabjj = t((graphInfo_pe2[2,]-graphInfo_pe2[1,]) %*% matrix(1:(numPathCuts - 1)/numPathCuts,1))
  path_jj_sh  = path_jj_sh - (tabjj+graphInfo_pe2[1,col(tabjj)])  
  
  # Find the distance the i-th shape points in the plus direction. Denoted d_sh+ in paper.
  d_sh_plus = sum(sqrt(Rfast::rowsums((path_jj_sh-path_ii_sh)^2)))/(numPathCuts - 1)

  # Find the distance the i-th shape points in the minus direction. Denoted d_sh+ in paper.
  d_sh_minus = sum(sqrt(Rfast::rowsums((path_jj_sh[nrow(path_jj_sh):1,]-path_ii_sh)^2)))/(numPathCuts - 1)

  # Shape distance
  d_sh = c(d_sh_plus, d_sh_minus)
  return(d_sh)
}


# Location Functions ------------------------------------------------------

#' pathToRC
#'
#' Convert the index number locations of graph's paths to (x,y) coordinates with
#' (0,0) in the bottom left corner of the graph's image.
#'
#' @param pathList List of index number locations of paths in a graph
#' @param dims Dimensions of the graph's image
#' @return Vector of (x,y) coordinates of the graph's paths
#'
#' @keywords ?
pathToRC = function(pathList, dims)
{
  return(cbind((pathList - 1) %/% dims[1] + 1, dims[1] - (pathList - 1) %% dims[1]))
}


# misc helper functions --------------------------------------------------------

#' pointLineProportionVect
#'
#' Subtract the n-th cutpoint on the straight line between the path's endpoints
#' from the n-th cutpoint on the path itself.
#'
#' @param endpt1 Vector of (x,y) coordinates of one endpoint of a path
#' @param endpt2 Vector of (x,y) coordinates of the other endpoint of a path
#' @param edgecut_prop_n Proportion of the path between the first endpoint and the n-th cut point
#' @param edgecutpt_n The (x,y) coordinates of the n-th cut point on the path
#' @return Vector of (x,y) coordinates
#'
#' @keywords ?
pointLineProportionVect = function(endpt1,
                                   endpt2,
                                   edgecut_prop_n,
                                   edgecutpt_n)
{
  # Find the n-th cutpoint on the straight line between the path's endpoints.
  linecutpt = endpt1 + (endpt2 - endpt1) * edgecut_prop_n
  
  # Subtract the n-th cutpoint on the straight line between
  # from the n-th cutpoint on the path itself.
  return(edgecutpt_n - linecutpt)
}


#' solveLP
#'
#' Use linear programming to solve a constrained minimization optimization
#' problem to find the optimal pairings of paths between two graphs.
#'
#' @param dists ?
#' @return A list of information about the optimal pairings
#'
#' @keywords ?
solveLP = function(dists)
{
  # Number of rows
  dims = dim(dists)[1]
  
  # Set distances as costs
  costs = c(dists)
  
  # Initialize constraints matrix. NOTE: There are dims*dims possible edge pairs where
  # one edge comes from path 1 and the other edge comes from path 2. Each column of the
  # matrix considers one edge pair. The matrix has a row for each edge.
  # Fill the first dims rows with repeated (dims x dims) identity matrices.
  A <- matrix(rbind(diag(dims),matrix(0,nrow=dims,ncol=dims)),nrow=2 * dims,ncol=dims * dims)
  
  # Set more entries in the matrix to 1.
  A[cbind(row=dims + rep(1:dims,each=dims),col=1:dims^2)] = 1
  
  # Make a vector of ones with length 2*dims
  b = rep(1, 2 * dims)
  
  # Solve the optimatize problem
  x = lpSolve::lp(
    direction = "min",  # direction of optimatization
    objective.in = costs,  # coefficients of objective function
    const.mat = A,  # matrix of constraints
    const.dir = rep('=', 2 * dims),  # direction of constraints
    const.rhs = b,  # right hand sides of constraints
    all.bin = TRUE  # all variables should be binary (either an edge is included=1, or excluded=0)
  )  
  
  return(list(
    matching_weight = x$objval,
    matching_size = sum(x$solution),
    matching =  order((which(x$solution != 0) - 1) %% dims + 1)
  ))
}


#' letterToPrototype
#'
#' Convert the graph to a prototype graph, a graph that serves as a cluster center.
#'
#' @param letter A graph from a handwriting sample
#' @param numPathCuts Number of segments to cut the path(s) into
#' @return List of pathEnds, pathQuarters, and pathCenters given as (x,y) coordinates
#' with the graph centroid at (0,0). The returned list also contains path lengths.
#' pathQuarters gives the (x,y) coordinates of the path at the cut points and despite the
#' name, the path might not be cut into quarters.
#'
#' @keywords ?
letterToPrototype = function(letter, numPathCuts = 8)
{
  pathCount = length(letter$allPaths)
  resGraph = list(pathEnds = matrix(NA, ncol = 4, nrow = pathCount), pathQuarters = matrix(NA, ncol = 2*(numPathCuts-1), nrow = pathCount), pathCenter = matrix(NA, ncol = 2, nrow = pathCount), lengths = rep(NA, pathCount)) # identical to handwriter
  
  dims = dim(letter$image)
  for(i in 1:pathCount)
  {
    pathLength = length(letter$allPaths[[i]])
    resGraph$pathEnds[i,1:2] = c((letter$allPaths[[i]][1]-1) %/% dims[1] + 1, dims[1] - (letter$allPaths[[i]][1]-1) %% dims[1]) - letter$centroid
    resGraph$pathEnds[i,3:4] = c((letter$allPaths[[i]][pathLength]-1) %/% dims[1] + 1, dims[1] - (letter$allPaths[[i]][pathLength]-1) %% dims[1]) - letter$centroid
    
    pathRC = PathToRC(letter$allPaths[[i]], dim(letter$image))
    resGraph$pathCenter[i,] = c(mean(pathRC[,1]), mean(pathRC[,2])) - letter$centroid
    
    for(j in 1:(numPathCuts-1))
    {
      quartTemp = letter$allPaths[[i]][ceiling(pathLength/(numPathCuts/j))]
      resGraph$pathQuarters[i,(j-1)*2 + 1:2] = c((quartTemp-1) %/% dims[1] + 1, dims[1] - (quartTemp-1) %% dims[1]) - letter$centroid
      # resGraph$pathQuarters[i,(j-1)*2 + 1:2] = pointLineProportionVect(resGraph$pathEnds[i,1:2], resGraph$pathEnds[i,3:4], j/numPathCuts, resGraph$pathQuarters[i,(j-1)*2 + 1:2])
    }
    
    #resGraph$pathQuarters[i,] = resGraph$pathQuarters[i,] - matrix(rep(resGraph$pathEnds[i,1:2], numPathCuts-1), nrow= 1)
    resGraph$lengths[i] = pathLength
  }
  return(resGraph)
}


#' getGraphInfo
#'
#' Gather and format the parameter values need to calculate the distance between
#' two graphs.
#'
#' @param imageList1 A graph 
#' @param imageList2 A graph
#' @param isProto1 True or false. Is the graph information in prototype format?
#' @param isProto2 True or false. Is the graph information in prototype format?
#' @param numPathCuts An integer number of cuts to make when comparing segments of paths
#' @return List of formatted parameters
#' 
#' @keywords ?
getGraphInfo = function(imageList1, imageList2, isProto1, isProto2, numPathCuts)
{
  # Find number of paths in each graph
  numPaths1 = max(length(imageList1$allPaths), dim(imageList1$pathEnds)[1])
  numPaths2 = max(length(imageList2$allPaths), dim(imageList2$pathEnds)[1])
  
  # Find the sum of the lengths of paths in graph 1
  letterSize = rep(0, 2)
  if (!isProto1)
  {
    letterSize[1] = sum(unlist(lapply(imageList1$allPaths, length)))
  } else if (isProto1)
  {
    letterSize[1] = sum(imageList1$lengths)
  }
  
  # Find the sum of the lengths of paths in graph 2
  if (!isProto2)
  {
    letterSize[2] = sum(unlist(lapply(imageList2$allPaths, length)))
  } else if (isProto2)
  {
    letterSize[2] = sum(imageList2$lengths)
  }
  
  # Find the max number of paths between the two grpahs
  pathCheckNum = max(numPaths1, numPaths2)
  
  # Initialize. Will store path cut points
  pq1 = pq2 = array(NA,dim=c(numPathCuts - 1,2,pathCheckNum))
  
  # Initialize. Will store path end points
  pe1 = pe2 = array(NA,dim=c(2,2,pathCheckNum))
  
  # Initialize. Will store path centers
  cent1 = cent2 = array(NA, c(1, 2, pathCheckNum))
  
  # Initialize. Will store path lengths
  len1 = len2 = rep(0, pathCheckNum)
  
  # Initialize. For each possible pair an edge from graph 1 with an edge from graph 2,
  # will store whether the pair "matches" i.e. produces the smallest distance between the
  # two graphs
  pathEndPointsMatch = rep(TRUE, pathCheckNum ^ 2)
  
  # Initialize. Will store the weights, aka distances, between the two graphs
  weights = matrix(NA, ncol = pathCheckNum, nrow = pathCheckNum)
  
  # Update len1, pe1, cent1, and pq1 for graph 1
  for (ii in 1:pathCheckNum)
  {
    if (isProto1 & ii <= numPaths1)
    {
      len1[ii] = imageList1$lengths[ii]
      pe1[, , ii] = matrix(
        imageList1$pathEnds[ii, ],
        ncol = 2,
        nrow = 2,
        byrow = TRUE
      )
      cent1[1, , ii] = imageList1$pathCenter[ii, ]
      pq1[, , ii] <- matrix(imageList1$pathQuarters[ii,][1:(2*(numPathCuts - 1))],numPathCuts - 1,byrow=TRUE)
    }
    else if (ii <= numPaths1)
    {
      len1[ii] = length(imageList1$allPaths[[ii]])
      
      pe1[, , ii] = imageList1$pathEndsrc[[ii]]
      
      pathRC = pathToRC(imageList1$allPaths[[ii]], dim(imageList1$image))
      cent1[1, , ii] = colMeans(pathRC) - imageList1$centroid
      pq1[, , ii] = Rfast::eachrow(pathRC[ceiling(length(imageList1$allPaths[[ii]]) / (numPathCuts /1:(numPathCuts-1))),],imageList1$centroid,"-")
    }
  }
  
  # Update len2, pe2, cent2, and pq2 for graph 2
  for (jj in 1:pathCheckNum)
  {
    if (isProto2 & jj <= numPaths2)
    {
      len2[jj] = imageList2$lengths[jj]
      pe2[, , jj] = matrix(
        imageList2$pathEnds[jj, ],
        ncol = 2,
        nrow = 2,
        byrow = TRUE
      )
      
      cent2[1, , jj] = imageList2$pathCenter[jj, ]
      pq2[, , jj] <- matrix(imageList2$pathQuarters[jj,][1:(2*(numPathCuts - 1))],numPathCuts - 1,byrow=TRUE)
    }
    else if (jj <= numPaths2)
    {
      len2[jj] = length(imageList2$allPaths[[jj]])
      
      pe2[, , jj] = imageList2$pathEndsrc[[jj]]
      
      pathRC = pathToRC(imageList2$allPaths[[jj]], dim(imageList2$image))
      cent2[1, , jj] = colMeans(pathRC) - imageList2$centroid
      pq2[, , jj] = Rfast::eachrow(pathRC[ceiling(length(imageList2$allPaths[[jj]]) / (numPathCuts /1:(numPathCuts-1))),],imageList2$centroid,"-")
    }
  }
  
  # Store parameters in a list
  graphInfo = list(
    numPaths1 = numPaths1,
    numPaths2 = numPaths2,
    pe1 = pe1,
    pe2 = pe2,
    pq1 = pq1,
    pq2 = pq2,
    cent1 = cent1,
    cent2 = cent2,
    len1 = len1,
    len2 = len2,
    letterSize = letterSize,
    pathCheckNum = pathCheckNum,
    pathEndPointsMatch = pathEndPointsMatch,
    weights = weights
  )
  return(graphInfo)
}


# Graph Distances --------------------------------------------------------

getAllPairsDistances = function(graphInfo, numPathCuts)
{
  for (ii in 1:graphInfo$pathCheckNum)
  {
    for (jj in 1:graphInfo$pathCheckNum)
    {
      if (ii > graphInfo$numPaths1)
      {
        # Set the weight if ghost edge in  graph 1
        graphInfo$weights[ii, jj] = graphInfo$len2[jj] ^ 2
      }
      else if (jj > graphInfo$numPaths2)
      {
        # Set the weight if ghost edge in graph 2
        graphInfo$weights[ii, jj] = graphInfo$len1[ii] ^ 2
      }
      else
        # plus is direction 1, minus is direction 2
      {
        # Distance between endpoint locations
        d = dist_loc(
          p1e1 = graphInfo$pe1[1, , ii],
          p1e2 = graphInfo$pe1[2, , ii],
          p2e1 = graphInfo$pe2[1, , jj],
          p2e2 = graphInfo$pe2[2, , jj]
        )
        
        # Add straight line distance
        d = d + 0.5 * dist_sld(
          p1e1 = graphInfo$pe1[1, , ii],
          p1e2 = graphInfo$pe1[2, , ii],
          p2e1 = graphInfo$pe2[1, , jj],
          p2e2 = graphInfo$pe2[2, , jj]
        )
        
        # Add shape distance
        d = d + 2 * dist_sh(
          graphInfo = graphInfo,
          numPathCuts = numPathCuts,
          path_ii = ii,
          path_jj = jj
        )
        
        # Do ends match?
        if (d[1] <= d[2]) {
          graphInfo$pathEndPointsMatch[jj + (ii - 1) * graphInfo$pathCheckNum] = TRUE
        } else {
          graphInfo$pathEndPointsMatch[jj + (ii - 1) * graphInfo$pathCheckNum] = FALSE
        }
        
        # Record weights for edge ii from the first graph and edge jj from the second graph
        graphInfo$weights[ii, jj] = min(d)
      }  # end else statement
      
      # Scale the weights for edge ii from the first graph and edge jj from the second graph
      graphInfo$weights[ii, jj] = graphInfo$weights[ii, jj] *
        ((graphInfo$len1[ii] / graphInfo[['letterSize']][1] + graphInfo$len2[jj] /
            graphInfo[['letterSize']][2]) / 2)
    }  # end for jj loop
  }  # end for ii loop
  
  return(graphInfo)
}


getGraphDistance = function(imageList1, imageList2, isProto1 = FALSE, isProto2 = FALSE, numPathCuts = 8)
{
  if (numPathCuts < 1)
  {
    numPathCuts = 1
  }
  
  # Find and format parameter values
  graph_info = getGraphInfo(
    imageList1,
    imageList2,
    isProto1 = isProto1,
    isProto2 = isProto2,
    numPathCuts = numPathCuts
  )
  
  # For each pair of edges -- edge ii from graph 1 and edge jj from graph 2 --
  # calculate the distance between the edges.
  graph_info = getAllPairsDistances(graphInfo = graph_info, numPathCuts = numPathCuts)
  
  # Find the optimal edge pairings between the two graphs with linear programming.
  distance12 = solveLP(graph_info[['weights']] + .00001)
  
  # Scale the weight by the matching size
  distance12$matching_weight = distance12$matching_weight - .00001 * distance12$matching_size
  return(distance12)
}


# weightedMeanGraphs ------------------------------------------------------


#' weightedMeanGraphs
#'
#' Description
#'
#' @param imageList1 List of images
#' @param imageList2 List of images
#' @param p1 ?
#' @param isProto1 Whether imageList1 is exemplars
#' @param isProto2 Whether imageList2 is exemplars
#' @param numPathCuts Number of cuts
#' @return ?
#'
#' @keywords ?
weightedMeanGraphs = function(imageList1, imageList2, p1, isProto1 = FALSE, isProto2 = FALSE, numPathCuts = 8)
{
  graph_info = getGraphInfo(imageList1, imageList2, isProto1, isProto2, numPathCuts)
  
  # ---- Graph Distance
  graph_info = getAllPairsDistances(graphInfo = graph_info, numPathCuts = numPathCuts)
  
  # Find the optimal edge pairings between the two graphs with linear programming.
  distance12 = solveLP(graph_info$weights + .00001)
  
  # Scale the weight by the matching size
  distance12$matching_weight = distance12$matching_weight - .00001 * distance12$matching_size
  
  ### Weighted mean of
  # 1) Edge end points
  # 2) Edge quartiles
  # 3) Edge lengths
  
  match1 = distance12$matching
  
  # Initialize
  resGraph = list(
    pathEnds = matrix(NA, ncol = 4, nrow = graph_info$pathCheckNum),
    pathQuarters = matrix(NA, ncol = 2 * (numPathCuts - 1), nrow = graph_info$pathCheckNum),
    pathCenter = matrix(NA, ncol = 2, nrow = graph_info$pathCheckNum),
    lengths = rep(NA, graph_info$pathCheckNum)
  )
  
  for (i in 1:graph_info$pathCheckNum)
  { # If edge is a ghost edge in graph 1
    if (i > graph_info$numPaths1)
    {
      graph_info$pe1[, , i] = matrix(c(graph_info$pe2[ifelse(graph_info$pathEndPointsMatch[match1[i] + (i - 1) * graph_info$pathCheckNum], 1, 2), , match1[i]], graph_info$pe2[ifelse(graph_info$pathEndPointsMatch[match1[i] + (i - 1) * graph_info$pathCheckNum], 2, 1), , match1[i]]),
                          ncol = 2,
                          nrow = 2,
                          byrow = TRUE)
      graph_info$pq1[, , i] = graph_info$pq2[ifelse(rep(graph_info$pathEndPointsMatch[match1[i] + (i - 1) * graph_info$pathCheckNum], (numPathCuts - 1)),
                              1:(numPathCuts - 1),
                              (numPathCuts - 1):1), , match1[i]]
      graph_info$cent1[1, , i] = graph_info$cent2[1, , match1[i]]
      resGraph$lengths[i] = 0 * p1 + graph_info$len2[match1[i]] * (1 - p1)
    } 
    else if (match1[i] > graph_info$numPaths2)
    {
      graph_info$pe2[, , match1[i]] = matrix(c(graph_info$pe1[1, , i], graph_info$pe1[2, , i]),
                                  ncol = 2,
                                  nrow = 2,
                                  byrow = TRUE)
      graph_info$pq2[, , match1[i]] = graph_info$pq1[1:(numPathCuts - 1), , i]
      graph_info$cent2[1, , match1[i]] = graph_info$cent1[1, , i]
      resGraph$lengths[i] = graph_info$len1[i] * p1 + 0 * (1 - p1)
    }
    else if (i <= graph_info$numPaths1 & match1[i] <= graph_info$numPaths2)
    {
      resGraph$lengths[i] = graph_info$len1[i] * p1 + graph_info$len2[match1[i]] * (1 - p1)
    }
    
    resGraph$pathEnds[i, 1:2] = graph_info$pe1[1, , i] * p1 + graph_info$pe2[ifelse(graph_info$pathEndPointsMatch[match1[i] + (i - 1) * graph_info$pathCheckNum], 1, 2), , match1[i]] * (1 - p1)
    resGraph$pathEnds[i, 3:4] = graph_info$pe1[2, , i] * p1 + graph_info$pe2[ifelse(graph_info$pathEndPointsMatch[match1[i] + (i - 1) * graph_info$pathCheckNum], 2, 1), , match1[i]] * (1 - p1)
    
    resGraph$pathCenter[i, ] = graph_info$cent1[1, , i] * p1 + graph_info$cent2[1, , match1[i]] *
      (1 - p1)
    
    for (j in 1:(numPathCuts - 1))
    {
      resGraph$pathQuarters[i, (j - 1) * 2 + (1:2)] = (pointLineProportionVect(graph_info$pe1[1, , i], graph_info$pe1[2, , i], j / numPathCuts, graph_info$pq1[j, , i])) * p1 +
        (pointLineProportionVect(graph_info$pe2[ifelse(graph_info$pathEndPointsMatch[match1[i] + (i - 1) * graph_info$pathCheckNum], 1, 2), , match1[i]], graph_info$pe2[ifelse(graph_info$pathEndPointsMatch[match1[i] + (i -1) * graph_info$pathCheckNum], 2, 1), , match1[i]], j / numPathCuts, graph_info$pq2[ifelse(graph_info$pathEndPointsMatch[match1[i] + (i - 1) * graph_info$pathCheckNum], j, numPathCuts - j), , match1[i]])) * (1 - p1) + resGraph$pathEnds[i, 1:2] + (resGraph$pathEnds[i, 3:4] -resGraph$pathEnds[i, 1:2]) * (j / numPathCuts)
    }
  }
  
  resGraph$numPathCuts = numPathCuts
  
  minimumPathLength = 1
  
  if (any(resGraph$lengths < minimumPathLength) & length(resGraph$lengths) > 1)
  {
    if (!all(resGraph$lengths < minimumPathLength))
    {
      toDelete = which(resGraph$lengths < minimumPathLength)
      resGraph$pathEnds = matrix(resGraph$pathEnds[-toDelete, ], ncol = 4)
      resGraph$pathQuarters = matrix(resGraph$pathQuarters[-toDelete, ], ncol = 2 *
                                       (numPathCuts - 1))
      resGraph$lengths = resGraph$lengths[-toDelete]
    }
  }
  return(resGraph)
}

getPaths <- function(comps, dims) {
  getNonLoopPathsForComponent <- function(adjm, skeleton0, nodeList) {
    # ALGORITHM:
    # 1. For each pair of nodes, n1 and n2, in adjm:
    #     2. CALCULATE STEP: 
    #          3. Find the shortest distance between n1 and n2 where an edge
    #             has weight 1 if either vertex is a node and 0.00001 otherwise.
    #          4. While the shortest distance d satisfies 1 <= d < 3:
    #               5. Get the names of the vertices in the shortest path p between n1 and n2 as a vector
    #               6. Get the number of vertices n in the shortest path p
    #               7. Add p to the paths list 
    #               8. UPDATE STEP: 
    #                    9. If p has more than 2 vertices, delete the middle edge from skeleton0. If p has exactly 2 vertices,
    #                       delete the single edge between the 2 vertices from skeleton0. Otherwise, return an error (Does this ever
    #                       occur? Should this be a break instead?).
    #                   10. Recalculate the shortest distance d between n1 and n2
    # 11. return paths list
    
    paths <- list()
    if (dim(adjm)[1] == 0) {
      return(NULL)
    }
    
    # update skeleton_df0 with node_only_dist=1 if one or both of the edge's vertices is a node and 0.00001 otherwise
    skeleton_df0 <- igraph::as_data_frame(skeleton0)
    skeleton_df0$node_only_dist <- ifelse(skeleton_df0$from %in% nodeList | skeleton_df0$to %in% nodeList, 1, 0.00001)
    skeleton0 <- igraph::graph_from_data_frame(skeleton_df0, directed = FALSE)
    
    # NOTE: each pair of nodes in adjm satisfies the condition that the shortest path
    # between the two nodes does not run through another node
    for (i in 1:dim(adjm)[1])
    {
      fromNode <- as.character(format(adjm[i, 1], scientific = FALSE, trim = TRUE))
      toNode <- as.character(format(adjm[i, 2], scientific = FALSE, trim = TRUE))
      
      # calculate distances of shortest path between fromNode and toNode using node_only_dist
      dists <- igraph::distances(skeleton0, v = fromNode, to = toNode, weights = igraph::E(skeleton0)$node_only_dist)
      while (dists < 3 & dists >= 1) {
        # CALCULATE STEP:
        # get shortest path between fromNode and toNode
        shortest <- igraph::shortest_paths(skeleton0, from = fromNode, to = toNode, weights = igraph::E(skeleton0)$node_only_dist)
        # count vertices in shortest path, including fromNode and toNode
        len <- length(unlist(shortest[[1]]))
        # add the shortest path to the list
        paths <- c(paths, list(as.numeric(names(shortest$vpath[[1]]))))
        
        # UPDATE STEP: If there are more than two vertices in the path, delete the
        # middle edge. If there are 2 vertices in the path, delete the single edge
        # between the vertices. 
        if (len > 2) {
          skeleton0 <- igraph::delete_edges(skeleton0, paste0(names(shortest$vpath[[1]])[len %/% 2], "|", names(shortest$vpath[[1]])[len %/% 2 + 1]))
        } else if (len == 2) {
          skeleton0 <- igraph::delete_edges(skeleton0, paste0(names(shortest$vpath[[1]])[1], "|", names(shortest$vpath[[1]])[2]))
        } else {
          stop("There must be some mistake. Single node should have node_only_dist path length of 0.")
        }
        # recalculate shortest path distance on updated graph
        dists <- igraph::distances(skeleton0, v = fromNode, to = toNode, weights = igraph::E(skeleton0)$node_only_dist)
      }
    }
    
    return(paths)
  }
  
  getAllNonLoopPaths <- function(comps) {
    # get non-loop paths for all components
    n <- length(comps)
    for (i in 1:n){
      paths <- getNonLoopPathsForComponent(comps[[i]]$nodes$adjm, 
                                           comps[[i]]$skeletons$skeleton0, 
                                           comps[[i]]$nodes$nodeList)
      if (is.null(paths)){
        comps[[i]]$paths$pathList <- list()
      } else {
        comps[[i]]$paths$pathList <- paths
      }
    }
    return(comps)
  }
  
  getLoopsForComponent <- function(nodeList, skeleton, skeleton0, pathList, dims) {
    # NOTE: pathList contains all the unique, shortest, non-loop paths in the component
    vertexNames <- names(igraph::V(skeleton0))
    
    fullSkeleton0 <- skeleton0
    
    # list all vertices in the paths except the first and last vertex in each path
    used <- unlist(lapply(pathList, function(x) {
      x[-c(1, length(x))]  # drop the first and last vertex in the path
    }))
    
    # create a skeleton graph of the vertices not in the used list. Add an edge
    # between each pair of vertices except when both vertices are nodes.
    unused <- as.numeric(vertexNames)[which(!(as.numeric(vertexNames) %in% used))]
    unusedAdj <- matrix(1, ncol = length(unused), nrow = length(unused))
    colnames(unusedAdj) <- as.character(format(unused, scientific = FALSE, trim = TRUE))
    rownames(unusedAdj) <- as.character(format(unused, scientific = FALSE, trim = TRUE))
    if (length(nodeList) > 1) {
      # select entries where the row is a node and the column is also a node and set the value to 0
      unusedAdj[, which(unused %in% nodeList)][which(unused %in% nodeList), ] <- 0
    } else {
      # select the entry in the node's row and column and set the value to 0
      unusedAdj[which(unused %in% nodeList), which(unused %in% nodeList)] <- 0
    }
    unusedSkeleton <- igraph::graph_from_adjacency_matrix(unusedAdj, mode = "undirected")
    
    # update skeleton0, keeping only the edges that are in skeleton0 and
    # unusedSkeleton. Do the same for skeleton
    skeleton0 <- igraph::intersection(skeleton0, unusedSkeleton, keep.all.vertices = TRUE)
    skeleton <- igraph::intersection(skeleton, skeleton0, byname = TRUE, keep.all.vertices = TRUE)
    
    loopList <- list()
    
    # Find loops that start and end at a node ---- 
    # 1. find any nodes that are in "unused" and have 2 or more attached edges.
    # 2. for each node n1 in STEP 1:
    #     3. list the node's direct neighbor (n1, n2, n3, ...)
    #     4. delete the edge in skeleton between the node n1 and its first neighbor in the list n2
    #     5. if n1 and n2 are still connected in skeleton: 
    #            6. find the shortest path p in skeleton from n1 to n2
    #            7. n1 is the first vertex in p and n2 is the last. Add n1 to the end of p to add
    #               back the edge that we deleted between n1 and n2 in STEP 4. p is now a loop that 
    #               begins and ends at node n1.
    #            8. add p to loopList
    # NOTE: the start and end node might have more than 2 attached edges (why do
    # we give a warning about this?)
    
    # list the node(s) in skeleton0 that have more than one edge
    check <- unused[degree(skeleton0, as.character(format(unused, scientific = FALSE, trim = TRUE))) > 1]
    check <- check[which(check %in% nodeList)]
    tryCatch(
      expr = {
        # list the node(s) to check and all of their immediate neighbors, i.e.
        # all vertices connected to the node(s) by a single edge
        neighbors <- igraph::neighborhood(skeleton, nodes = as.character(check))
        
        # get paths that start and end at the same point, where that point is a node in nodeList
        if (length(neighbors) > 0) {
          for (i in 1:length(neighbors)) {
            # get vector of neighbors for node i
            neigh <- as.numeric(names(neighbors[[i]]))
            # delete the edge between the node i (the first vertex in the list) and the second vertex in neigh
            skeleton <- igraph::delete_edges(skeleton, paste0(neigh[1], "|", neigh[2]))
            # if the first and second vertices are still connected, add the path to the loop list
            if (igraph::distances(skeleton, v = as.character(neigh[1]), to = as.character(neigh[2])) < Inf) {
              newPath <- as.numeric(names(unlist(igraph::shortest_paths(skeleton, from = format(neigh[1], scientific = FALSE), to = format(neigh[2], scientific = FALSE), weights = igraph::E(skeleton)$pen_dist)$vpath)))
              loopList <- append(loopList, list(c(newPath, newPath[1])))
            }
          }
        }
      },
      error = function(e) {
        message("Error in loops... skipping...")
      }
    )
    
    # set the used vertices list as the unique vertices in pathList and loopList
    used <- as.numeric(unique(c(unlist(pathList), unlist(loopList))))
    # update the "unused" list
    unused <- as.numeric(vertexNames)[which(!(as.numeric(vertexNames) %in% used))]
    
    # Search for more loops ---- 
    
    # Create a subgraph of skeleton0 where the vertices are the nodes plus the
    # vertices in "unused" and each vertex has at least 2 edges. Find a "root" -
    # a node in the subgraph that has two or more neighbors that are in the
    # "unused" vector but the node itself might not be in the "unused" vector.
    # Perform a depth-first search through the subgraph vertices starting at the
    # root to find the first part of the loop. To find the second part of the
    # loop, search fullSkeleton0 (the original skeleton0) to find the shortest
    # path between the the last vertex in the first part of the loop and the
    # root. The second loop part is allowed to use vertices that have already
    # been used in a path or loop, including the first loop part. Combine the
    # first and second part of the loop to form the full loop. Vertices next to
    # the root might be visited twice in the loop.
    
    # create a subgraph of skeleton0 where the vertices are the nodes plus the
    # vertices in "unused".
    remaining0 <- igraph::induced_subgraph(skeleton0, vids = format(c(unused, nodeList), scientific = FALSE, trim = TRUE))
    # for each vertex in the subgraph, count the number of direct neighbors
    numNeighbors <- lapply(igraph::neighborhood(remaining0, nodes = igraph::V(remaining0)), length)
    # create a new subgraph from remaining0 that only keeps the vertices that have 2 or more neighbors
    remaining0 <- igraph::induced_subgraph(remaining0, vids = igraph::V(remaining0)[numNeighbors > 1])
    # find nodes in the new subgraph that have 2 or more neighbors
    roots <- format(nodeList[which(nodeList %in% names(igraph::V(remaining0)))], scientific = FALSE, trim = TRUE)
    if (length(roots) > 0) {
      for (i in 1:length(roots))
      { # perform a depth-first search starting from root i and get the vertex
        # IDs in the order in which they were visited (unreachable = FALSE means
        # search does not visit vertices that are unreachable from root i)
        loopPart1 <- names(na.omit(igraph::dfs(remaining0, roots[i], unreachable = FALSE)$order))
        # find the shortest path in the original skeleton0 (fullSkeleton0) from
        # the last vertex in loopPart1 to root i. Remove the last vertex in
        # loopPart1 from the vector so that it isn't listed twice in the loop.
        loopPart2 <- igraph::shortest_paths(fullSkeleton0, from = loopPart1[length(loopPart1)], to = roots[i])$vpath[[1]][-1]
        loopList <- append(loopList, list(as.numeric(c(loopPart1, names(loopPart2)))))
      }
    }
    used <- as.numeric(unique(c(unlist(pathList), unlist(loopList))))
    unused <- as.numeric(vertexNames)[which(!(as.numeric(vertexNames) %in% used))]
    
    # Search for even more loops ---- 
    
    # now get loops that are more difficult. They are close to nodes, but
    # separated by paths already found previously. Have to dig a little further.
    remaining0 <- igraph::induced_subgraph(skeleton0, vids = format(unused, scientific = FALSE, trim = TRUE))
    if (length(unused) > 0) {
      neighbors <- igraph::neighborhood(fullSkeleton0, order = 2, nodes = format(unused, scientific = FALSE, trim = TRUE))
      ends <- lapply(neighbors, function(x) nodeList[which(format(nodeList, scientific = FALSE, trim = TRUE) %in% names(x))])
      roots <- format(unique(unlist(ends)), scientific = FALSE, trim = TRUE)
      if (length(roots) > 0) {
        ends <- igraph::neighborhood(fullSkeleton0, order = 2, nodes = roots)
        ends <- unlist(lapply(ends, function(x) names(x)[which(names(x) %in% format(unused, scientific = FALSE, trim = TRUE))][1]))
        
        for (i in 1:length(roots))
        {
          loopPart1 <- names(na.omit(igraph::dfs(remaining0, ends[i], unreachable = FALSE)$order))
          loopPart2a <- igraph::shortest_paths(fullSkeleton0, from = loopPart1[1], to = roots[i])$vpath[[1]][-1]
          loopPart2b <- igraph::shortest_paths(fullSkeleton0, from = loopPart1[length(loopPart1)], to = roots[i])$vpath[[1]][-1]
          loopList <- append(loopList, list(as.numeric(c(rev(names(loopPart2a)), loopPart1, names(loopPart2b)))))
        }
      }
    }
    
    
    ## And a little deeper
    remaining0 <- induced_subgraph(skeleton0, vids = format(unused, scientific = FALSE, trim = TRUE))
    used <- as.numeric(unique(c(unlist(pathList), unlist(loopList))))
    unused <- as.numeric(vertexNames)[which(!(as.numeric(vertexNames) %in% used))]
    if (length(unused) > 0) {
      ends <- lapply(igraph::neighborhood(fullSkeleton0, order = 3, nodes = format(unused, scientific = FALSE, trim = TRUE)), function(x) nodeList[which(format(nodeList, scientific = FALSE, trim = TRUE) %in% names(x))])
      
      roots <- format(unique(unlist(ends)), scientific = FALSE, trim = TRUE)
      if (length(roots) > 0) {
        ends <- igraph::neighborhood(fullSkeleton0, order = 3, nodes = roots)
        ends <- unlist(lapply(ends, function(x) names(x)[which(names(x) %in% format(unused, scientific = FALSE, trim = TRUE))][1]))
        
        for (i in 1:length(roots))
        {
          loopPart1 <- names(na.omit(dfs(remaining0, ends[i], unreachable = FALSE)$order))
          loopPart2a <- igraph::shortest_paths(fullSkeleton0, from = loopPart1[1], to = roots[i])$vpath[[1]][-1]
          loopPart2b <- igraph::shortest_paths(fullSkeleton0, from = loopPart1[length(loopPart1)], to = roots[i])$vpath[[1]][-1]
          loopList <- append(loopList, list(as.numeric(c(rev(names(loopPart2a)), loopPart1, names(loopPart2b)))))
        }
      }
    }
    
    ## All that remains now is perfect loops. Start and end at same point with no intersections or end points.
    remaining0 <- igraph::induced_subgraph(remaining0, vids = igraph::V(remaining0)[!(names(igraph::V(remaining0)) %in% unlist(loopList))])
    while (TRUE) {
      if (length(igraph::V(remaining0)) > 0) {
        perfectLoop <- names(na.omit(dfs(remaining0, igraph::V(remaining0)[1], unreachable = FALSE)$order))
        remaining0 <- igraph::delete_vertices(remaining0, v = perfectLoop)
        loopList <- append(loopList, list(as.numeric(c(perfectLoop, perfectLoop[1]))))
      } else {
        break
      }
    }
    
    return(loopList)
  }
  
  getAllLoops <- function(comps, dims) {
    n <- length(comps)
    for (i in 1:n){
      comps[[i]]$paths$loopList <- getLoopsForComponent(nodeList = comps[[i]]$nodes$nodeList,
                                                        skeleton = comps[[i]]$skeletons$skeleton,
                                                        skeleton0 = comps[[i]]$skeletons$skeleton0,
                                                        pathList = comps[[i]]$paths$pathList,
                                                        dims = dims)
      comps[[i]]$paths$allPaths <- append(comps[[i]]$paths$pathList, comps[[i]]$paths$loopList)
    }
    return(comps)
  }
  
  updateSkeleton0 <- function(graphdf0, skeleton0, nodeList){
    graphdf0 <- igraph::as_data_frame(skeleton0)
    graphdf0$node_only_dist <- ifelse(graphdf0$from %in% nodeList | graphdf0$to %in% nodeList, 1, 0)
    skeleton0 <- igraph::graph_from_data_frame(graphdf0, directed = FALSE)
    return(list('graphdf0'=graphdf0, 'skeleton0'=skeleton0))
  }
  
  updateAllSkeleton0s <- function(comps) {
    # set node_only_dist values: 1 = edge is connected to a node; 0 = edge is not connected to a node
    n <- length(comps)
    for (i in 1:n){
      updated <- updateSkeleton0(graphdf0 = comps[[i]]$skeletons$skeleton_df0, 
                                 skeleton0 = comps[[i]]$skeletons$skeleton0, 
                                 nodeList = comps[[i]]$nodes$nodeList)
      comps[[i]]$skeletons$skeleton_df0 <- updated$graphdf0
      comps[[i]]$skeletons$skeleton0 <- updated$skeleton0
    }
    return(comps)
  }
  
  comps <- getAllNonLoopPaths(comps = comps)
  comps <- getAllLoops(comps = comps, dims = dims)
  comps <- updateAllSkeleton0s(comps = comps)
  
  return(comps)
}

splitPathsIntoGraphs <- function(comps, dims) {
  addTroughNodes <- function(breakPoints, troughNodes, dims) {
    # find the indice(s) of the trough node(s) that is not in the same row as its
    # neighbor to the right
    breaks_by_row <- which(i_to_r(troughNodes[-length(troughNodes)], dims[1]) != i_to_r(troughNodes[-1], dims[1])) 
    # find the indice(s) of the trough node(s) that when you subtract 1 from its row it is not in
    # the same row as its neighbor to the right
    breaks_by_col1 <- which(i_to_c(troughNodes[-length(troughNodes)], dims[1]) - 1 != i_to_c(troughNodes[-1], dims[1]))
    # find the indice(s) of the trough node(s) that is not in the same row as its neighbor to the right minus 1
    breaks_by_col2 <- which(i_to_c(troughNodes[-length(troughNodes)], dims[1]) != i_to_c(troughNodes[-1], dims[1]) - 1)
    
    # combine the breaks - Why were these rules chosen???
    breaks <- intersect(union(breaks_by_row, breaks_by_col1), breaks_by_col2)
    # add indices of the first and last vertex in the troughNodes list
    breaks <- c(1, breaks, length(troughNodes))
    
    # take the average between each break index and the next, rounding up to the
    # nearest integer and add the troughNodes at these indices to the candidates
    # list - Why do we take the average???
    average_breaks <- ceiling((breaks[-1] + breaks[-length(breaks)]) / 2)
    breakPoints <- c(breakPoints, troughNodes[average_breaks])
    
    return(breakPoints)
  }
  
  assignGraphIDs <- function(comps) {
    # assign sequential IDs to paths across all components
    graph_counter <- 0 
    n <- length(comps)
    for (i in 1:n){
      if (length(igraph::V(comps[[i]]$skeletons$skeleton0)$graphID) > 0){
        igraph::V(comps[[i]]$skeletons$skeleton0)$graphID <- igraph::V(comps[[i]]$skeletons$skeleton0)$graphID + graph_counter
        # vertices that are break points have graphID = NA
        graph_counter <- max(igraph::V(comps[[i]]$skeletons$skeleton0)$graphID, na.rm = TRUE)
      }
    }
    return(comps)
  }
  
  checkBreakPoints <- function(breakPoints, allPaths, nodeGraph, terminalNodes, dims) {
    # Internal function called by processHandwriting that eliminates breakpoints
    # based on rules to try to coherently separate letters.
    
    # Check rules for candidate breakpoints
    breakFlag <- rep(TRUE, length(breakPoints))
    
    for (i in 1:length(allPaths)) { 
      # create character vector of each vertex in path i
      tempPath <- format(allPaths[[i]], scientific = FALSE, trim = TRUE)
      # check if path i contains candidate nodes
      nodeChecks <- which(breakPoints %in% tempPath)
      # delete edge between the first and last node in the path if it exists
      tempNodeGraph <- igraph::delete_edges(nodeGraph, paste0(tempPath[1], "|", tempPath[length(tempPath)]))
      
      if (igraph::distances(tempNodeGraph, v = tempPath[1], to = tempPath[length(tempPath)]) < Inf) {
        # No breaking on multiple paths between nodes. 
        breakFlag[nodeChecks] <- FALSE
      } else if (any(tempPath %in% terminalNodes)) {
        # No break if path has an endpoint
        breakFlag[nodeChecks] <- FALSE
      } else if (any(which(tempPath %in% c(breakPoints[nodeChecks])) <= 4 | which(tempPath %in% c(breakPoints[nodeChecks])) >= length(tempPath) - 3) | length(tempPath) <= 10) {
        # No breaks too close to a vertex
        breakFlag[nodeChecks[which(breakPoints[nodeChecks] <= 5 | breakPoints[nodeChecks] >= length(tempPath) - 4)]] <- FALSE
      }
    }
    
    return(breakFlag)
  }
  
  checkSimplicityBreaks <- function(breakPoints, pathList, loopList, letters, skeleton0, nodeList, terminalNodes, hasTrough, dims) {
    # Internal function for removing breakpoints that separate paths that are too
    # simple to be split. Remove break if graph on left and right of the break
    # have 4 or fewer nodes and no loops or double paths. Never remove break on a
    # trough.
    tooSimpleFlag <- rep(FALSE, length(breakPoints))
    for (i in 1:length(pathList))
    {
      tempPath <- pathList[[i]]
      nodestoCheck <- which(breakPoints %in% tempPath)
      if (length(nodestoCheck) >= 1) {
        if (!hasTrough[i]) {
          pathIndex <- which(tempPath == breakPoints[nodestoCheck])
          
          borderLetters <- c(
            igraph::V(skeleton0)$graphID[which(igraph::V(skeleton0)$name == tempPath[pathIndex - 1])],
            igraph::V(skeleton0)$graphID[which(igraph::V(skeleton0)$name == tempPath[pathIndex + 1])]
          )
          left <- letters[[borderLetters[1]]]
          right <- letters[[borderLetters[2]]]
          
          nodesOnLeft <- sum(nodeList %in% left)
          nodesOnRight <- sum(nodeList %in% right)
          terminalLeft <- sum(terminalNodes %in% left)
          terminalRight <- sum(terminalNodes %in% right)
          
          if (nodesOnLeft == 3 & nodesOnRight == 3 & terminalLeft == 2 & terminalRight == 2) {
            pathsOnLeft <- length(pathLetterAssociate(c(pathList, loopList), left))
            pathsOnRight <- length(pathLetterAssociate(c(pathList, loopList), right))
            if (pathsOnLeft == 2 & pathsOnRight == 2) {
              tooSimpleFlag[nodestoCheck] <- TRUE
            }
          }
        }
      }
    }
    return(tooSimpleFlag)
  }
  
  checkStacking <- function(breakPoints, allPaths, letters, skeleton0, dims) {
    # Internal function for removing breakpoints that follow all of the rules, but
    # separate two letters that are stacked on top of each other.
    stackPtFlag <- rep(FALSE, length(breakPoints))
    
    for (i in 1:length(allPaths))
    {
      tempPath <- allPaths[[i]]
      tempRow <- ((tempPath - 1) %% dims[1]) + 1
      tempCol <- ((tempPath - 1) %/% dims[1]) + 1
      nodeChecks <- which(breakPoints %in% tempPath)
      if (length(nodeChecks) == 1) {
        if (abs((max(tempRow) - min(tempRow)) / (max(tempCol) + 1 - min(tempCol))) > 2) {
          stackPtFlag[nodeChecks] <- TRUE
        } else {
          pathIndex <- which(tempPath == breakPoints[nodeChecks])
          
          borderLetters <- c(
            igraph::V(skeleton0)$graphID[which(igraph::V(skeleton0)$name == tempPath[pathIndex - 1])],
            igraph::V(skeleton0)$graphID[which(igraph::V(skeleton0)$name == tempPath[pathIndex + 1])]
          )
          gr1 <- letters[[borderLetters[1]]]
          gr1Rows <- ((gr1 - 1) %% dims[1]) + 1
          gr2 <- letters[[borderLetters[2]]]
          gr2Rows <- ((gr2 - 1) %% dims[1]) + 1
          
          # Call a break a stack point if the overlap between the bordering letters is
          # less than 10% of the total range of the combined letters.
          if (is.null(gr1) || is.null(gr2)) {
            next
          }
          overlap <- min(abs(max(gr1Rows) - min(gr2Rows)), abs(max(gr2Rows) - min(gr1Rows)))
          totalRange <- (diff(range(c(gr1Rows, gr2Rows))))
          overlapPercentage <- overlap / totalRange
          if (is.na(overlapPercentage)) {
            overlapPercentage <- 1
          }
          if (overlapPercentage < .1) {
            stackPtFlag[nodeChecks] <- TRUE
          }
        }
      }
    }
    return(stackPtFlag)
  }
  
  findTroughNodes <- function(tempPath, dims) {
    troughNodes <- c()
    # if path has more than 10 vertices
    if (length(tempPath) > 10) {
      # get the row number of each vertex in the path
      rows <- ((tempPath - 1) %% dims[1]) + 1
      # skip the first 4 and last 4 vertices. Assign vertex j as a troughNode if
      # the following conditions are met: 
      #    (1) there is at least one vertex before j in tempPath that is at least 
      #        2 rows higher than j 
      #    (2) there is at least one vertex after j that is at least 2 rows higher than j 
      #    (3) there are no vertices in the path between j and the closest vertices
      #        that satisfy conditions 1 and 2 that are lower than j. 
      # If j is a troughNode, set hasTrough to true for path i = tempPath
      for (j in 5:(length(rows) - 4)) { 
        if (isTroughNode(rows = rows, j = j)){
          troughNodes <- c(troughNodes, tempPath[j])
        }
      }
    }
    return(troughNodes)
  }
  
  getBreakPointsForComponent <- function(pathList, dims) {
    hasTrough <- rep(FALSE, length(pathList))
    troughNodes <- c()
    breakPoints <- c()
    for (i in 1:length(pathList)) {
      tempPath <- pathList[[i]]
      newTroughNodes <- findTroughNodes(tempPath = tempPath, dims = dims)
      if (length(newTroughNodes) > 0){
        troughNodes <- c(troughNodes, newTroughNodes)
        hasTrough[i] <- TRUE
      } else {
        # for paths without a trough node, add the middle vertex to the candidate
        # list
        breakPoints <- c(breakPoints, tempPath[ceiling(length(tempPath) / 2)])
      }
    }
    return(list('breakPoints' = breakPoints, 'hasTrough' = hasTrough, 'troughNodes' = troughNodes))
  }
  
  getAllBreakPoints <- function(comps, dims) {
    n <- length(comps)
    for (i in 1:n){
      if (length(comps[[i]]$paths$pathList) >= 1) {
        candidates <- getBreakPointsForComponent(pathList = comps[[i]]$paths$pathList, dims = dims)
        comps[[i]]$paths$hasTrough <- candidates$hasTrough
        comps[[i]]$nodes$breakPoints <- addTroughNodes(breakPoints = candidates$breakPoints,
                                                       troughNodes = candidates$troughNodes,
                                                       dims = dims)
      } else {
        comps[[i]]$nodes$breakPoints <- list()
        comps[[i]]$paths$hasTrough <- list()
      }
    }
    return(comps)
  }
  
  getNodeGraph <- function(allPaths, nodeList) {
    # Internal function for creating a graph from a path list and node list. More specifically,
    # create a graph with a vertex for each node in nodeList. Then add an edge between the first and last
    # node (pixel / vertex) in each path in allPaths.
    nodeGraph <- igraph::make_empty_graph(directed = FALSE)
    nodeGraph <- igraph::add_vertices(nodeGraph, length(nodeList), name = format(nodeList, scientific = FALSE, trim = TRUE))
    for (i in 1:length(allPaths))
    {
      nodeGraph <- igraph::add_edges(nodeGraph, format(c(allPaths[[i]][1], allPaths[[i]][length(allPaths[[i]])]), scientific = FALSE, trim = TRUE))
    }
    return(nodeGraph)
  }
  
  isolateAllGraphs <- function(comps, dims) {
    # assign sequential IDs to paths within each component
    n <- length(comps)
    for (i in 1:n){
      temp_graph <- comps[[i]]$skeletons$skeleton0
      if (length(igraph::V(temp_graph)$name) > 0) {
        isolated <- isolateGraphsForComponent(allPaths = comps[[i]]$paths$allPaths,
                                              skeleton0 = comps[[i]]$skeletons$skeleton0,
                                              breakPoints = comps[[i]]$nodes$breakPoints,
                                              pathList = comps[[i]]$paths$pathList,
                                              loopList = comps[[i]]$paths$loopList,
                                              nodeList = comps[[i]]$nodes$nodeList,
                                              terminalNodes = comps[[i]]$nodes$terminalNodes,
                                              hasTrough = comps[[i]]$paths$hasTrough,
                                              dims = dims)
        comps[[i]]$paths$allGraphs <- isolated$allGraphs
        comps[[i]]$skeletons$skeleton0 <- isolated$skeleton0
        comps[[i]]$nodes$breakPoints <- isolated$breakPoints
        comps[[i]]$nodes$nodeList <- isolated$nodeList
        comps[[i]]$paths$allPaths <- NULL
      } else {
        # empty
        comps[[i]]$nodes$breakPoints <- numeric(0)
        # rename allPaths as allGraphs
        comps[[i]]$paths$allGraphs <- comps[[i]]$paths$allPaths
        comps[[i]]$paths$allPaths <- NULL
        # NOTE: comps[[i]]$skeletons$skeleton0, comps[[i]]$nodes$nodeList
        # don't change from before this loop
      }
    }
    return(comps)
  }
  
  isolateGraphsForComponent <- function(allPaths, skeleton0, breakPoints, dims, pathList, loopList, nodeList, terminalNodes, hasTrough) {
    # Break on breakPoints and group points by which graph they fall into
    # NOTE: skip if skeleton0 and (or?) allPaths are empty. Even if breakPoints
    # is empty, still need to run makeGraphs() to assign graph number.
    graphList <- makeGraphs(allPaths, skeleton0, breakPoints)
    
    # get number of vertices in each graph
    graph_lengths <- unlist(lapply(graphList[[1]], length))
    
    # list graphs with more than 5 vertices and those with 5 or fewer
    graphs <- graphList[[1]][graph_lengths > 5]
    
    # get graph IDs for all graphs, even ones with 5 vertices or less
    igraph::V(skeleton0)$graphID <- graphList[[2]]
    # delete vertices from graphs with 5 or fewer vertices. does not delete vertices with graphID = NA
    skeleton0 <- igraph::delete_vertices(skeleton0, igraph::V(skeleton0)[which(igraph::V(skeleton0)$graphID %in% which(graph_lengths <= 5))])
    igraph::V(skeleton0)$graphID[!is.na(igraph::V(skeleton0)$graphID)] <- as.numeric(as.factor(na.omit(igraph::V(skeleton0)$graphID)))
    
    # Remove breakpoints that shouldn't have broken
    allGraphs <- allPaths
    if (length(breakPoints) > 0) {
      final_breaks <- breakPoints[!(checkStacking(breakPoints, allGraphs, graphs, skeleton0, dims))]
      final_breaks <- final_breaks[!(checkSimplicityBreaks(final_breaks, pathList, loopList, graphs, skeleton0, nodeList, terminalNodes, hasTrough, dims))]
      
      pathsWithBreaks <- lapply(allGraphs, function(x) {
        which(x %in% breakPoints)
      })
      breaksPerPath <- unlist(lapply(pathsWithBreaks, length))
      for (i in which(breaksPerPath > 0)) {
        newNodes <- pathsWithBreaks[[i]]
        if (allGraphs[[i]][newNodes] %in% final_breaks) {
          tryCatch(
            expr = {
              igraph::E(skeleton0, P = format(allGraphs[[i]][c(newNodes - 2, newNodes - 1)], scientific = FALSE, trim = TRUE))$node_only_dist <- 1
              igraph::E(skeleton0, P = format(allGraphs[[i]][c(newNodes + 1, newNodes + 2)], scientific = FALSE, trim = TRUE))$node_only_dist <- 1
            }, error = function(e) {
              
            }
          )
          
          newNodes <- c(newNodes - 1, newNodes + 1)
          nodeList <- c(nodeList, allGraphs[[i]][newNodes])
          allGraphs[[i]] <- list(allGraphs[[i]][1:(newNodes[1])], allGraphs[[i]][(newNodes[2]):length(allGraphs[[i]])])
        }
        
        # graphIDs = range(V(skeleton0)$graphID[names(V(skeleton0)) %in% format(allGraphs[[i]], scientific = FALSE, trim = TRUE)], na.rm = TRUE)
        # Had to break this above line into this to stop a range(numeric(0)) error from happening
        skelInPaths <- igraph::V(skeleton0)$graphID[names(igraph::V(skeleton0)) %in% format(allGraphs[[i]], scientific = FALSE, trim = TRUE)]
        if (identical(skelInPaths, numeric(0))) {
          graphIDs <- c(Inf, -Inf)
        } else {
          graphIDs <- range(skelInPaths, na.rm = TRUE)
        }
        
        igraph::V(skeleton0)$graphID[which(igraph::V(skeleton0)$graphID == graphIDs[2])] <- graphIDs[1]
        igraph::V(skeleton0)$graphID[which(names(igraph::V(skeleton0)) %in% format(allGraphs[[i]][newNodes], scientific = FALSE, trim = TRUE))] <- graphIDs[1]
      }
      allGraphs <- lapply(rapply(allGraphs, enquote, how = "unlist"), eval)
    } else {
      # empty
      final_breaks <- numeric(0)
    }
    
    return(list('allGraphs' = allGraphs, 'skeleton0' = skeleton0, 'final_breaks' = final_breaks, 'nodeList' = nodeList))
  }
  
  isTroughNode <- function(rows, j) {
    if (any(rows[1:(j - 1)] < rows[j] - 1) & any(rows[(j + 1):length(rows)] < rows[j] - 1)) {
      # Find all vertices to the left of j in tempPath that are at least 2
      # rows higher than j (in the image). Of these vertices, find the
      # vertex that is closest to j from the left in tempPath
      lowerEnd <- max(which(rows[1:(j - 1)] < rows[j] - 1))
      # Find all vertices to the right of j in tempPath that are at least 2
      # rows higher than j (in the image). Of these vertices, find the
      # vertex that is closest to j from the right in tempPath
      upperEnd <- min(which(rows[(j + 1):length(rows)] < rows[j] - 1))
      # If none of the vertices between the lowerEnd and upperEnd are one or
      # more rows lower than j, then assign j as a trough node
      if (!any(rows[lowerEnd:(j + upperEnd)] > rows[j])) {
        isTrough <- TRUE
      } else {
        isTrough <- FALSE
      }
    } else {
      isTrough <- FALSE
    }
    return(isTrough)
  }
  
  makeGraphs <- function(allPaths, skeleton0, breakPoints) {
    oldVerts <- igraph::V(skeleton0)$name
    # delete breakPoints from the skeleton
    formatted_breakPoints <- format(breakPoints, scientific = FALSE, trim = TRUE)
    if (any(as.character(formatted_breakPoints) %in% names(igraph::V(skeleton0)))) {
      skeleton0 <- igraph::delete_vertices(skeleton0, v = as.character(formatted_breakPoints))
    }
    
    # find connected components. Each connected component is a graph, and
    # igraph::components assigns each component a group number. This number
    # is the graphID.
    comps <- igraph::components(skeleton0)
    graphIDs <- rep(NA, length(oldVerts))
    # assign the ID to the original vertices that are not break points
    graphIDs[which(!(oldVerts %in% formatted_breakPoints))] <- comps$membership
    # lists of vertex names grouped by graph
    graphs <- lapply(1:max(comps$membership), function(x) as.numeric(igraph::V(skeleton0)$name[comps$membership == x]))
    
    return(list(graphs, graphIDs))
  }
  
  updateBreakPointsForComponent <- function(pathList, nodeList, breakPoints, terminalNodes, dims, allPaths) {
    # create a graph with vertices for each node and edges between the first and last pixel / vertex in each path
    nodeGraph <- getNodeGraph(pathList, nodeList)
    # flag candidates as good if the following are true: 
    #     (1) there NOT is more than one route between the first and last 
    #         nodes in the path containing the candidate 
    #     (2) the path does NOT contain a terminal node 
    #     (3) the candidate break point is NOT within 4 of the first or 
    #         last node in the path 
    #     (4) the path contains more than 10 vertices.
    goodBreaks <- checkBreakPoints(breakPoints = breakPoints, allPaths = pathList, nodeGraph = nodeGraph, terminalNodes = terminalNodes, dims)
    breakPoints <- breakPoints[goodBreaks]
    # list the break(s) in each path or 'integer(0)' if the path does not contain a break
    pathsWithBreaks <- lapply(allPaths, function(x) {
      which(x %in% breakPoints)
    })
    # number of breaks per path
    breaksPerPath <- unlist(lapply(pathsWithBreaks, length))
    # update the list of breaks: if a path has more than one break, take the average of the breaks (rounding up) and
    # remove the original breaks so that each path has either 0 or 1 break.
    for (i in which(breaksPerPath > 1))
    { # if current path has more than one break, average the breaks and round up
      newBreak <- floor(mean(which(allPaths[[i]] %in% breakPoints)))
      # drop pre stack breaks in current path from the pre stack breaks list
      breakPoints <- breakPoints[which(!(breakPoints %in% allPaths[[i]]))]
      # add "new break" for current path to list
      breakPoints <- c(breakPoints, allPaths[[i]][newBreak])
    }
    return(breakPoints)
  }
  
  updateAllBreakPoints <- function(comps, dims) {
    # discard bad break points
    n <- length(comps)
    for (i in 1:n){
      tempPaths <- comps[[i]]$paths$pathList
      if (length(tempPaths) > 0){
        comps[[i]]$nodes$breakPoints <- updateBreakPointsForComponent(pathList = tempPaths, 
                                                                      nodeList = comps[[i]]$nodes$nodeList, 
                                                                      breakPoints = comps[[i]]$nodes$breakPoints, 
                                                                      terminalNodes = comps[[i]]$nodes$terminalNodes, 
                                                                      allPaths = comps[[i]]$paths$allPaths,
                                                                      dims = dims)
      } else {
        comps[[i]]$nodes$breakPoints <- list()
      }
    }
    return(comps)
  }
  
  comps <- getAllBreakPoints(comps = comps, dims = dims)
  comps <- updateAllBreakPoints(comps = comps, dims = dims)
  comps <- isolateAllGraphs(comps = comps, dims = dims)
  comps <- assignGraphIDs(comps = comps)
  
  return(comps)
}

splitPathsIntoGraphs <- function(comps, dims) {
  addTroughNodes <- function(breakPoints, troughNodes, dims) {
    # find the indice(s) of the trough node(s) that is not in the same row as its
    # neighbor to the right
    breaks_by_row <- which(i_to_r(troughNodes[-length(troughNodes)], dims[1]) != i_to_r(troughNodes[-1], dims[1])) 
    # find the indice(s) of the trough node(s) that when you subtract 1 from its row it is not in
    # the same row as its neighbor to the right
    breaks_by_col1 <- which(i_to_c(troughNodes[-length(troughNodes)], dims[1]) - 1 != i_to_c(troughNodes[-1], dims[1]))
    # find the indice(s) of the trough node(s) that is not in the same row as its neighbor to the right minus 1
    breaks_by_col2 <- which(i_to_c(troughNodes[-length(troughNodes)], dims[1]) != i_to_c(troughNodes[-1], dims[1]) - 1)
    
    # combine the breaks - Why were these rules chosen???
    breaks <- intersect(union(breaks_by_row, breaks_by_col1), breaks_by_col2)
    # add indices of the first and last vertex in the troughNodes list
    breaks <- c(1, breaks, length(troughNodes))
    
    # take the average between each break index and the next, rounding up to the
    # nearest integer and add the troughNodes at these indices to the candidates
    # list - Why do we take the average???
    average_breaks <- ceiling((breaks[-1] + breaks[-length(breaks)]) / 2)
    breakPoints <- c(breakPoints, troughNodes[average_breaks])
    
    return(breakPoints)
  }
  
  assignGraphIDs <- function(comps) {
    # assign sequential IDs to paths across all components
    graph_counter <- 0 
    n <- length(comps)
    for (i in 1:n){
      if (length(igraph::V(comps[[i]]$skeletons$skeleton0)$graphID) > 0){
        igraph::V(comps[[i]]$skeletons$skeleton0)$graphID <- igraph::V(comps[[i]]$skeletons$skeleton0)$graphID + graph_counter
        # vertices that are break points have graphID = NA
        graph_counter <- max(igraph::V(comps[[i]]$skeletons$skeleton0)$graphID, na.rm = TRUE)
      }
    }
    return(comps)
  }
  
  checkBreakPoints <- function(breakPoints, allPaths, nodeGraph, terminalNodes, dims) {
    # Internal function called by processHandwriting that eliminates breakpoints
    # based on rules to try to coherently separate letters.
    
    # Check rules for candidate breakpoints
    breakFlag <- rep(TRUE, length(breakPoints))
    
    for (i in 1:length(allPaths)) { 
      # create character vector of each vertex in path i
      tempPath <- format(allPaths[[i]], scientific = FALSE, trim = TRUE)
      # check if path i contains candidate nodes
      nodeChecks <- which(breakPoints %in% tempPath)
      # delete edge between the first and last node in the path if it exists
      tempNodeGraph <- igraph::delete_edges(nodeGraph, paste0(tempPath[1], "|", tempPath[length(tempPath)]))
      
      if (igraph::distances(tempNodeGraph, v = tempPath[1], to = tempPath[length(tempPath)]) < Inf) {
        # No breaking on multiple paths between nodes. 
        breakFlag[nodeChecks] <- FALSE
      } else if (any(tempPath %in% terminalNodes)) {
        # No break if path has an endpoint
        breakFlag[nodeChecks] <- FALSE
      } else if (any(which(tempPath %in% c(breakPoints[nodeChecks])) <= 4 | which(tempPath %in% c(breakPoints[nodeChecks])) >= length(tempPath) - 3) | length(tempPath) <= 10) {
        # No breaks too close to a vertex
        breakFlag[nodeChecks[which(breakPoints[nodeChecks] <= 5 | breakPoints[nodeChecks] >= length(tempPath) - 4)]] <- FALSE
      }
    }
    
    return(breakFlag)
  }
  
  checkSimplicityBreaks <- function(breakPoints, pathList, loopList, letters, skeleton0, nodeList, terminalNodes, hasTrough, dims) {
    # Internal function for removing breakpoints that separate paths that are too
    # simple to be split. Remove break if graph on left and right of the break
    # have 4 or fewer nodes and no loops or double paths. Never remove break on a
    # trough.
    tooSimpleFlag <- rep(FALSE, length(breakPoints))
    for (i in 1:length(pathList))
    {
      tempPath <- pathList[[i]]
      nodestoCheck <- which(breakPoints %in% tempPath)
      if (length(nodestoCheck) >= 1) {
        if (!hasTrough[i]) {
          pathIndex <- which(tempPath == breakPoints[nodestoCheck])
          
          borderLetters <- c(
            igraph::V(skeleton0)$graphID[which(igraph::V(skeleton0)$name == tempPath[pathIndex - 1])],
            igraph::V(skeleton0)$graphID[which(igraph::V(skeleton0)$name == tempPath[pathIndex + 1])]
          )
          left <- letters[[borderLetters[1]]]
          right <- letters[[borderLetters[2]]]
          
          nodesOnLeft <- sum(nodeList %in% left)
          nodesOnRight <- sum(nodeList %in% right)
          terminalLeft <- sum(terminalNodes %in% left)
          terminalRight <- sum(terminalNodes %in% right)
          
          if (nodesOnLeft == 3 & nodesOnRight == 3 & terminalLeft == 2 & terminalRight == 2) {
            pathsOnLeft <- length(pathLetterAssociate(c(pathList, loopList), left))
            pathsOnRight <- length(pathLetterAssociate(c(pathList, loopList), right))
            if (pathsOnLeft == 2 & pathsOnRight == 2) {
              tooSimpleFlag[nodestoCheck] <- TRUE
            }
          }
        }
      }
    }
    return(tooSimpleFlag)
  }
  
  checkStacking <- function(breakPoints, allPaths, letters, skeleton0, dims) {
    # Internal function for removing breakpoints that follow all of the rules, but
    # separate two letters that are stacked on top of each other.
    stackPtFlag <- rep(FALSE, length(breakPoints))
    
    for (i in 1:length(allPaths))
    {
      tempPath <- allPaths[[i]]
      tempRow <- ((tempPath - 1) %% dims[1]) + 1
      tempCol <- ((tempPath - 1) %/% dims[1]) + 1
      nodeChecks <- which(breakPoints %in% tempPath)
      if (length(nodeChecks) == 1) {
        if (abs((max(tempRow) - min(tempRow)) / (max(tempCol) + 1 - min(tempCol))) > 2) {
          stackPtFlag[nodeChecks] <- TRUE
        } else {
          pathIndex <- which(tempPath == breakPoints[nodeChecks])
          
          borderLetters <- c(
            igraph::V(skeleton0)$graphID[which(igraph::V(skeleton0)$name == tempPath[pathIndex - 1])],
            igraph::V(skeleton0)$graphID[which(igraph::V(skeleton0)$name == tempPath[pathIndex + 1])]
          )
          gr1 <- letters[[borderLetters[1]]]
          gr1Rows <- ((gr1 - 1) %% dims[1]) + 1
          gr2 <- letters[[borderLetters[2]]]
          gr2Rows <- ((gr2 - 1) %% dims[1]) + 1
          
          # Call a break a stack point if the overlap between the bordering letters is
          # less than 10% of the total range of the combined letters.
          if (is.null(gr1) || is.null(gr2)) {
            next
          }
          overlap <- min(abs(max(gr1Rows) - min(gr2Rows)), abs(max(gr2Rows) - min(gr1Rows)))
          totalRange <- (diff(range(c(gr1Rows, gr2Rows))))
          overlapPercentage <- overlap / totalRange
          if (is.na(overlapPercentage)) {
            overlapPercentage <- 1
          }
          if (overlapPercentage < .1) {
            stackPtFlag[nodeChecks] <- TRUE
          }
        }
      }
    }
    return(stackPtFlag)
  }
  
  findTroughNodes <- function(tempPath, dims) {
    troughNodes <- c()
    # if path has more than 10 vertices
    if (length(tempPath) > 10) {
      # get the row number of each vertex in the path
      rows <- ((tempPath - 1) %% dims[1]) + 1
      # skip the first 4 and last 4 vertices. Assign vertex j as a troughNode if
      # the following conditions are met: 
      #    (1) there is at least one vertex before j in tempPath that is at least 
      #        2 rows higher than j 
      #    (2) there is at least one vertex after j that is at least 2 rows higher than j 
      #    (3) there are no vertices in the path between j and the closest vertices
      #        that satisfy conditions 1 and 2 that are lower than j. 
      # If j is a troughNode, set hasTrough to true for path i = tempPath
      for (j in 5:(length(rows) - 4)) { 
        if (isTroughNode(rows = rows, j = j)){
          troughNodes <- c(troughNodes, tempPath[j])
        }
      }
    }
    return(troughNodes)
  }
  
  getBreakPointsForComponent <- function(pathList, dims) {
    hasTrough <- rep(FALSE, length(pathList))
    troughNodes <- c()
    breakPoints <- c()
    for (i in 1:length(pathList)) {
      tempPath <- pathList[[i]]
      newTroughNodes <- findTroughNodes(tempPath = tempPath, dims = dims)
      if (length(newTroughNodes) > 0){
        troughNodes <- c(troughNodes, newTroughNodes)
        hasTrough[i] <- TRUE
      } else {
        # for paths without a trough node, add the middle vertex to the candidate
        # list
        breakPoints <- c(breakPoints, tempPath[ceiling(length(tempPath) / 2)])
      }
    }
    return(list('breakPoints' = breakPoints, 'hasTrough' = hasTrough, 'troughNodes' = troughNodes))
  }
  
  getAllBreakPoints <- function(comps, dims) {
    n <- length(comps)
    for (i in 1:n){
      if (length(comps[[i]]$paths$pathList) >= 1) {
        candidates <- getBreakPointsForComponent(pathList = comps[[i]]$paths$pathList, dims = dims)
        comps[[i]]$paths$hasTrough <- candidates$hasTrough
        comps[[i]]$nodes$breakPoints <- addTroughNodes(breakPoints = candidates$breakPoints,
                                                       troughNodes = candidates$troughNodes,
                                                       dims = dims)
      } else {
        comps[[i]]$nodes$breakPoints <- list()
        comps[[i]]$paths$hasTrough <- list()
      }
    }
    return(comps)
  }
  
  getNodeGraph <- function(allPaths, nodeList) {
    # Internal function for creating a graph from a path list and node list. More specifically,
    # create a graph with a vertex for each node in nodeList. Then add an edge between the first and last
    # node (pixel / vertex) in each path in allPaths.
    nodeGraph <- igraph::make_empty_graph(directed = FALSE)
    nodeGraph <- igraph::add_vertices(nodeGraph, length(nodeList), name = format(nodeList, scientific = FALSE, trim = TRUE))
    for (i in 1:length(allPaths))
    {
      nodeGraph <- igraph::add_edges(nodeGraph, format(c(allPaths[[i]][1], allPaths[[i]][length(allPaths[[i]])]), scientific = FALSE, trim = TRUE))
    }
    return(nodeGraph)
  }
  
  isolateAllGraphs <- function(comps, dims) {
    # assign sequential IDs to paths within each component
    n <- length(comps)
    for (i in 1:n){
      temp_graph <- comps[[i]]$skeletons$skeleton0
      if (length(igraph::V(temp_graph)$name) > 0) {
        isolated <- isolateGraphsForComponent(allPaths = comps[[i]]$paths$allPaths,
                                              skeleton0 = comps[[i]]$skeletons$skeleton0,
                                              breakPoints = comps[[i]]$nodes$breakPoints,
                                              pathList = comps[[i]]$paths$pathList,
                                              loopList = comps[[i]]$paths$loopList,
                                              nodeList = comps[[i]]$nodes$nodeList,
                                              terminalNodes = comps[[i]]$nodes$terminalNodes,
                                              hasTrough = comps[[i]]$paths$hasTrough,
                                              dims = dims)
        comps[[i]]$paths$allGraphs <- isolated$allGraphs
        comps[[i]]$skeletons$skeleton0 <- isolated$skeleton0
        comps[[i]]$nodes$breakPoints <- isolated$breakPoints
        comps[[i]]$nodes$nodeList <- isolated$nodeList
        comps[[i]]$paths$allPaths <- NULL
      } else {
        # empty
        comps[[i]]$nodes$breakPoints <- numeric(0)
        # rename allPaths as allGraphs
        comps[[i]]$paths$allGraphs <- comps[[i]]$paths$allPaths
        comps[[i]]$paths$allPaths <- NULL
        # NOTE: comps[[i]]$skeletons$skeleton0, comps[[i]]$nodes$nodeList
        # don't change from before this loop
      }
    }
    return(comps)
  }
  
  isolateGraphsForComponent <- function(allPaths, skeleton0, breakPoints, dims, pathList, loopList, nodeList, terminalNodes, hasTrough) {
    # Break on breakPoints and group points by which graph they fall into
    # NOTE: skip if skeleton0 and (or?) allPaths are empty. Even if breakPoints
    # is empty, still need to run makeGraphs() to assign graph number.
    graphList <- makeGraphs(allPaths, skeleton0, breakPoints)
    
    # get number of vertices in each graph
    graph_lengths <- unlist(lapply(graphList[[1]], length))
    
    # list graphs with more than 5 vertices and those with 5 or fewer
    graphs <- graphList[[1]][graph_lengths > 5]
    
    # get graph IDs for all graphs, even ones with 5 vertices or less
    igraph::V(skeleton0)$graphID <- graphList[[2]]
    # delete vertices from graphs with 5 or fewer vertices. does not delete vertices with graphID = NA
    skeleton0 <- igraph::delete_vertices(skeleton0, igraph::V(skeleton0)[which(igraph::V(skeleton0)$graphID %in% which(graph_lengths <= 5))])
    igraph::V(skeleton0)$graphID[!is.na(igraph::V(skeleton0)$graphID)] <- as.numeric(as.factor(na.omit(igraph::V(skeleton0)$graphID)))
    
    # Remove breakpoints that shouldn't have broken
    allGraphs <- allPaths
    if (length(breakPoints) > 0) {
      final_breaks <- breakPoints[!(checkStacking(breakPoints, allGraphs, graphs, skeleton0, dims))]
      final_breaks <- final_breaks[!(checkSimplicityBreaks(final_breaks, pathList, loopList, graphs, skeleton0, nodeList, terminalNodes, hasTrough, dims))]
      
      pathsWithBreaks <- lapply(allGraphs, function(x) {
        which(x %in% breakPoints)
      })
      breaksPerPath <- unlist(lapply(pathsWithBreaks, length))
      for (i in which(breaksPerPath > 0)) {
        newNodes <- pathsWithBreaks[[i]]
        if (allGraphs[[i]][newNodes] %in% final_breaks) {
          tryCatch(
            expr = {
              igraph::E(skeleton0, P = format(allGraphs[[i]][c(newNodes - 2, newNodes - 1)], scientific = FALSE, trim = TRUE))$node_only_dist <- 1
              igraph::E(skeleton0, P = format(allGraphs[[i]][c(newNodes + 1, newNodes + 2)], scientific = FALSE, trim = TRUE))$node_only_dist <- 1
            }, error = function(e) {
              
            }
          )
          
          newNodes <- c(newNodes - 1, newNodes + 1)
          nodeList <- c(nodeList, allGraphs[[i]][newNodes])
          allGraphs[[i]] <- list(allGraphs[[i]][1:(newNodes[1])], allGraphs[[i]][(newNodes[2]):length(allGraphs[[i]])])
        }
        
        # graphIDs = range(V(skeleton0)$graphID[names(V(skeleton0)) %in% format(allGraphs[[i]], scientific = FALSE, trim = TRUE)], na.rm = TRUE)
        # Had to break this above line into this to stop a range(numeric(0)) error from happening
        skelInPaths <- igraph::V(skeleton0)$graphID[names(igraph::V(skeleton0)) %in% format(allGraphs[[i]], scientific = FALSE, trim = TRUE)]
        if (identical(skelInPaths, numeric(0))) {
          graphIDs <- c(Inf, -Inf)
        } else {
          graphIDs <- range(skelInPaths, na.rm = TRUE)
        }
        
        igraph::V(skeleton0)$graphID[which(igraph::V(skeleton0)$graphID == graphIDs[2])] <- graphIDs[1]
        igraph::V(skeleton0)$graphID[which(names(igraph::V(skeleton0)) %in% format(allGraphs[[i]][newNodes], scientific = FALSE, trim = TRUE))] <- graphIDs[1]
      }
      allGraphs <- lapply(rapply(allGraphs, enquote, how = "unlist"), eval)
    } else {
      # empty
      final_breaks <- numeric(0)
    }
    
    return(list('allGraphs' = allGraphs, 'skeleton0' = skeleton0, 'final_breaks' = final_breaks, 'nodeList' = nodeList))
  }
  
  isTroughNode <- function(rows, j) {
    if (any(rows[1:(j - 1)] < rows[j] - 1) & any(rows[(j + 1):length(rows)] < rows[j] - 1)) {
      # Find all vertices to the left of j in tempPath that are at least 2
      # rows higher than j (in the image). Of these vertices, find the
      # vertex that is closest to j from the left in tempPath
      lowerEnd <- max(which(rows[1:(j - 1)] < rows[j] - 1))
      # Find all vertices to the right of j in tempPath that are at least 2
      # rows higher than j (in the image). Of these vertices, find the
      # vertex that is closest to j from the right in tempPath
      upperEnd <- min(which(rows[(j + 1):length(rows)] < rows[j] - 1))
      # If none of the vertices between the lowerEnd and upperEnd are one or
      # more rows lower than j, then assign j as a trough node
      if (!any(rows[lowerEnd:(j + upperEnd)] > rows[j])) {
        isTrough <- TRUE
      } else {
        isTrough <- FALSE
      }
    } else {
      isTrough <- FALSE
    }
    return(isTrough)
  }
  
  makeGraphs <- function(allPaths, skeleton0, breakPoints) {
    oldVerts <- igraph::V(skeleton0)$name
    # delete breakPoints from the skeleton
    formatted_breakPoints <- format(breakPoints, scientific = FALSE, trim = TRUE)
    if (any(as.character(formatted_breakPoints) %in% names(igraph::V(skeleton0)))) {
      skeleton0 <- igraph::delete_vertices(skeleton0, v = as.character(formatted_breakPoints))
    }
    
    # find connected components. Each connected component is a graph, and
    # igraph::components assigns each component a group number. This number
    # is the graphID.
    comps <- igraph::components(skeleton0)
    graphIDs <- rep(NA, length(oldVerts))
    # assign the ID to the original vertices that are not break points
    graphIDs[which(!(oldVerts %in% formatted_breakPoints))] <- comps$membership
    # lists of vertex names grouped by graph
    graphs <- lapply(1:max(comps$membership), function(x) as.numeric(igraph::V(skeleton0)$name[comps$membership == x]))
    
    return(list(graphs, graphIDs))
  }
  
  updateBreakPointsForComponent <- function(pathList, nodeList, breakPoints, terminalNodes, dims, allPaths) {
    # create a graph with vertices for each node and edges between the first and last pixel / vertex in each path
    nodeGraph <- getNodeGraph(pathList, nodeList)
    # flag candidates as good if the following are true: 
    #     (1) there NOT is more than one route between the first and last 
    #         nodes in the path containing the candidate 
    #     (2) the path does NOT contain a terminal node 
    #     (3) the candidate break point is NOT within 4 of the first or 
    #         last node in the path 
    #     (4) the path contains more than 10 vertices.
    goodBreaks <- checkBreakPoints(breakPoints = breakPoints, allPaths = pathList, nodeGraph = nodeGraph, terminalNodes = terminalNodes, dims)
    breakPoints <- breakPoints[goodBreaks]
    # list the break(s) in each path or 'integer(0)' if the path does not contain a break
    pathsWithBreaks <- lapply(allPaths, function(x) {
      which(x %in% breakPoints)
    })
    # number of breaks per path
    breaksPerPath <- unlist(lapply(pathsWithBreaks, length))
    # update the list of breaks: if a path has more than one break, take the average of the breaks (rounding up) and
    # remove the original breaks so that each path has either 0 or 1 break.
    for (i in which(breaksPerPath > 1))
    { # if current path has more than one break, average the breaks and round up
      newBreak <- floor(mean(which(allPaths[[i]] %in% breakPoints)))
      # drop pre stack breaks in current path from the pre stack breaks list
      breakPoints <- breakPoints[which(!(breakPoints %in% allPaths[[i]]))]
      # add "new break" for current path to list
      breakPoints <- c(breakPoints, allPaths[[i]][newBreak])
    }
    return(breakPoints)
  }
  
  updateAllBreakPoints <- function(comps, dims) {
    # discard bad break points
    n <- length(comps)
    for (i in 1:n){
      tempPaths <- comps[[i]]$paths$pathList
      if (length(tempPaths) > 0){
        comps[[i]]$nodes$breakPoints <- updateBreakPointsForComponent(pathList = tempPaths, 
                                                                      nodeList = comps[[i]]$nodes$nodeList, 
                                                                      breakPoints = comps[[i]]$nodes$breakPoints, 
                                                                      terminalNodes = comps[[i]]$nodes$terminalNodes, 
                                                                      allPaths = comps[[i]]$paths$allPaths,
                                                                      dims = dims)
      } else {
        comps[[i]]$nodes$breakPoints <- list()
      }
    }
    return(comps)
  }
  
  comps <- getAllBreakPoints(comps = comps, dims = dims)
  comps <- updateAllBreakPoints(comps = comps, dims = dims)
  comps <- isolateAllGraphs(comps = comps, dims = dims)
  comps <- assignGraphIDs(comps = comps)
  
  return(comps)
}
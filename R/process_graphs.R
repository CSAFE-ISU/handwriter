createGraphLists <- function(comps, dims) {
  angleDiff <- function(fromIndex, toIndex, dims) {
    vecs <- toRC(c(fromIndex, toIndex), dims)
    diff <- c(vecs[1, 1] - vecs[2, 1], vecs[2, 2] - vecs[1, 2])
    return(atan2(diff[1], diff[2]))
  }
  
  createGraphListsForComponent <- function(allPaths, graphs, nodeList, nodeConnections, terminalNodes, dims) {
    # Assign nodes to each letter
    nodesinGraph <- replicate(length(graphs), list(NA))
    connectingNodesinGraph <- replicate(length(graphs), list(NA))
    terminalNodesinGraph <- replicate(length(graphs), list(NA))
    
    for (i in 1:length(graphs))
    {
      nodesinGraph[[i]] <- graphs[[i]][which(graphs[[i]] %in% nodeList)]
      connectingNodesinGraph[[i]] <- graphs[[i]][which(graphs[[i]] %in% nodeConnections)]
      terminalNodesinGraph[[i]] <- graphs[[i]][which(graphs[[i]] %in% terminalNodes)]
    }
    
    graphList <- replicate(length(graphs), list(path = NA, nodes = NA), simplify = FALSE)
    for (i in 1:length(graphs))
    {
      graphList[[i]]$path <- graphs[[i]]
      # graphList[[i]]$nodes = nodesinGraph[[i]][nodeOrder[[i]]]
      graphList[[i]]$allPaths <- pathLetterAssociate(allPaths, graphs[[i]])
    }
    
    letterAdj <- list()
    nodeOrder <- replicate(list(), n = length(graphs))
    decCode <- rep("", length(graphs))
    connectivityScores <- replicate(list(), n = length(graphs))
    
    for (i in 1:length(graphs))
    {
      if (length(nodesinGraph[[i]]) > 0) {
        graphList[[i]]$adjMatrix <- matrix(0, ncol = length(nodesinGraph[[i]]), nrow = length(nodesinGraph[[i]]))
        
        pathStarts <- unlist(lapply(graphList[[i]]$allPaths, function(x) x[1]))
        pathEnds <- unlist(lapply(graphList[[i]]$allPaths, function(x) x[length(x)]))
        
        connectivityScores[[i]] <- getConnectivity(pathEndings = c(pathStarts, pathEnds), nodesSingle = nodesinGraph[[i]])
        
        nodeOrder[[i]] <- getNodeOrder(graphs[[i]], nodesinGraph[[i]], connectivityScores[[i]], dims)
        
        nodeSet <- nodesinGraph[[i]][order(nodeOrder[[i]])]
        
        warn <- FALSE
        for (j in 1:length(pathStarts))
        {
          if (!(pathStarts[j] %in% nodeSet)) {
            warning(paste0("Maybe a loop that didn't merge with node. graphList[[", i, "]]"))
            warn <- TRUE
          } else {
            pathStarts[j] <- which(nodeSet == pathStarts[j])
          }
          
          if (!(pathEnds[j] %in% nodeSet)) {
            warning(paste0("Maybe a loop that didn't merge with node. graphList[[", i, "]]"))
            warn <- TRUE
          } else {
            pathEnds[j] <- which(nodeSet == pathEnds[j])
          }
        }
        if (warn) {
          next
        }
        
        graphList[[i]]$adjMatrix[cbind(pathStarts, pathEnds)] <- 1
        graphList[[i]]$adjMatrix[cbind(pathEnds, pathStarts)] <- 1
        binCode <- t(graphList[[i]]$adjMatrix)[!upper.tri(graphList[[i]]$adjMatrix)]
        lenBinCode <- length(binCode)
        binCode <- c(rep(0, (-1 * lenBinCode) %% 4), binCode)
        for (j in 1:(length(binCode) / 4))
        {
          decCode[i] <- paste0(decCode[i], LETTERS[sum(binCode[(4 * (j - 1) + 1):(4 * j)] * 2^((4:1) - 1)) + 1])
        }
        graphList[[i]]$letterCode <- decCode[i]
        graphList[[i]]$nodes <- sort(nodesinGraph[[i]][order(nodeOrder[[i]])])
        graphList[[i]]$connectingNodes <- sort(connectingNodesinGraph[[i]][order(nodeOrder[[i]])])
        graphList[[i]]$terminalNodes <- sort(terminalNodesinGraph[[i]][order(nodeOrder[[i]])])
        colnames(graphList[[i]]$adjMatrix) <- format(graphList[[i]]$nodes, scientific = FALSE, trim = TRUE)
        rownames(graphList[[i]]$adjMatrix) <- format(graphList[[i]]$nodes, scientific = FALSE, trim = TRUE)
      } else {
        graphList[[i]]$adjMatrix <- matrix(0, ncol = 0, nrow = 0)
        graphList[[i]]$nodes <- sort(nodesinGraph[[i]])
        graphList[[i]]$connectingNodes <- sort(connectingNodesinGraph[[i]])
        graphList[[i]]$terminalNodes <- sort(terminalNodesinGraph[[i]])
        graphList[[i]]$letterCode <- "A"
      }
    }
    
    return(graphList)
  }
  
  getConnectivity <- function(pathEndings, nodesSingle) {
    res <- rep(NA, length(nodesSingle))
    for (j in 1:length(nodesSingle))
    {
      res[j] <- sum(pathEndings == nodesSingle[j])
    }
    return(res)
  }
  
  getNodeOrder <- function(letter, nodesInGraph, nodeConnectivity, dims) {
    
    if (length(nodesInGraph) == 0) {
      return(nodesInGraph)
    } else {
      nodeOrder <- rep(NA, length(nodesInGraph))
      
      nodeCounter <- 1
      maxConnectivity <- max(nodeConnectivity)
      
      for (i in maxConnectivity:1)
      {
        thisTier <- which(nodeConnectivity == i)
        if (length(thisTier) == 1) {
          nodeOrder[thisTier[1]] <- nodeCounter
          if (i == maxConnectivity) {
            baseNode <- nodesInGraph[thisTier[1]]
          }
          nodeCounter <- nodeCounter + 1
        } else if (length(thisTier) > 1) {
          if (i == maxConnectivity) {
            # Left most node is first. If tie, then higher one.
            nodeOrder[thisTier[1]] <- nodeCounter
            nodeCounter <- nodeCounter + 1
            baseNode <- nodesInGraph[thisTier[1]]
            thisTier <- thisTier[-1]
          }
          count <- 1
          angles <- rep(NA, length(thisTier))
          for (point in nodesInGraph[thisTier])
          {
            angles[count] <- angleDiff(baseNode, point, dims)
            count <- count + 1
          }
          angleOrder <- order(angles, decreasing = TRUE)
          nodeOrder[thisTier[angleOrder]] <- nodeCounter:(nodeCounter + length(thisTier) - 1)
          nodeCounter <- nodeCounter + length(thisTier)
        }
      }
      return(nodeOrder)
    }
  }
  
  toRC <- function(nodes, dims) {
    cs <- (nodes - 1) %/% dims[1] + 1
    rs <- (nodes - 1) %% dims[1] + 1
    return(matrix(c(rs, cs), ncol = 2))
  }
  
  n <- length(comps)
  for (i in 1:n){
    if (length(comps[[i]]$paths$graphs) > 0){
      comps[[i]]$paths$graphList <- createGraphListsForComponent(allPaths = comps[[i]]$paths$allGraphs, 
                                                                 graphs = comps[[i]]$paths$graphs, 
                                                                 nodeList = comps[[i]]$nodes$nodeList, 
                                                                 nodeConnections = comps[[i]]$nodes$nodeConnections, 
                                                                 terminalNodes = comps[[i]]$nodes$terminalNodes, 
                                                                 dims = dims)
    } else {
      comps[[i]]$paths$graphList <- list()
    }
  }
  return(comps)
}

organizeGraphs <- function(comps) {
  organizeGraphsForComponent <- function(skeleton0) {
    # make an empty list for each graphID
    graphs <- replicate(n = length(na.omit(unique(igraph::V(skeleton0)$graphID))), list())
    # get the graphID of each vertex
    strs <- names(igraph::V(skeleton0))
    
    # for each graphID
    for (i in 1:length(na.omit(unique(V(skeleton0)$graphID))))
    {
      tmp <- as.numeric(as.factor(V(skeleton0)$graphID))
      graphs[[i]] <- as.numeric(strs[which(tmp == i)])
    }
    return(graphs)
  }
  
  n <- length(comps)
  for (i in 1:n){
    temp_graph <- comps[[i]]$skeletons$skeleton0
    if (length(igraph::V(temp_graph)$graphID) > 0){
      comps[[i]]$paths$graphs <- organizeGraphsForComponent(temp_graph)
    } else {
      comps[[i]]$paths$graphs <- list()
    }
  }
  
  return(comps)
}
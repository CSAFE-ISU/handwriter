## Junction Detection.
## Provide skeletonized image from ThinText.

# A black pixel becomes a node if its removal creates exactly one or at least
# three 4-connected black components in its 1-neighborhood.

# Also from Zhang thinning paper (allegedly)

#' Internal function for counting 4-connected components around a pixel.
countChanges = function(coords, img)
{
  rr = coords[1]
  cc = coords[2]
  if(rr>1 & cc>1 & rr<dim(img)[1] & cc<dim(img)[2])
  {
    neighbs = c(t(img[(rr-1):(rr+1),][,(cc-1):(cc+1)]))[c(2,3,6,9,8,7,4,1,2)]
    return(sum(neighbs == 1 & c(neighbs[-1], neighbs[1]) == 0))
  }
  else
  {
    stop("Please use `crop` to crop your image. Not padded around outside.")
  }
}

#' Internal function for identifying which neighbors are black.
whichNeighbors = function(coords, img)
{
  rr = coords[1]
  cc = coords[2]
  neighbs = c(t(img[(rr-1):(rr+1),][,(cc-1):(cc+1)]))[c(2,3,6,9,8,7,4,1)]
  yesNeighbs = which(neighbs == 0)
  res = as.matrix(rep(0, 8), nrow = 1)
  res[yesNeighbs] = 1

  return(res)
}

#' Internal function for identifying which neighbors are black excluding diagonals
#' to the middle point when a non-diagonal between those two vertices exists.
whichNeighbors0 = function(coords, img)
{
  rr = coords[1]
  cc = coords[2]
  neighbs = c(t(img[(rr-1):(rr+1),][,(cc-1):(cc+1)]))[c(2,3,6,9,8,7,4,1)]
  yesNeighbs = which(neighbs == 0)
  res = as.matrix(rep(0, 8), nrow = 1)
  res[yesNeighbs] = 1

  if(res[1] == 1 | res[3] == 1)
    res[2] = 0
  if(res[3] == 1 | res[5] == 1)
    res[4] = 0
  if(res[5] == 1 | res[7] == 1)
    res[6] = 0
  if(res[7] == 1 | res[1] == 1)
    res[8] = 0
  return(res)
}

#' Internal function to merge nodes that are very close together.
findMergeNodes = function(skel_graph, mergeMat)
{
  newNodes = rep(NA, dim(mergeMat)[1])
  for(i in 1:dim(mergeMat)[1])
  {
    fromNode = as.character(format(mergeMat[i,1], scientific = FALSE, trim = TRUE))
    toNode = as.character(format(mergeMat[i,2], scientific = FALSE, trim = TRUE))
    path = shortest_paths(skel_graph, from = fromNode, to = toNode, weights = E(skel_graph)$pen_dist)$vpath[[1]]
    len = length(path)
    newNodes[i] = as.numeric(names(path[ceiling(len/2)]))
  }
  return(newNodes)
}

#' Internal function for getting a list of all non loop paths in a writing sample.
AllUniquePaths = function(adj, graph, graph0)
{
  #Gets all paths that are not loops
  #paths = apply(adj, 1, LooplessPaths, graph = graph, graph0 = graph0)
  paths = list()
  if(dim(adj)[1] == 0){return(NULL)}
  for(i in 1:dim(adj)[1])
  {
    fromNode = as.character(format(adj[i,1], scientific = FALSE, trim = TRUE))
    toNode = as.character(format(adj[i,2], scientific = FALSE, trim = TRUE))

    while(shortest.paths(graph0, v = fromNode, to = toNode, weights = E(graph0)$nodeOnlyDist) < 3 & shortest.paths(graph0, v = fromNode, to = toNode, weights = E(graph0)$nodeOnlyDist) >= 1)
    {
      shortest = shortest_paths(graph0, from = fromNode, to = toNode, weights = E(graph0)$nodeOnlyDist)
      len = length(unlist(shortest[[1]]))
      paths = c(paths, list(as.numeric(names(shortest$vpath[[1]]))))
      if(len>2)
      {
        graph = delete.edges(graph, paste0(names(shortest$vpath[[1]])[len%/%2], "|", names(shortest$vpath[[1]])[len%/%2+1]))
        graph0 = delete.edges(graph0, paste0(names(shortest$vpath[[1]])[len%/%2], "|", names(shortest$vpath[[1]])[len%/%2+1]))
      }
      else if(len == 2)
      {
        graph0 = delete.edges(graph0, paste0(names(shortest$vpath[[1]])[1], "|", names(shortest$vpath[[1]])[2]))
      }
      else
        stop("There must be some mistake. Single node should have nodeOnlyDist path length of 0.")
    }
  }

  return(paths)
}

#' Internal function for getting looped paths.
#' 
#' @importFrom utils combn
getLoops = function(nodeList, graph, graph0, pathList, dims)
{
  vertexNames = names(V(graph0))

  fullGraph0 = graph0
  
  used = unlist(lapply(pathList, function(x){x[-c(1, length(x))]}))
  unused = as.numeric(vertexNames)[which(!(as.numeric(vertexNames) %in% used))]
  unusedAdj = matrix(1, ncol = length(unused), nrow = length(unused))
  colnames(unusedAdj) = as.character(format(unused, scientific = FALSE, trim = TRUE))
  rownames(unusedAdj) = as.character(format(unused, scientific = FALSE, trim = TRUE))
  if(length(nodeList) > 1)
  {
    unusedAdj[,which(unused %in% nodeList)][which(unused %in% nodeList),] = 0
  }
  else
    unusedAdj[which(unused %in% nodeList),which(unused %in% nodeList)] = 0
  unusedGraph = graph_from_adjacency_matrix(unusedAdj, mode = "undirected")

  graph0 = intersection(graph0, unusedGraph, keep.all.vertices = TRUE)
  graph = intersection(graph, graph0, byname = TRUE, keep.all.vertices = TRUE)
  check = unused[degree(graph0, as.character(format(unused, scientific = FALSE, trim = TRUE))) > 1]
  check = check[which(check %in% nodeList)]

  loopList = list()

  neighbors = neighborhood(graph, nodes = as.character(check))

  if(any(unlist(lapply(neighbors, length)) > 3))
  {
    cat("At least 1 of the nodes in the potential loops has more than 2 neighbors after removal of the connections. Try again! \nThe nodes in question are: \n", dput(names(neighbors)[which(unlist(lapply(neighbors, length)) > 3)]))
  }

  ## Get paths that start and end at the same point, where that point is a node in nodeList
  if(length(neighbors) > 0)
  {
    for(i in 1:length(neighbors))
    {
      neigh = as.numeric(names(neighbors[[i]]))
      graph = delete.edges(graph, paste0(neigh[1], "|", neigh[2]))
      if(distances(graph, v = as.character(neigh[1]), to = as.character(neigh[2])) < Inf)
      {
        newPath = as.numeric(names(unlist(shortest_paths(graph, from = format(neigh[1], scientific = FALSE), to = format(neigh[2], scientific = FALSE), weights = E(graph)$pen_dist)$vpath)))
        loopList = append(loopList, list(c(newPath, newPath[1])))
      }
    }
  }
  

  
  ## Eliminate loop paths that we have found and find ones that dont have vertex on the loop. This is caused by combining of nodes that are close together.
  used = as.numeric(unique(c(unlist(pathList), unlist(loopList))))
  unused = as.numeric(vertexNames)[which(!(as.numeric(vertexNames) %in% used))]
  remaining0 = induced_subgraph(graph0, vids = format(c(unused, nodeList), scientific = FALSE, trim = TRUE))
  numNeighbors = lapply(neighborhood(remaining0, nodes = V(remaining0)), length)
  remaining0 = induced_subgraph(remaining0, vids = V(remaining0)[numNeighbors > 1])
  
  roots = format(nodeList[which(nodeList %in% names(V(remaining0)))], scientific = FALSE, trim = TRUE)
  
  if(length(roots) > 0)
  {
    for(i in 1:length(roots))
    {
      loopPart1 = names(na.omit(dfs(remaining0, roots[i], unreachable = FALSE)$order))
      loopPart2 = shortest_paths(fullGraph0, from = loopPart1[length(loopPart1)], to = roots[i])$vpath[[1]][-1]
      loopList = append(loopList, list(as.numeric(c(loopPart1, names(loopPart2)))))
    }
  }
  
  ## Now get loops that are more difficult. They are close to nodes, but separated by paths already found previously. Have to dig a little further.
  remaining0 = induced_subgraph(graph0, vids = format(unused, scientific = FALSE, trim = TRUE))
  used = as.numeric(unique(c(unlist(pathList), unlist(loopList))))
  unused = as.numeric(vertexNames)[which(!(as.numeric(vertexNames) %in% used))]
  if(length(unused) > 0)
  {
    ends = lapply(neighborhood(fullGraph0, order = 2, nodes = format(unused, scientific = FALSE, trim = TRUE)), function(x) nodeList[which(format(nodeList, scientific = FALSE, trim = TRUE) %in% names(x))])
    
    roots = format(unique(unlist(ends)), scientific = FALSE, trim = TRUE)
    if(length(roots) > 0)
    {
      ends = neighborhood(fullGraph0, order = 2, nodes = roots)
      ends = unlist(lapply(ends, function(x) names(x)[which(names(x) %in% format(unused, scientific = FALSE, trim = TRUE))][1]))
      
      for(i in 1:length(roots))
      {
        loopPart1 = names(na.omit(dfs(remaining0, ends[i], unreachable = FALSE)$order))
        loopPart2a = shortest_paths(fullGraph0, from = loopPart1[1], to = roots[i])$vpath[[1]][-1]
        loopPart2b = shortest_paths(fullGraph0, from = loopPart1[length(loopPart1)], to = roots[i])$vpath[[1]][-1]
        loopList = append(loopList, list(as.numeric(c(rev(names(loopPart2a)), loopPart1, names(loopPart2b)))))
      }
    }
  }
  
  
  ## And a little deeper
  remaining0 = induced_subgraph(graph0, vids = format(unused, scientific = FALSE, trim = TRUE))
  used = as.numeric(unique(c(unlist(pathList), unlist(loopList))))
  unused = as.numeric(vertexNames)[which(!(as.numeric(vertexNames) %in% used))]
  if(length(unused) > 0)
  {
    ends = lapply(neighborhood(fullGraph0, order = 3, nodes = format(unused, scientific = FALSE, trim = TRUE)), function(x) nodeList[which(format(nodeList, scientific = FALSE, trim = TRUE) %in% names(x))])
    
    roots = format(unique(unlist(ends)), scientific = FALSE, trim = TRUE)
    if(length(roots) > 0)
    {
      ends = neighborhood(fullGraph0, order = 3, nodes = roots)
      ends = unlist(lapply(ends, function(x) names(x)[which(names(x) %in% format(unused, scientific = FALSE, trim = TRUE))][1]))
      
      for(i in 1:length(roots))
      {
        loopPart1 = names(na.omit(dfs(remaining0, ends[i], unreachable = FALSE)$order))
        loopPart2a = shortest_paths(fullGraph0, from = loopPart1[1], to = roots[i])$vpath[[1]][-1]
        loopPart2b = shortest_paths(fullGraph0, from = loopPart1[length(loopPart1)], to = roots[i])$vpath[[1]][-1]
        loopList = append(loopList, list(as.numeric(c(rev(names(loopPart2a)), loopPart1, names(loopPart2b)))))
      }
    }
  }
  
  ## All that remains now is perfect loops. Start and end at same point with no intersections or end points.
  remaining0 = induced_subgraph(remaining0, vids = V(remaining0)[!(names(V(remaining0)) %in% unlist(loopList))])
  while(TRUE)
  {
    if(length(V(remaining0)) > 0)
    {
      perfectLoop = names(na.omit(dfs(remaining0, V(remaining0)[1], unreachable = FALSE)$order))
      remaining0 = delete.vertices(remaining0, v = perfectLoop)
      loopList = append(loopList, list(as.numeric(c(perfectLoop, perfectLoop[1]))))
    }
    else break
  }
  
  return(loopList)
}

#' Internal function called by processHandwriting that eliminates breakpoints based on rules to try to coherently separate letters
checkBreakPoints = function(candidateNodes, allPaths, nodeGraph, terminalNodes, dims)
{
  #Check rules for candidate breakpoints
  breakFlag = rep(TRUE, length(candidateNodes))

  for(i in 1:length(allPaths))
  {
    tempPath = format(allPaths[[i]], scientific = FALSE, trim = TRUE)
    nodeChecks = which(candidateNodes %in% tempPath)
    tempNodeGraph = delete.edges(nodeGraph, paste0(tempPath[1], "|", tempPath[length(tempPath)]))

    if(distances(tempNodeGraph, v = tempPath[1], to = tempPath[length(tempPath)]) < Inf)
    {
      #No breaking on multiple paths between nodes.
      breakFlag[nodeChecks] = FALSE
    }
    else if(any(tempPath %in% terminalNodes))
    {
      # No break if path has an endpoint
      breakFlag[nodeChecks] = FALSE
    }
    else if(any(which(tempPath %in% c(candidateNodes[nodeChecks])) <= 4 | which(tempPath %in% c(candidateNodes[nodeChecks])) >= length(tempPath) - 3) | length(tempPath) <= 10)
    {
      #No breaks too close to a vertex
      breakFlag[nodeChecks[which(candidateNodes[nodeChecks] <= 5 | candidateNodes[nodeChecks] >= length(tempPath) - 4)]] = FALSE
    }
  }

  return(breakFlag)
}

#' Internal function that uses existing breakPoint list to assign letters to the nodes in nodeGraph0.
letterPaths = function(allPaths, nodeGraph0, breakPoints)
{
  oldVerts = V(nodeGraph0)$name
  if(any(as.character(format(breakPoints, scientific = FALSE, trim = TRUE)) %in% names(V(nodeGraph0))))
    nodeGraph0 = delete_vertices(nodeGraph0, v = as.character(format(breakPoints, scientific = FALSE, trim = TRUE)))
  grIDs = rep(NA, length(V(nodeGraph0)))
  dists = distances(nodeGraph0, v = names(V(nodeGraph0)), to = names(V(nodeGraph0)), weights = E(nodeGraph0)$nodeOnlyDist)
  vertList = V(nodeGraph0)$name

  grPaths = list()
  i = 1
  while(length(vertList) > 0)
  {
    tempIDs = which(dists[which(V(nodeGraph0)$name == vertList[1]),] < Inf)
    grPaths = c(grPaths, list(as.numeric(V(nodeGraph0)$name[tempIDs])))
  #  grIDs[V(nodeGraph0)$name %in% as.character(grPaths[[i]])] = i
    grIDs[tempIDs] = i
    vertList = vertList[vertList %in% setdiff(vertList, format(grPaths[[i]], scientific = FALSE, trim = TRUE))]
    i = i+1
  }
  grIDs2 = rep(NA, length(oldVerts))
  grIDs2[which(!(oldVerts %in% format(breakPoints, scientific = FALSE, trim = TRUE)))] = grIDs

  return(list(grPaths, grIDs2))
}

#' getNodes
#'
#' Detect intersection points of an image thinned with thinImage.
#' @param img Thinned binary image.
#' @keywords vertex detection, Zhang, Suen
#' @return Returns image matrix. 1 is blank, 0 is a node.
#' @examples
#' data(london)
#' london = crop(london)
#' london_thin = thinImage(london)
#' london_nodes = getNodes(london_thin, dim(london))
#'
#' ## Not Run
#' #data(message)
#' #message = crop(message)
#' #message_thin = thinImage(message)
#' #message_nodes = getNodes(message_thin, dim(message))
#'
#' @export

getNodes = function(indices, dims)
{
  ## First, we find endpoints and intersections of skeleton.
  img = matrix(1, ncol = dims[2], nrow = dims[1])
  img[indices] = 0
  img.m = cbind(((indices-1) %% dims[1]) + 1, ((indices - 1) %/% dims[1]) + 1)
  changeCount = matrix(apply(X = img.m, MARGIN = 1, FUN = countChanges, img = img), byrow = F, nrow = 1)
  nodes = matrix(1, dims[1], dims[2])
  nodes[indices] = ifelse(changeCount == 1 | changeCount >= 3, 0, 1)

  ## If there is a 2x2 block in the thinned image and none of those pixels are nodes, make one of them a node.
  ## All will have connectivity of 2. Choose pixel with most neighbors as node. Also make opposite diagonal pixel a node.
  ## When nodes are combined later this will form 1 node that absorbs all connections.

  node2by2fill = function(coords, img)
  {
    rr = coords[1]
    cc = coords[2]
    
    if(img[rr,cc] == 0 & img[rr+1, cc] == 0 & img[rr,cc+1] == 0 & img[rr+1,cc+1] == 0)
    {
      index2by2 = matrix(c(rr,cc,rr+1, cc, rr,cc+1, rr+1,cc+1), byrow = TRUE, ncol = 2)
      numNeighbors = colSums(apply(X = index2by2, MARGIN = 1, FUN = whichNeighbors, img = img))
      newNode = index2by2[which.max(numNeighbors),]
      oppositeCorner = index2by2[(4:1)[which.max(numNeighbors)],]
      return(c(newNode[1] + (newNode[2] - 1)*dim(img)[1], oppositeCorner[1] + (oppositeCorner[2] - 1)*dim(img)[1]))
    }
    else
      return(c(NA,NA))
  }
  
  nodes2by2 = t(apply(img.m, 1, FUN = node2by2fill, img = img))
  nodes[c(nodes2by2[apply(nodes2by2, 1, function(x){all(!is.na(x))}),])] = 0
  
  return(list(which(nodes == 0), c(indices[changeCount >= 3], c(nodes2by2[apply(nodes2by2, 1, function(x){all(!is.na(x))}),]))))
}

#' processHandwriting
#'
#' Huge step in handwriting processing. Takes in thin image form and the breakpoints suggested by getNodes
#' and parses the writing into letters. Returns final letter separation points, a list of the paths in the image,
#' and a list of the letter paths in the image.
#'
#' @param img Thinned binary image.
#' @param nodes List of nodes from the getNodes function.
#' return(list(breakPoints = finalBreaks, pathList = allPaths, letterList = letters))
#' @return Returns a list of length 3. Object [[1]] (called breakPoints) is the set of final letter separation points.
#' Object [[2]] (called pathList) is a list of the paths between the input specified nodes.
#' Object [[3]] (called letters) is a list of the pixels in the different letters in the handwriting sample.
#'
#' @importFrom reshape2 melt
#' @importFrom grDevices as.raster
#' @importFrom graphics hist
#' @importFrom stats na.omit
#' @importFrom utils install.packages
#' 
#' @import igraph
#'
#' @export

processHandwriting = function(img, dims)
{
  # Next, we have to follow certain rules to find non intersection breakpoints.
  
  cat("Starting Processing...\n")
  indices = img
  img = matrix(1, nrow = dims[1], ncol = dims[2])
  img[indices] = 0
  cat("Getting Nodes...\n")
  nodeList = getNodes(indices, dims)
  nodeConnections = nodeList[[2]]
  terminalNodes = nodeList[!(nodeList %in% nodeConnections)]
  nodeList = nodeList[[1]]
  img.m = cbind(((indices-1) %% dims[1]) + 1, ((indices - 1) %/% dims[1]) + 1)
  
  neighborList = matrix(NA, nrow = indices, ncol = 8)
  neighborList = t(apply(as.matrix(img.m, ncol = 2), 1, whichNeighbors, img = img))
  graphdf = melt(neighborList)
  graphdf = subset(graphdf, value == 1)
  graphdf$from = indices[graphdf$Var1]
  graphdf$to = graphdf$from + c(-1, dims[1]-1, dims[1], dims[1] + 1, 1, 1-dims[1], -dims[1], -1-dims[1])[graphdf$Var2]
  graphdf$man_dist = rep(c(1,2), 4)[graphdf$Var2]
  graphdf$euc_dist = c(1,sqrt(2), 1, sqrt(2), 1, sqrt(2), 1, sqrt(2))[graphdf$Var2]
  graphdf$pen_dist = c(1,3,1,3,1,3,1,3)[graphdf$Var2]
  
  neighborList0 = matrix(NA, nrow = indices, ncol = 8)
  neighborList0 = t(apply(as.matrix(img.m, ncol = 2), 1, whichNeighbors0, img = img))
  graphdf0 = melt(neighborList0)
  graphdf0 = subset(graphdf0, value == 1)
  graphdf0$from = indices[graphdf0$Var1]
  graphdf0$to = graphdf0$from + c(-1, dims[1]-1, dims[1], dims[1] + 1, 1, 1-dims[1], -dims[1], -1-dims[1])[graphdf0$Var2]
  graphdf0$nodeOnlyDist = ifelse(graphdf0$from %in% nodeList | graphdf0$to %in% nodeList, 1, 0)
  graphdf0$from = as.character(format(graphdf0$from, scientific = FALSE, trim = TRUE))
  graphdf0$to = as.character(format(graphdf0$to, scientific = FALSE, trim = TRUE))
  graphdf0 = subset(graphdf0, select = c(from, to, nodeOnlyDist))
  
  graphdf$from = as.character(format(graphdf$from, scientific = FALSE, trim = TRUE))
  graphdf$to = as.character(format(graphdf$to, scientific = FALSE, trim = TRUE))
  graphdf = subset(graphdf, select = c(from, to, man_dist, euc_dist, pen_dist))
  
  skel_graph = graph_from_data_frame(d = graphdf, vertices = as.character(format(indices, scientific = FALSE, trim = TRUE)), directed = FALSE)
  skel_graph0 = graph_from_data_frame(d = graphdf0, vertices = as.character(format(indices, scientific = FALSE, trim = TRUE)), directed = FALSE)
  skel_graph = simplify(skel_graph, remove.multiple = TRUE, edge.attr.comb="mean")
  skel_graph0 = simplify(skel_graph0, remove.multiple = TRUE, edge.attr.comb="mean")
  
  V(skel_graph)$color = ifelse(V(skel_graph)$name %in% nodeList, 1, 0)
  V(skel_graph0)$color = ifelse(V(skel_graph0)$name %in% nodeList, 1, 0)
  
  terminalNodes = nodeList[!(nodeList %in% nodeConnections)]
  dists0 = distances(skel_graph0, v = as.character(format(nodeList, scientific = FALSE, trim = TRUE)), to = as.character(format(nodeList, scientific = FALSE, trim = TRUE)), weights = E(skel_graph0)$nodeOnlyDist)
  adj0 = ifelse(dists0 == 1 | dists0 == 2, 1, 0)
  
  while(TRUE)
  {
    originalNodeList = nodeList
    distsFull = distances(skel_graph0, v = as.character(format(nodeList, scientific = FALSE, trim = TRUE)), to = as.character(format(nodeList, scientific = FALSE, trim = TRUE)), weights = NA)
    distsFull[!upper.tri(distsFull)] = 0
    nodesToMerge = which(distsFull <= 2 & distsFull > 0)
    
    rNodes = ((nodesToMerge-1) %% length(nodeList)) + 1
    cNodes = ((nodesToMerge-1) %/% length(nodeList)) + 1
    mergeSets = cbind(nodeList[rNodes], nodeList[cNodes])
    mergeSets = cbind(mergeSets, apply(mergeSets, 1, function(x){all(!(x %in% terminalNodes))}))
    mergeSets = mergeSets[mergeSets[,3] == 1,c(1,2)]
    mergeSets = matrix(mergeSets, ncol = 2)
    
    if(dim(mergeSets)[1] == 0) break
    
    if(anyDuplicated(c(mergeSets)) > 0)
    {
      duplicates = which(mergeSets %in% mergeSets[apply(matrix(duplicated(c(mergeSets)), ncol = 2), 1, any)])
      rduplicates = ((duplicates - 1) %% dim(mergeSets)[1]) + 1
      duplicateDists = distsFull[matrix(as.character(format(mergeSets[rduplicates,], scientific = FALSE, trim = TRUE)), ncol = 2)]
    }
    newNodes = findMergeNodes(skel_graph, mergeSets)
    nodeList = unique(c(nodeList[!(nodeList %in% c(mergeSets[,c(1,2)]))], newNodes))
    
    ### Migrate connections from original nodes to new nodes
    toDelete = NULL
    nRowCol = dim(adj0)[1]
    for(i in 1:dim(mergeSets)[1])
    {
      whichRowCol = which(colnames(adj0) %in% format(mergeSets[i,c(1,2)], scientific = FALSE, trim = TRUE))
      newConnectivities = apply(matrix(adj0[whichRowCol,], nrow = length(whichRowCol)), 2, function(x) x[1] == 1 | x[2] == 1)
      newConnectivities[is.na(newConnectivities)] = 0
  
      toAdd = dim(adj0)[1]+1
      toDelete = c(toDelete, which(rownames(adj0) %in% format(mergeSets[i,c(1,2)], scientific = FALSE, trim = TRUE)))
      
      adj0 = rbind(cbind(adj0,0),0)
      adj0[,toAdd] = c(newConnectivities,0)
      adj0[toAdd,] = c(newConnectivities,0)
      colnames(adj0)[toAdd] = format(newNodes[i], scientific = FALSE, trim = TRUE)
      rownames(adj0)[toAdd] = format(newNodes[i], scientific = FALSE, trim = TRUE)
    }
    if(length(toDelete) > 0)
      adj0 = as.matrix(adj0[,-toDelete])[-toDelete,]
  }
  
  graphdf0 = as_data_frame(skel_graph0)
  graphdf0$nodeOnlyDist = ifelse(graphdf0$from %in% nodeList | graphdf0$to %in% nodeList, 1, 0.00001)
  skel_graph0 = graph_from_data_frame(graphdf0, directed = FALSE)
  
  adj0[lower.tri(adj0)] = 0
  adj.m = melt(adj0)
  adj.m = subset(adj.m, value == 1)
  names(adj.m) = c("from", "to", "value")
  
  cat("Finding direct paths...")
  pathList = AllUniquePaths(adj.m, skel_graph, skel_graph0)
  cat("and loops...\n")
  loopList = getLoops(nodeList, skel_graph, skel_graph0, pathList, dim(img))

  allPaths = append(pathList, loopList)

  graphdf0 = as_data_frame(skel_graph0)
  graphdf0$nodeOnlyDist = ifelse(graphdf0$from %in% nodeList | graphdf0$to %in% nodeList, 1, 0)
  skel_graph0 = graph_from_data_frame(graphdf0, directed = FALSE)
  
  
  ####################### This is after path finding. Find breakpoints and check rules for removal.
  #Nominate and check candidate breakpoints
  cat("Looking for letter break points...")
  hasTrough = rep(FALSE, length(pathList))
  troughNodes = c()
  candidateNodes = c()
  for(i in 1:length(pathList))
  {
    # Look for troughs in edges.
    tempPath = pathList[[i]]
    if(length(tempPath) > 10)
    {
      rows = ((tempPath-1) %% dims[1]) + 1
      for(j in 5:(length(rows)-4))
      {
        if(any(rows[1:(j-1)] < rows[j]-1) & any(rows[(j+1):length(rows)] < rows[j]-1))
        {
          lowerEnd = max(which(rows[1:(j-1)] < rows[j]-1))
          upperEnd = min(which(rows[(j+1):length(rows)] < rows[j]-1))
          if(!any(rows[lowerEnd:(j+upperEnd)] > rows[j]))
          {
            troughNodes = c(troughNodes, tempPath[j])
            hasTrough[i] = TRUE
          }
        }
      }
    }
    if(hasTrough[i] == FALSE)
    {
      candidateNodes = c(candidateNodes, tempPath[ceiling(length(tempPath)/2)])
    }
  }
  breaks = which((((troughNodes[-1]-1) %% dims[1]) + 1) != (((troughNodes[-length(troughNodes)] - 1) %% dims[1]) + 1) |
                   ((((troughNodes[-1]-1) %/% dims[1]) + 1) != (((troughNodes[-length(troughNodes)] - 1) %/% dims[1])) &
                      (((troughNodes[-1]-1) %/% dims[1])) != (((troughNodes[-length(troughNodes)] - 1) %/% dims[1]) + 1)))
  breaks = c(1, breaks, length(troughNodes))
  candidateNodes = c(candidateNodes, troughNodes[ceiling((breaks[-1] + breaks[-length(breaks)])/2)])

 # print(plotPath(candidateNodes, img, img, zoomBorder = NA))
  cat("and discarding bad ones...\n")
  
  goodBreaks = checkBreakPoints(candidateNodes = candidateNodes, allPaths = pathList, nodeGraph = getNodeGraph(pathList, nodeList), terminalNodes = terminalNodes, dims)
  preStackBreaks = candidateNodes[goodBreaks]
  
  pathsWithBreaks = lapply(allPaths, function(x){which(x %in% preStackBreaks)})
  breaksPerPath = unlist(lapply(pathsWithBreaks, length))
  for(i in which(breaksPerPath > 1))
  {
    newBreak = floor(mean(which(allPaths[[i]] %in% preStackBreaks)))
    preStackBreaks = preStackBreaks[which(!(preStackBreaks %in% allPaths[[i]]))]
    preStackBreaks = c(preStackBreaks, allPaths[[i]][newBreak])
  }
  
  ##################### Potential breakpoints (except for stacked letters) found. Break into letter paths.

  cat("Isolating letter paths...\n")

  ## Break on breakpoints and group points by which letter they fall into. Adjust graph accordingly.
  letterList = letterPaths(allPaths, skel_graph0, preStackBreaks)
  letters = letterList[[1]][unlist(lapply(letterList[[1]], length)) > 5]
  
  V(skel_graph0)$letterID = letterList[[2]]
  skel_graph0 = delete.vertices(skel_graph0, V(skel_graph0)[which(V(skel_graph0)$letterID %in% which(unlist(lapply(letterList[[1]], length)) <= 5))])
  V(skel_graph0)$letterID[!is.na(V(skel_graph0)$letterID)] = as.numeric(as.factor(na.omit(V(skel_graph0)$letterID)))
  
  # Remove breakpoints that shouldn't have broken.
  finalBreaks = preStackBreaks[!(checkStacking(preStackBreaks, allPaths, letters, skel_graph0, dims))]
  finalBreaks = finalBreaks[!(checkSimplicityBreaks(finalBreaks, pathList, loopList, letters, skel_graph0, nodeList, terminalNodes, hasTrough, dims))]
  
  breakAddedEndPoints = NULL
  pathsWithBreaks = lapply(allPaths, function(x){which(x %in% preStackBreaks)})
  breaksPerPath = unlist(lapply(pathsWithBreaks, length))
  for(i in which(breaksPerPath > 0))
  {
    newNodes = pathsWithBreaks[[i]]
    if(allPaths[[i]][newNodes] %in% finalBreaks)
    {
      E(skel_graph0, P = format(allPaths[[i]][c(newNodes - 2, newNodes - 1)], scientific = FALSE, trim = TRUE))$nodeOnlyDist = 1
      E(skel_graph0, P = format(allPaths[[i]][c(newNodes + 1, newNodes + 2)], scientific = FALSE, trim = TRUE))$nodeOnlyDist = 1
      newNodes = c(newNodes - 1, newNodes + 1)
      breakAddedEndPoints = c(breakAddedEndPoints, allPaths[[i]][newNodes])
      nodeList = c(nodeList, allPaths[[i]][newNodes])
      allPaths[[i]] = list(allPaths[[i]][1:(newNodes[1])], allPaths[[i]][(newNodes[2]):length(allPaths[[i]])])
    }
    else
    {
      letterIDs = range(V(skel_graph0)$letterID[names(V(skel_graph0)) %in% format(allPaths[[i]], scientific = FALSE, trim = TRUE)], na.rm = TRUE)
      
     V(skel_graph0)$letterID[which(V(skel_graph0)$letterID == letterIDs[2])] = letterIDs[1]
     V(skel_graph0)$letterID[which(names(V(skel_graph0)) %in% format(allPaths[[i]][newNodes], scientific = FALSE, trim = TRUE))] = letterIDs[1]
    }
  }
  
  allPaths = lapply(rapply(allPaths, enquote, how="unlist"), eval)
  
  
  cat("Organizing everything...")
  
  
  letters = replicate(n = length(na.omit(unique(V(skel_graph0)$letterID))), list())
  strs = names(V(skel_graph0))
  for(i in 1:length(na.omit(unique(V(skel_graph0)$letterID))))
  {
    tmp = as.numeric(as.factor(V(skel_graph0)$letterID))
    letters[[i]] = as.numeric(strs[which(tmp == i)])
  }
  
  nodesinGraph = replicate(length(letters), list(NA))
  for(i in 1:length(letters))
  {
      nodesinGraph[[i]] = letters[[i]][which(letters[[i]] %in% nodeList)]
  }
  
  
  letterList = replicate(length(letters), list(path = NA, nodes = NA), simplify=FALSE)
  for(i in 1:length(letters))
  {
    letterList[[i]]$path = letters[[i]]
    #letterList[[i]]$nodes = nodesinGraph[[i]][nodeOrder[[i]]]
    letterList[[i]]$allPaths = pathLetterAssociate(allPaths,letters[[i]])
  }
    
  letterAdj = list()
  nodeOrder = replicate(list(), n = length(letters))
  decCode = rep("", length(letters))
  connectivityScores = replicate(list(), n = length(letters))
  
  getConnectivity = function(pathEndings, nodesSingle)
  {
    res = rep(NA, length(nodesSingle))
    for(j in 1:length(nodesSingle))
    {
      res[j] = sum(pathEndings == nodesSingle[j])
    }
    return(res)
  }
  
  for(i in 1:length(letters))
  {
    if(length(nodesinGraph[[i]]) > 0)
    {
      letterList[[i]]$adjMatrix = matrix(0,ncol = length(nodesinGraph[[i]]), nrow = length(nodesinGraph[[i]]))
      
      pathStarts = unlist(lapply(letterList[[i]]$allPaths, function(x)x[1]))
      pathEnds = unlist(lapply(letterList[[i]]$allPaths, function(x)x[length(x)]))
      
      connectivityScores[[i]] = getConnectivity(pathEndings = c(pathStarts, pathEnds), nodesSingle = nodesinGraph[[i]])
      
      nodeOrder[[i]] = getNodeOrder(letters[[i]], nodesinGraph[[i]], connectivityScores[[i]], dims)

      nodeSet = nodesinGraph[[i]][order(nodeOrder[[i]])]
      for(j in 1:length(pathStarts))
      {
        if(!(pathStarts[j] %in% nodeSet))
        {
          warning(paste0("Maybe a loop that didn't merge with node. letterList[[",i,"]]"))
        }
        else
          pathStarts[j] = which(nodeSet == pathStarts[j])
        
        if(!(pathEnds[j] %in% nodeSet))
        {
          warning(paste0("Maybe a loop that didn't merge with node. letterList[[",i,"]]"))
        }
        else
          pathEnds[j] = which(nodeSet == pathEnds[j])
      }
      letterList[[i]]$adjMatrix[cbind(pathStarts, pathEnds)] = 1
      letterList[[i]]$adjMatrix[cbind(pathEnds, pathStarts)] = 1
      binCode = t(letterList[[i]]$adjMatrix)[!upper.tri(letterList[[i]]$adjMatrix)]
      lenBinCode = length(binCode)
      binCode = c(rep(0, (-1*lenBinCode)%%4), binCode)
      for(j in 1:(length(binCode)/4))
      {
        decCode[i] = paste0(decCode[i], LETTERS[sum(binCode[(4*(j-1)+1):(4*j)]*2^((4:1) - 1))+1])
      }
      letterList[[i]]$letterCode = decCode[i]
      letterList[[i]]$nodes = nodesinGraph[[i]][order(nodeOrder[[i]])]
      colnames(letterList[[i]]$adjMatrix) = format(letterList[[i]]$nodes, scientific = FALSE, trim = TRUE)
      rownames(letterList[[i]]$adjMatrix) = format(letterList[[i]]$nodes, scientific = FALSE, trim = TRUE)
    }
    else
    {
      letterList[[i]]$adjMatrix = matrix(0,ncol = 0, nrow = 0)
      letterList[[i]]$nodes = nodesinGraph[[i]]
      letterList[[i]]$letterCode = "A"
    }
  }
  
  featureSets = extract_character_features(letterList, dims)

  for(i in 1:length(letters))
  {
    letterList[[i]]$characterFeatures = featureSets[[i]]
  }
  
  letterPlaces = matrix(unlist(lapply(featureSets, FUN = function(x) {c(x$line_number, x$order_within_line)})), ncol = 2, byrow = TRUE)
  letterOrder = order(letterPlaces[,1], letterPlaces[,2])
  letterList = letterList[letterOrder]
  
  cat("and done.\n")
  return(list(nodes = nodeList, breakPoints = finalBreaks, letterList = letterList))
}

#' Function associating entries in allPaths to each letter
pathLetterAssociate = function(allPaths,letter){
  associatedPaths = list()
  for(i in 1:length(allPaths)){
    if(all(allPaths[[i]] %in% letter)){
      associatedPaths = c(associatedPaths,list(allPaths[[i]]))
    } 
  }
  return(associatedPaths)
}

#' Internal function for removing breakpoints that separate graphs that are too simple to be split. Remove break if graph on 
#' left and right of the break have 4 or fewer nodes and no loops or double paths. Never remove break on a trough.
checkSimplicityBreaks = function(candidateBreaks, pathList, loopList, letters, nodeGraph0, nodeList, terminalNodes, hasTrough, dims)
{
  tooSimpleFlag = rep(FALSE, length(candidateBreaks))
  for(i in 1:length(pathList))
  {
    tempPath = pathList[[i]]
    nodestoCheck = which(candidateBreaks %in% tempPath)
    if(length(nodestoCheck) >= 1)
    {
      if(!hasTrough[i])
      {
        pathIndex = which(tempPath == candidateBreaks[nodestoCheck])
        
        borderLetters = c(V(nodeGraph0)$letterID[which(V(nodeGraph0)$name == tempPath[pathIndex - 1])],
                          V(nodeGraph0)$letterID[which(V(nodeGraph0)$name == tempPath[pathIndex + 1])])
        left = letters[[borderLetters[1]]]
        right = letters[[borderLetters[2]]]
        
        nodesOnLeft = sum(nodeList %in% left)
        nodesOnRight = sum(nodeList %in% right)
        terminalLeft = sum(terminalNodes %in% left)
        terminalRight = sum(terminalNodes %in% right)
        
        if(nodesOnLeft == 3 & nodesOnRight == 3 & terminalLeft == 2 & terminalRight == 2)
        {
          pathsOnLeft = length(pathLetterAssociate(c(pathList, loopList),left))
          pathsOnRight = length(pathLetterAssociate(c(pathList, loopList), right))
          if(pathsOnLeft == 2 & pathsOnRight == 2)
          {
            tooSimpleFlag[nodestoCheck] = TRUE
          }
        }
      }
    }
  }
  return(tooSimpleFlag)
}

#' Internal function for removing breakpoints that follow all of the rules, but separate two letters that are
#' stacked on top of eachother. Currently, this is done in a very ad hoc, and untested manner. Will look for a better
#' solution in the future.
checkStacking = function(candidateBreaks, allPaths, letters, nodeGraph0, dims)
{
  stackPtFlag = rep(FALSE, length(candidateBreaks))

  for(i in 1:length(allPaths))
  {
    tempPath = allPaths[[i]]
    tempRow = ((tempPath - 1) %% dims[1]) + 1
    tempCol = ((tempPath - 1) %/% dims[1]) + 1
    nodeChecks = which(candidateBreaks %in% tempPath)
    if(length(nodeChecks) == 1)
    {
      if(abs((max(tempRow) - min(tempRow))/(max(tempCol) + 1 - min(tempCol))) > 2)
      {
        stackPtFlag[nodeChecks] = TRUE
      }
      else
      {
        pathIndex = which(tempPath == candidateBreaks[nodeChecks])

        borderLetters = c(V(nodeGraph0)$letterID[which(V(nodeGraph0)$name == tempPath[pathIndex - 1])],
                            V(nodeGraph0)$letterID[which(V(nodeGraph0)$name == tempPath[pathIndex + 1])])
        gr1 = letters[[borderLetters[1]]]
        gr1Rows = ((gr1 - 1) %% dims[1]) + 1
        gr2 = letters[[borderLetters[2]]]
        gr2Rows = ((gr2 - 1) %% dims[1]) + 1

        # Call a break a stack point if the overlap between the bordering letters is
        # less than 10% of the total range of the combined letters.
        
        overlap = min(abs(max(gr1Rows) - min(gr2Rows)), abs(max(gr2Rows) - min(gr1Rows)))
        totalRange = (diff(range(c(gr1Rows,gr2Rows))))
        overlapPercentage = overlap/totalRange
        if(overlapPercentage < .1)
        {
          stackPtFlag[nodeChecks] = TRUE
        }
      }
    }
  }
  return(stackPtFlag)
}

#' @export
countNodes = function(letterList, nodes)
{
  unlist(lapply(letterList, function(x){sum(x %in% nodes)}))
}

#' Internal function for creating a graph from a path list and node list.
getNodeGraph = function(allPaths, nodeList)
{
  nodeGraph = make_empty_graph(directed = FALSE)
  nodeGraph = add_vertices(nodeGraph, length(nodeList), name = format(nodeList, scientific = FALSE, trim = TRUE))
  for(i in 1:length(allPaths))
  {
    nodeGraph = add.edges(nodeGraph, format(c(allPaths[[i]][1], allPaths[[i]][length(allPaths[[i]])]), scientific = FALSE, trim = TRUE))
  }
  return(nodeGraph)
}

#' Internal function for ordering the nodes in a letter.
getNodeOrder = function(letter, nodesInGraph, nodeConnectivity, dims)
{
  toRC = function(nodes, dims)
  {
    cs = (nodes-1)%/%dims[1] + 1
    rs = (nodes-1)%%dims[1] + 1
    return(matrix(c(rs,cs), ncol = 2))
  }
  angleDiff = function(fromIndex, toIndex, dims)
  {
    vecs = toRC(c(fromIndex, toIndex), dims)
    diff = c(vecs[1,1] - vecs[2,1], vecs[2,2] - vecs[1,2])
    return(atan2(diff[1], diff[2]))
  }
  
  if(length(nodesInGraph) == 0)
    return(nodesInGraph)
  else
  {
    nodeOrder = rep(NA, length(nodesInGraph))

    nodeCounter = 1
    maxConnectivity = max(nodeConnectivity)
    
    for(i in maxConnectivity:1)
    {
      thisTier = which(nodeConnectivity == i)
      if(length(thisTier) == 1)
      {
        nodeOrder[thisTier[1]] = nodeCounter
        if(i == maxConnectivity)
        {
          baseNode = nodesInGraph[thisTier[1]]
        }
        nodeCounter = nodeCounter + 1
      }
      else if(length(thisTier) > 1)
      {
        if(i == maxConnectivity)
        {
          #Left most node is first. If tie, then higher one.
          nodeOrder[thisTier[1]] = nodeCounter
          nodeCounter = nodeCounter + 1
          baseNode = nodesInGraph[thisTier[1]]
          thisTier = thisTier[-1]
        }
        count = 1
        angles = rep(NA, length(thisTier))
        for(point in nodesInGraph[thisTier])
        {
          angles[count] = angleDiff(baseNode, point, dims)
          count = count + 1
        }
        angleOrder = order(angles, decreasing = TRUE)
        nodeOrder[thisTier[angleOrder]] = nodeCounter:(nodeCounter + length(thisTier) - 1)
        nodeCounter = nodeCounter + length(thisTier)
      }
    }
    return(nodeOrder)
  }
}

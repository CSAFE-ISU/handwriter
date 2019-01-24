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

#' Internal function to get all non loop paths between two points. Called from AllUniquePaths.
LooplessPaths = function(nodes, graph, graph0)
{
  paths = list()
  fromNode = as.character(nodes[1])
  toNode = as.character(nodes[2])

  while(shortest.paths(graph0, v = fromNode, to = toNode, weights = E(graph0)$nodeOnlyDist) %in% c(1,2))
  {
    shortest = shortest_paths(graph0, from = fromNode, to = toNode, weights = E(graph0)$nodeOnlyDist)
    len = length(unlist(shortest[[1]]))
    paths = c(paths, shortest)
    graph = delete.edges(graph, paste0(names(shortest$vpath[[1]])[-len],"|",names(shortest$vpath[[1]])[-1]))
    graph0 = delete.edges(graph0, paste0(names(shortest$vpath[[1]])[-len],"|",names(shortest$vpath[[1]])[-1]))
  }
  return(paths)
}

#' Internal function for getting a list of all non loop paths in a writing sample.
AllUniquePaths = function(adj, graph, graph0)
{
  #Gets all paths that are not loops
  paths = apply(adj, 1, LooplessPaths, graph = graph, graph0 = graph0)
  pathList = list()

  for(i in 1:length(paths))
  {
    len = length(paths[[i]])
    for(j in 1:(len%/%4))
    {
      pathList = append(pathList, list(as.numeric(names(unlist(paths[[i]][(j-1)*4 + 1]$vpath)))))
    }
  }

  return(pathList)
}

#' Internal function for getting looped paths.
#' 
#' @importFrom utils combn
getLoops = function(nodeList, graph, graph0, pathList, dims)
{
  vertexNames = names(V(graph0))

  used = unlist(lapply(pathList, function(x){x[-c(1, length(x))]}))
  unused = as.numeric(vertexNames)[which(!(as.numeric(vertexNames) %in% used))]
  unusedAdj = matrix(1, ncol = length(unused), nrow = length(unused))
  colnames(unusedAdj) = as.character(unused)
  rownames(unusedAdj) = as.character(unused)
  unusedAdj[which(unused %in% nodeList),][,which(unused %in% nodeList)] = 0 #[as.character(nodeList),][,as.character(nodeList)] = 0
  unusedGraph = graph_from_adjacency_matrix(unusedAdj, mode = "undirected")

  graph0 = intersection(graph0, unusedGraph, keep.all.vertices = TRUE)
  graph = intersection(graph, graph0, byname = TRUE, keep.all.vertices = TRUE)
  check = unused[degree(graph0, as.character(unused)) > 1]
  check = check[which(check %in% nodeList)]

  loopList = list()

  neighbors = neighborhood(graph, nodes = as.character(check))
  edgeRemovalCheck = c()
  for(i in 1:length(check))
  {
    neigh = as.numeric(names(neighbors[[i]]))
    if(((neigh[1] - dims[1] - 1) %in% neigh) & (((neigh[1] - dims[1]) %in% neigh) | (neigh[1] - 1) %in% neigh))
    {
      graph = delete.edges(graph, edges = paste0(neigh[1] - dims[1] - 1, "|", neigh[1]))
    }
    if(((neigh[1] - dims[1] + 1) %in% neigh) & (((neigh[1] - dims[1]) %in% neigh) | (neigh[1] + 1) %in% neigh))
    {
      graph = delete.edges(graph, edges = paste0(neigh[1] - dims[1] + 1, "|", neigh[1]))
    }
    if(((neigh[1] + dims[1] - 1) %in% neigh) & (((neigh[1] + dims[1]) %in% neigh) | (neigh[1] - 1) %in% neigh))
    {
      graph = delete.edges(graph, edges = paste0(neigh[1] + dims[1] - 1, "|", neigh[1]))
    }
    if(((neigh[1] + dims[1] + 1) %in% neigh) & (((neigh[1] + dims[1]) %in% neigh) | (neigh[1] + 1) %in% neigh))
    {
      graph = delete.edges(graph, edges = paste0(neigh[1] + dims[1] + 1, "|", neigh[1]))
    }
    edgeRemovalCheck = combn(as.character(neigh[-1]), 2)
    whichRemove = apply(edgeRemovalCheck, 2, FUN = function(edges, graph){are_adjacent(graph, edges[1], edges[2])}, graph = graph)
    graph = delete.edges(graph, edges = apply(edgeRemovalCheck[,whichRemove], 2, function(x){paste0(x[1], "|", x[2])}))
  }

  neighbors = neighborhood(graph, nodes = as.character(check))

  if(any(unlist(lapply(neighbors, length)) > 3))
    cat("At least 1 of the nodes in the potential loops has more than 2 neighbors after removal of the connections. Try again!")

  for(i in 1:length(neighbors))
  {
    neigh = as.numeric(names(neighbors[[i]]))
    graph = delete.edges(graph, paste0(neigh[1], "|", neigh[2]))
    if(distances(graph, v = as.character(neigh[1]), to = as.character(neigh[2])) < Inf)
    {
      newPath = as.numeric(names(unlist(shortest_paths(graph, from = as.character(neigh[1]), to = as.character(neigh[2]), weights = E(graph)$pen_dist)$vpath)))
      loopList = append(loopList, list(c(newPath, newPath[1])))
    }
  }

  return(loopList)
}

#' makeGifImages
#'
#' Saves a collection of plots, which sequentially display paths in a list, to the provided file path.
#' Also prints some suggested code for imagemagick gif creation. Can be useful for visualizing path lists or letter path lists.
#' @param image Binary image.
#' @param image_thin Thinned binary image.
#' @param allPaths List of paths (probably from processHandwriting returned list).
#' @param file_path Where the plots created should be saved to. Directory path, relative to current working directory.
#' @param filenames Identifying prefix for saved plots. Will be appended with numeric suffixes.
#' @return Nothing. Will print out a terminal command for creating your gif. Just navigate to the directory and copy and paste.
#' @examples
#' #### Not Run
#' # Make csafe_pathList the resulting object from the pathList
#' # item in the processHandwriting returned list
#'
#' # makeGifImages(csafe, csafe_thin, csafe_pathList, "../GifPlots/", "csafePaths")
#'
#' @export

makeGifImages = function(img, img_thin, allPaths, file_path, filenames)
{
  if(length(allPaths) < 100)
  {
    digits = ceiling(log10(length(allPaths)))
    num = c(paste0("000", 1:9), paste0("00", 10:99), paste0("0", 100:999), as.character(1000:9999))

    for(i in 1:length(allPaths))
    {
      p = plotNodes(img, img_thin, allPaths[[i]])
      ggsave(paste0(file_path, filenames, num[i], ".png"), p, device = "png")
    }
  }
  else
    stop("Too many paths. You don't want that many images. If you do, then write your own function.")
  print(paste0("convert -delay 30 -loop 0 ", filenames, "*.png Animate", filenames, ".gif"))
}

#' Internal function called by processHandwriting that eliminates breakpoints based on rules to try to coherently separate letters
checkBreakPoints = function(candidateNodes, allPaths, nodeGraph, dims)
{
  #Check rules for candidate breakpoints
  breakFlag = rep(TRUE, length(candidateNodes))

  for(i in 1:length(allPaths))
  {
    tempPath = allPaths[[i]]
    nodeChecks = which(candidateNodes %in% tempPath)
    tempNodeGraph = delete.edges(nodeGraph, paste0(tempPath[1], "|", tempPath[length(tempPath)]))

    if(tempPath[1] == tempPath[length(tempPath)])
    {
      # Dont break loops.
      # What I think a single stroke edge is???
      breakFlag[nodeChecks] = FALSE
    }
    else if(distances(tempNodeGraph, v = as.character(tempPath[1]), to = as.character(tempPath[length(tempPath)])) < Inf)
    {
      #No breaking occurs on any edge where a path exists between the 2 vertices of the edge that does not contain the edges itself.
      breakFlag[nodeChecks] = FALSE
    }
    else if(any(degree(nodeGraph, as.character(c(tempPath[1], tempPath[length(tempPath)]))) == 1))
    {
      # No break if either vertex on edge has an order of 1
      breakFlag[nodeChecks] = FALSE
    }


    if(sum(breakFlag[nodeChecks]) > 1)
    {
      # No breaking occurs twice on one edge within 25 pixels (if this occurs, the higher y-Coordinate break is taken)
      pathIndex = which(tempPath %in% candidateNodes[breakFlag[nodeChecks]])

      i = 2
      while(TRUE)
      {
        if(i > length(pathIndex)){ break }

        if(pathIndex[i] < (pathIndex[i-1]+25))
        {
          if(((tempPath[pathIndex[i]] - 1)%%dims[1] + 1) > ((tempPath[pathIndex[i-1]] - 1)%%dims[1]))
          {
            breakFlag[which(candidateNodes == tempPath[pathIndex[i]])] = FALSE
            pathIndex = pathIndex[-i]
            i=i-1
          }
          else
          {
            breakFlag[which(candidateNodes == tempPath[pathIndex[i-1]])] = FALSE
            pathIndex = pathIndex[-(i-1)]
            i = i-1
          }
        }
        i = i+1
      }
    }

    if(any(which(tempPath %in% c(candidateNodes[nodeChecks])) <= 5 | which(tempPath %in% c(candidateNodes[nodeChecks])) >= length(tempPath) - 4))
    {
      #No breaking occurs within 5 pixels to a vertex <- This means I could delete the 11 thing.
      breakFlag[nodeChecks[which(candidateNodes[nodeChecks] <= 6 | candidateNodes[nodeChecks] >= length(tempPath) - 5)]] = FALSE
    }
  }

  return(breakFlag)
}

#' Internal function that uses existing breakPoint list to assign letters to the nodes in nodeGraph0.
letterPaths = function(allPaths, nodeGraph0, breakPoints)
{
  oldVerts = V(nodeGraph0)$name
  nodeGraph0 = delete_vertices(nodeGraph0, v = as.character(breakPoints))
  grIDs = rep(NA, length(V(nodeGraph0)))
  dists = distances(nodeGraph0, v = V(nodeGraph0), to = V(nodeGraph0))
  vertList = V(nodeGraph0)$name

  grPaths = list()
  i = 1
  while(length(vertList) > 0)
  {
    tempIDs = which(dists[which(V(nodeGraph0)$name == vertList[1]),] < Inf)
    grPaths = c(grPaths, list(as.numeric(V(nodeGraph0)$name[tempIDs])))
  #  grIDs[V(nodeGraph0)$name %in% as.character(grPaths[[i]])] = i
    grIDs[tempIDs] = i
    vertList = vertList[vertList %in% setdiff(vertList, grPaths[[i]])]
    i = i+1
  }
  grIDs2 = rep(NA, length(oldVerts))
  grIDs2[which(!(oldVerts %in% as.character(breakPoints)))] = grIDs

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

getNodes = function(img, dims)
{
  ## First, we find endpoints and intersections of skeleton.
  indices = img
  img = matrix(1, ncol = dims[2], nrow = dims[1])
  img[indices] = 0
  img.m = cbind(((indices-1) %% dims[1]) + 1, ((indices - 1) %/% dims[1]) + 1)
  changeCount = matrix(apply(X = img.m, MARGIN = 1, FUN = countChanges, img = img), byrow = F, nrow = 1)
  nodes = matrix(1, dims[1], dims[2])
  nodes[indices] = ifelse(changeCount == 1 | changeCount >= 3, 0, 1)

  ## If there is a 2x2 block in the thinned image and none of those pixels are nodes, make one of them a node.
  ## All will have connectivity of 2. Choose pixel with most neighbors as node.
  ## Later note: This isn't good enough. Consider path finding later.
  node2by2fill = function(coords, img)
  {
    rr = coords[1]
    cc = coords[2]
    
    if(img[rr,cc] == 0 & img[rr+1, cc] == 0 & img[rr,cc+1] == 0 & img[rr+1,cc+1] == 0)
    {
      index2by2 = matrix(c(rr,cc,rr+1, cc, rr,cc+1, rr+1,cc+1), byrow = TRUE, ncol = 2)
      numNeighbors = colSums(apply(X = index2by2, MARGIN = 1, FUN = whichNeighbors, img = img))
      newNode = index2by2[which.max(numNeighbors),]
      return(newNode[1] + (newNode[2] - 1)*dim(img)[1])
    }
    else
      return(NA)
  }
  
  nodes2by2 = apply(img.m, 1, FUN = node2by2fill, img = img)
  nodes[nodes2by2[!is.na(nodes2by2)]] = 0
  return(which(nodes == 0))
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
#' @import igraph
#'
#' @examples
#' data(csafe)
#' csafe = crop(csafe)
#' csafe_thin = thinImage(csafe)
#' csafe_nodes = getNodes(csafe_thin, dim(csafe))
#' csafe_processList = processHandwriting(csafe_thin, csafe_nodes, dim(csafe))
#' csafe_breaks = csafe_processList$breakPoints
#' csafe_paths = csafe_processList$pathList
#' csafe_letterss = csafe_processList$letterList
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
  graphdf0$from = as.character(graphdf0$from)
  graphdf0$to = as.character(graphdf0$to)
  graphdf0 = subset(graphdf0, select = c(from, to, nodeOnlyDist))

  graphdf$from = as.character(graphdf$from)
  graphdf$to = as.character(graphdf$to)
  graphdf = subset(graphdf, select = c(from, to, man_dist, euc_dist, pen_dist))

  skel_graph = graph_from_data_frame(d = graphdf, vertices = as.character(indices), directed = FALSE)
  skel_graph0 = graph_from_data_frame(d = graphdf0, vertices = as.character(indices), directed = FALSE)
  skel_graph = simplify(skel_graph, remove.multiple = TRUE, edge.attr.comb="mean")
  skel_graph0 = simplify(skel_graph0, remove.multiple = TRUE, edge.attr.comb="mean")

  V(skel_graph)$color = ifelse(V(skel_graph)$name %in% nodeList, 1, 0)
  V(skel_graph0)$color = ifelse(V(skel_graph0)$name %in% nodeList, 1, 0)

  dists0 = distances(skel_graph0, v = as.character(nodeList), to = as.character(nodeList), weights = E(skel_graph0)$nodeOnlyDist)
  adj0 = ifelse(dists0 == 1 | dists0 == 2, 1, 0)
  adj0[lower.tri(adj0)] = 0
  adj.m = melt(adj0)
  adj.m = subset(adj.m, value == 1)
  names(adj.m) = c("from", "to", "value")

  cat("Finding direct paths..")
  pathList = AllUniquePaths(adj.m, skel_graph, skel_graph0)
  cat("and loops...\n")
  loopList = getLoops(nodeList, skel_graph, skel_graph0, pathList, dim(img))

  allPaths = append(pathList, loopList)

  ####################### This is after path finding. Find breakpoints and check rules for removal.
  #Nominate and check candidate breakpoints
  cat("Looking for letter break points...")
  troughNodes = c()
  candidateNodes = c()
  for(i in 1:length(allPaths))
  {
    # Look for troughs in edges.
    hasTrough = FALSE
    tempPath = allPaths[[i]]
    if(length(tempPath) > 10)
    {
      rows = ((tempPath-1) %% dims[1]) + 1
      for(j in 6:(length(rows)-5))
      {
        if(any(rows[1:(j-1)] < rows[j]-1) & any(rows[(j+1):length(rows)] < rows[j]-1))
        {
          lowerEnd = max(which(rows[1:(j-1)] < rows[j]-1))
          upperEnd = min(which(rows[(j+1):length(rows)] < rows[j]-1))
          if(!any(rows[lowerEnd:(j+upperEnd)] > rows[j]))
          {
            troughNodes = c(troughNodes, tempPath[j])
            hasTrough = TRUE
          }
        }
      }
    }
    if(hasTrough == FALSE)
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
  
  goodBreaks = checkBreakPoints(candidateNodes = candidateNodes, allPaths = allPaths, nodeGraph = getNodeGraph(allPaths, nodeList), dims)
  preStackBreaks = candidateNodes[goodBreaks]
  
  ##################### Potential breakpoints (except for stacked letters) found. Break into letter paths.

  cat("Isolating letter paths...")

  lettersList = letterPaths(allPaths, skel_graph0, preStackBreaks)
  letters = lettersList[[1]]
  V(skel_graph0)$letterID = lettersList[[2]]
  
  finalBreaks = preStackBreaks[!(checkStacking(preStackBreaks, allPaths, letters, skel_graph0, dims))]
  
  breakAddedEndPoints = NULL
  pathsWithBreaks = lapply(allPaths, function(x){which(x %in% finalBreaks)})
  for(i in which(lapply(pathsWithBreaks, length) > 0))
  {
    newNodes = pathsWithBreaks[[i]]
    E(skel_graph0, P = as.character(allPaths[[i]][c(newNodes - 2, newNodes - 1)]))$nodeOnlyDist = 1
    E(skel_graph0, P = as.character(allPaths[[i]][c(newNodes + 1, newNodes + 2)]))$nodeOnlyDist = 1
    newNodes = c(newNodes - 1, newNodes + 1)
    breakAddedEndPoints = c(breakAddedEndPoints, allPaths[[i]][newNodes])
    nodeList = c(nodeList, allPaths[[i]][newNodes])
    allPaths[[i]] = list(allPaths[[i]], allPaths[[i]][1:(newNodes[1]-1)], allPaths[[i]][(newNodes[1]-1):length(allPaths[[i]])])
  }
  
  allPaths = lapply(rapply(allPaths, enquote, how="unlist"), eval)
  
  lettersList = letterPaths(allPaths, skel_graph0, finalBreaks)
  letters = lettersList[[1]]
  
  # for(i in seq(1, length(breakAddedEndPoints), 2))
  # {
  #   pairToJoin = which(sapply(letters, function(x) any(x %in% breakAddedEndPoints[c(i,i+1)])))
  #   letters[[pairToJoin[1]]] = list(letters[[pairToJoin[1]]], c(letters[[pairToJoin[1]]], letters[[pairToJoin[2]]]))
  # }
  # letters = lapply(rapply(letters, enquote, how="unlist"), eval)
  # 
  # for(i in 2:(length(letters)-1))
  # {
  #   if(all(letters[[i-1]] %in% letters[[i]]) & all(letters[[i+1]] %in% letters[[i]]))
  #     isMerged[i] = TRUE
  # }
  # letters = letters[unlist(lapply(letters, length)) > 5]
  # 
  
  isMerged = rep(FALSE, length(letters))
  nodesinGraph = replicate(length(letters), list(NA))
  for(i in 1:length(letters))
  {
    if(isMerged[i] == FALSE)
    {
      nodesinGraph[[i]] = letters[[i]][which(letters[[i]] %in% nodeList)]
    }
    else
    {
      nodesinGraph[[i]] = letters[[i]][which(letters[[i]] %in% nodeList[!(nodeList %in% breakAddedEndPoints)])]
    }
  }
  
  nodeOrder = orderNodes(letters = letters, nodesInGraph = nodesinGraph, dims = dims)
  letterAdj = list()
  decCode = rep(NA, length(letters))
  for(i in 1:length(letters))
  {
    letterDists = distances(graph = skel_graph0, v = as.character(nodesinGraph[[i]][order(nodeOrder[[i]])]), to = as.character(nodesinGraph[[i]][order(nodeOrder[[i]])]), weights = E(skel_graph0)$nodeOnlyDist)
    letterAdj[[i]] = (letterDists == 1 | letterDists == 2) + 0
    letterAdj[[i]][lower.tri(letterAdj[[i]])] = 0
    binCode = t(letterAdj[[i]])[!upper.tri(letterAdj[[i]])]
    decCode[i] = sum(binCode*2^((length(binCode):1) - 1))
  }
  
  lettersList = replicate(length(letters), list(path = NA, nodesInGraph = NA, nodeOrder = NA), simplify=FALSE)
  for(i in 1:length(letters))
  {
    lettersList[[i]]$path = letters[[i]]
    lettersList[[i]]$nodesInGraph = nodesinGraph[[i]]
    lettersList[[i]]$nodeOrder = nodeOrder[[i]]
    lettersList[[i]]$allPaths = pathLetterAssociate(allPaths,letters[[i]])
    lettersList[[i]]$adjMatrix = letterAdj[[i]]
    lettersList[[i]]$letterCode = decCode[i]
  }
  
  cat("and done.\n")
  return(list(thin = indices, nodes = nodeList, breakPoints = finalBreaks, pathList = allPaths, letterList = lettersList))
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

#' Internal function for removing breakpoints that follow all of the rules, but separate two letters that are
#' stacked on top of eachother. Currently, this is done in a very ad hoc, and untested manner. Will look for a better
#' solution in the future.
checkStacking = function(candidateBreaks, allPaths, letters, nodeGraph0, dims)
{
  #This is artificial, and I'm not a huge fan of it, but we'll let it be for now. May want to come back and do it better.
  stackPtFlag = rep(FALSE, length(candidateBreaks))

  for(i in 1:length(allPaths))
  {
    tempPath = allPaths[[i]]
    tempRow = ((tempPath - 1) %% dims[1]) + 1
    tempCol = ((tempPath - 1) %/% dims[1]) + 1
    nodeChecks = which(candidateBreaks %in% tempPath)
    if(length(nodeChecks) == 1)
    {
      if(abs((max(tempRow) - min(tempRow))/(max(tempCol) - min(tempCol))) > 2)
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
        else
        {
          # Call a break point a stack point if one of the letters is completely dominated
          # In the vertical sense. AKA if 1 is contained completely within the columns of another.

          gr1Cols = ((gr1 - 1) %/% dims[1]) + 1
          gr2Cols = ((gr2 - 1) %/% dims[1]) + 1
          rg1 = range(gr1Cols)
          rg2 = range(gr2Cols)
     #     cat("rg1:", rg1, "\nrg2:", rg2)
          #if(all(between(rg1, rg2[1], rg2[2])) | all(between(rg2, rg1[1], rg1[2])))
          if(all(rg1 > rg2[1] & rg1 < rg2[2]) | all(rg2 > rg1[1] & rg2 < rg1[2]))
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
  nodeGraph = add_vertices(nodeGraph, length(nodeList), name = as.character(nodeList))
  for(i in 1:length(allPaths))
  {
    nodeGraph = add.edges(nodeGraph, as.character(c(allPaths[[i]][1], allPaths[[i]][length(allPaths[[i]])])))
  }
  return(nodeGraph)
}

#' plotNodes
#'
#' This function returns a plot with the full image plotted in light gray and the skeleton printed in black, with red triangles over the vertices.
#' Also called from plotPath, which is a more useful function, in general.
#' @param img Full image matrix, unthinned.
#' @param thinned Thinned image matrix
#' @param nodeList Nodelist returned from getNodes.
#' @param nodeSize Size of triangles printed. 3 by default. Move down to 2 or 1 for small text images.
#' @return Plot of full and thinned image with vertices overlaid.
#' 
#' @import ggplot2
#' 
#' @examples
#' # See getNodes() examples first.
#' # plotNodes(london, london_thin, london_nodes)
#' # plotNodes(message, message_thin, message_nodes)
#'
#' @export

plotNodes = function(img, thinned, nodeList, nodeSize = 3, nodeColor = "red")
{
  p = plotImageThinned(img, thinned)
  pointSet = data.frame(X = ((nodeList - 1) %/% dim(img)[1]) + 1, Y = dim(img)[1] - ((nodeList - 1) %% dim(img)[1]))
  p = p + geom_point(data = pointSet, aes(X, Y), size = nodeSize, shape = I(16), color = I(nodeColor), alpha = I(.4))
  return(p)
  #l.m = melt(img)
  #l.m$value[thinned] = 2
  #l.m$value[nodeList] = 3
  #n.m2 = n.m[nodeList,]
  #p = ggplot(l.m, aes(Var2, rev(Var1))) + geom_raster(aes(fill = as.factor(value != 1), alpha = ifelse(value==0,.3,1))) + scale_alpha_continuous(guide = FALSE) + scale_fill_manual(values = c("white", "black"), guide = FALSE) + theme_void() + geom_point(data= n.m2, aes(x = Var2, y = dim(img)[1] - Var1 + 1), shape = I(17), size = I(nodeSize), color = I("red"))
}

#' Internal function for ordering the nodes in a letter.
orderNodes = function(letters, nodesInGraph, dims)
{
  toRC = function(nodes, dims)
  {
    cs = (nodes-1)%/%dims[1] + 1
    rs = (nodes-1)%%dims[1] + 1
    return(matrix(c(rs,cs), ncol = 2))
  }
  connectivity = function(letter, nodes, dims)
  {
    res = rep(NA, length(nodes))
    for(i in 1:length(nodes))
    {
      perimeter = c(nodes[i]-1, nodes[i]+dims[1]-1, nodes[i]+dims[1], nodes[i]+dims[1]+1, nodes[i]+1, nodes[i]-dims[1]+1, nodes[i]-dims[1], nodes[i]-dims[1]-1, nodes[i]-1) %in% letter
      res[i] = sum((perimeter[1:8] - perimeter[2:9]) == -1)
    }
    return(res)
  }
  angleDiff = function(fromIndex, toIndex, dims)
  {
    vecs = toRC(c(fromIndex, toIndex), dims)
    diff = c(vecs[1,1] - vecs[2,1], vecs[2,2] - vecs[1,2])
    return(atan2(diff[1], diff[2]))
  }
  getOrder = function(letter, nodesInGraph, dims)
  {
    if(length(nodesInGraph) == 0)
      return(nodesInGraph)
    else
    {
      nodeOrder = rep(NA, length(nodesInGraph))
      nodeConnectivity = connectivity(letter, nodesInGraph, dims)
      
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
            nodeCounter = nodeCounter + 1
          }
        }
        else
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
  
  res = rep(list(NA), length(letters))
  for(jj in 1:length(letters))
  {
    res[[jj]] = getOrder(letters[[jj]], nodesInGraph[[jj]], dims)
  }
  return(res)
}

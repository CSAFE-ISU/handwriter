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
  while(distances(graph0, v = fromNode, to = toNode, weights = E(graph0)$nodeOnlyDist) %in% c(1,2))
  {
    shortest = shortest_paths(graph0, from = fromNode, to = toNode, weights = E(graph0)$nodeOnlyDist)#pen_dist)
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

#' plotPath
#'
#' Returns a plot with a little red triangle on the pixels in the 'path' vector.
#' @param image Binary image.
#' @param image_thin Thinned binary image.
#' @param zoomBorder Pixel padding around the most extreme red triangles in every direction.
#' Doesn't check boundaries, so be smarter than the plot. Default is NA, which shows the whole image.
#' @param nodeSize Size of the red triangles. For fine images a smaller size may be necessary. Default is 3.
#' @return Returns a ggplot2 plot.
#' @examples
#' #### Not Run
#' # Make csafe_pathList the resulting object from the pathList item in the processHandwriting returned list
#'
#' # plotPath(csafe_pathList[[1]], csafe, csafe_thin, zoomBorder = NA)
#'
#' @export

plotPath = function(path, image, image_thin, zoomBorder = NA, nodeSize = 3)
{
  i1 = min(path %% dim(image)[1])
  i2 = max(path %% dim(image)[1])
  j1 = min(path %/% dim(image)[1])
  j2 = max(path %/% dim(image)[1])

  image_path = matrix(1, ncol = dim(image)[2], nrow = dim(image)[1])
  image_path[path] = 0

  if(!is.na(zoomBorder))
  {
    mincol = min(j1, j2)-zoomBorder
    maxcol = max(j1, j2)+zoomBorder
    minrow = min(i1, i2)-zoomBorder
    maxrow = max(i1, i2)+zoomBorder

    return(plotNodes(image[minrow:maxrow,][,mincol:maxcol], image_thin[minrow:maxrow,][,mincol:maxcol], image_path[minrow:maxrow,][,mincol:maxcol]))
  }
  else
    return(plotNodes(image, image_thin, image_path, nodeSize = nodeSize))
}

#' makeGifImages
#'
#' Saves a collection of plots, which sequentially display paths in a list, to the provided file path.
#' Also prints some suggested code for imagemagick gif creation. Can be useful for visualizing path lists or grapheme path lists.
#' @param image Binary image.
#' @param image_thin Thinned binary image.
#' @param allPaths List of paths (probably from processHandwriting returned list).
#' @param file_path Where the plots created should be saved to. Directory path, relative to current working directory.
#' @param filenames Identifying prefix for saved plots. Will be appended with numeric suffixes.
#' @return Nothing. Will print out a terminal command for creating your gif. Just navigate to the directory and copy and paste.
#' @examples
#' #### Not Run
#' # Make csafe_pathList the resulting object from the pathList item in the processHandwriting returned list
#'
#' # makeGifImages(csafe, csafe_thin, csafe_pathList, "../GifPlots/", "csafePaths")
#'
#' @export

makeGifImages = function(img, img_thin, allPaths, file_path, filenames)
{
  if(length(allPaths) < 1000)
  {
    digits = ceiling(log10(length(allPaths)))
    num = c(paste0("000", 1:9), paste0("00", 10:99), paste0("0", 100:999), as.character(1000:9999))

    for(i in 1:length(allPaths))
    {
      p = plotPath(allPaths[[i]], img, img_thin, zoomBorder = NA)
      ggsave(paste0(file_path, filenames, num[i], ".png"), p, device = "png")
    }
  }
  else
    stop("Too many paths. You don't want that many images. If you do, then write your own function.")
  print(paste0("convert -delay 30 -loop 0 ", filenames, "*.png Animate", filenames, ".gif"))
}

#' Internal function called by processHandwriting that eliminates breakpoints based on rules to try to coherently separate graphemes.
checkBreakPoints = function(candidateNodes, allPaths, nodeGraph, dims)
{
  #Check rules for candidate breakpoints
  breakFlag = rep(TRUE, length(candidateNodes))

  for(i in 1:length(allPaths))
  {
    tempPath = allPaths[[i]]
    nodeChecks = which(candidateNodes %in% tempPath)
    tempNodeGraph = delete.edges(nodeGraph, paste0(tempPath[1], "|", tempPath[length(tempPath)]))
    if(length(tempPath) < 11)
    {
      # If path length < 11, no break points. <- This is redundant with the within 5 pixels to vertex rule.
    #  breakFlag[nodeChecks] = FALSE
    }
    else if(tempPath[1] == tempPath[length(tempPath)])
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
      #No breaking occurs within 5 pixels to a vertex <- Think this means I can delete 11 thing.
      breakFlag[nodeChecks[which(candidateNodes[nodeChecks] <= 6 | candidateNodes[nodeChecks] >= length(tempPath) - 5)]] = FALSE
    }
  }

  return(breakFlag)
}

#' Internal function that uses existing breakPoint list to assign graphemes to the nodes in nodeGraph0.
graphemePaths = function(allPaths, nodeGraph0, breakPoints)
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
#' london_thin = thinImage(london, verbose = TRUE)
#' london_nodes = getNodes(london_thin)
#'
#' data(cells)
#' cells = crop(cells)
#' cells_thin = thinImage(cells, verbose = TRUE)
#' cells_nodes = getNodes(cells_thin)
#'
#' data(message)
#' message = crop(message)
#' message_thin = thinImage(message, verbose = TRUE)
#' message_nodes = getNodes(message_thin)
#'
#' @export

getNodes = function(img)
{
  ## First, we find endpoints and intersections of skeleton.
  indices = which(img == 0)
  img.m = cbind(((indices-1) %% dim(img)[1]) + 1, ((indices - 1) %/% dim(img)[1]) + 1)
  changeCount = matrix(apply(X = img.m, MARGIN = 1, FUN = countChanges, img = img), byrow = F, nrow = 1)
  nodes = matrix(1, dim(img)[1], dim(img)[2])
  nodes[indices] = ifelse(changeCount == 1 | changeCount >= 3, 0, 1)

  return(nodes)
}

#' processHandwriting
#'
#' Huge step in handwriting processing. Takes in thin image form and the breakpoints suggested by getNodes
#' and parses the writing into graphemes. Returns final grapheme separation points, a list of the paths in the image,
#' and a list of the grapheme paths in the image.
#'
#' @param img Thinned binary image.
#' @param nodes List of nodes from the getNodes function.
#' return(list(breakPoints = finalBreaks, pathList = allPaths, graphemeList = graphemes))
#' @return Returns a list of length 3. Object [[1]] (called breakPoints) is the set of final grapheme separation points.
#' Object [[2]] (called pathList) is a list of the paths between the input specified nodes.
#' Object [[3]] (called graphemes) is a list of the pixels in the different graphemes in the handwriting sample.
#'
#' @examples
#' data(csafe)
#' csafe = crop(csafe)
#' csafe_thin = thinImage(csafe, verbose = TRUE)
#' csafe_nodes = getNodes(csafe_thin)
#' csafe_processList = processHandwriting(csafe_thin, csafe_nodes)
#' csafe_breaks = csafe_processList$breakPoints
#' csafe_paths = csafe_processList$pathList
#' csafe_graphemes = csafe_processList$graphemeList
#'
#' @export

processHandwriting = function(img, nodes)
{
  # Next, we have to follow certain rules to find non intersection breakpoints.

  indices = which(img == 0)
  nodeList = which(nodes == 0)
  img.m = cbind(((indices-1) %% dim(img)[1]) + 1, ((indices - 1) %/% dim(img)[1]) + 1)

  neighborList = matrix(NA, nrow = indices, ncol = 8)
  neighborList = t(apply(as.matrix(img.m, ncol = 2), 1, whichNeighbors, img = img))
  graphdf = melt(neighborList)
  graphdf = subset(graphdf, value == 1)
  graphdf$from = indices[graphdf$Var1]
  graphdf$to = graphdf$from + c(-1, dim(img)[1]-1, dim(img)[1], dim(img)[1] + 1, 1, 1-dim(img)[1], -dim(img)[1], -1-dim(img)[1])[graphdf$Var2]
  graphdf$man_dist = rep(c(1,2), 4)[graphdf$Var2]
  graphdf$euc_dist = c(1,sqrt(2), 1, sqrt(2), 1, sqrt(2), 1, sqrt(2))[graphdf$Var2]
  graphdf$pen_dist = c(1,3,1,3,1,3,1,3)[graphdf$Var2]

  neighborList0 = matrix(NA, nrow = indices, ncol = 8)
  neighborList0 = t(apply(as.matrix(img.m, ncol = 2), 1, whichNeighbors0, img = img))
  graphdf0 = melt(neighborList0)
  graphdf0 = subset(graphdf0, value == 1)
  graphdf0$from = indices[graphdf0$Var1]
  graphdf0$to = graphdf0$from + c(-1, dim(img)[1]-1, dim(img)[1], dim(img)[1] + 1, 1, 1-dim(img)[1], -dim(img)[1], -1-dim(img)[1])[graphdf0$Var2]
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

  dists0 = distances(skel_graph0, v = as.character(nodeList), to = as.character(nodeList),weight = E(skel_graph0)$nodeOnlyDist)
  adj0 = ifelse(dists0 == 1 | dists0 == 2, 1, 0)
  adj0[lower.tri(adj0)] = 0
  adj.m = melt(adj0)
  adj.m = subset(adj.m, value == 1)
  names(adj.m) = c("from", "to", "value")

  pathList = AllUniquePaths(adj.m, skel_graph, skel_graph0)
  loopList = getLoops(nodeList, skel_graph, skel_graph0, pathList, dim(img))

  allPaths = append(pathList, loopList)

  ####################### This is after path finding. Find breakpoints and check rules for removal.
  #Nominate and check candidate breakpoints
  troughNodes = c()
  candidateNodes = c()
  for(i in 1:length(allPaths))
  {
    # Look for troughs in edges.
    hasTrough = FALSE
    tempPath = allPaths[[i]]
    if(length(tempPath) > 10)
    {
      rows = ((tempPath-1) %% dim(img)[1]) + 1
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
  breaks = which((((troughNodes[-1]-1) %% dim(img)[1]) + 1) != (((troughNodes[-length(troughNodes)] - 1) %% dim(img)[1]) + 1) |
                   ((((troughNodes[-1]-1) %/% dim(img)[1]) + 1) != (((troughNodes[-length(troughNodes)] - 1) %/% dim(img)[1])) &
                      (((troughNodes[-1]-1) %/% dim(img)[1])) != (((troughNodes[-length(troughNodes)] - 1) %/% dim(img)[1]) + 1)))
  breaks = c(1, breaks, length(troughNodes))
  candidateNodes = c(candidateNodes, troughNodes[ceiling((breaks[-1] + breaks[-length(breaks)])/2)])

  print(plotPath(candidateNodes, img, img, zoomBorder = NA))

  goodBreaks = checkBreakPoints(candidateNodes = candidateNodes, allPaths = allPaths, nodeGraph = getNodeGraph(allPaths, nodeList), dim(img))
  preStackBreaks = candidateNodes[goodBreaks]

  ##################### Potential breakpoints (except for stacked graphemes) found. Break into grapheme paths.

  graphemesList = graphemePaths(allpaths, skel_graph0, preStackBreaks)
  graphemes = graphemesList[[1]]
  V(skel_graph0)$graphemeID = graphemesList[[2]]

  finalBreaks = preStackBreaks[!(checkStacking(preStackBreaks, allPaths, graphemes, skel_graph0, dim(img)))]
  graphemesList = graphemePaths(allpaths, skel_graph0, finalBreaks)

  graphemesList = graphemePaths(allpaths, skel_graph0, finalBreaks)
  graphemes = graphemesList[[1]]

  return(list(breakPoints = finalBreaks, pathList = allPaths, graphemeList = graphemes))
}

#' Internal function for removing breakpoints that follow all of the rules, but separate two graphemes that are
#' stacked on top of eachother. Currently, this is done in a very ad hoc, and untested manner. Will look for a better
#' solution in the future.
checkStacking = function(candidateBreaks, allPaths, graphemes, nodeGraph0, dims)
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

        borderGraphemes = c(V(nodeGraph0)$graphemeID[which(V(nodeGraph0)$name == tempPath[pathIndex - 1])],
                            V(nodeGraph0)$graphemeID[which(V(nodeGraph0)$name == tempPath[pathIndex + 1])])
        gr1 = graphemes[[borderGraphemes[1]]]
        gr1Rows = ((gr1 - 1) %% dims[1]) + 1
        gr2 = graphemes[[borderGraphemes[2]]]
        gr2Rows = ((gr2 - 1) %% dims[1]) + 1

        # Call a break a stack point if the overlap between the bordering graphemes is
        # less than 10% of the total range of the combined graphemes.
        overlap = min(abs(max(gr1Rows) - min(gr2Rows)), abs(max(gr2Rows) - min(gr1Rows)))
        totalRange = (diff(range(c(gr1Rows,gr2Rows))))
        overlapPercentage = overlap/totalRange
        if(overlapPercentage < .1)
        {
          stackPtFlag[nodeChecks] = TRUE
        }
        else
        {
          # Call a break point a stack point if one of the graphemes is completely dominated
          # In the vertical sense. AKA if 1 is contained completely within the columns of another.

          gr1Cols = ((gr1 - 1) %/% dims[1]) + 1
          gr2Cols = ((gr2 - 1) %/% dims[1]) + 1
          rg1 = range(gr1Cols)
          rg2 = range(gr2Cols)
     #     cat("rg1:", rg1, "\nrg2:", rg2)
          if(all(between(rg1, rg2[1], rg2[2])) | all(between(rg2, rg1[1], rg1[2])))
            stackPtFlag[nodeChecks] = TRUE
        }
      }
    }
  }
  return(stackPtFlag)
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
#' @examples
#' # See getNodes() examples first.
#' plotNodes(london, london_thin, london_nodes)
#' plotNodes(cells, cells_thin, cells_nodes)
#' plotNodes(message, message_thin, message_nodes)
#'
#' @export

plotNodes = function(img, thinned, nodeList, nodeSize = 3)
{
  l.m = melt(img)
  t.m = melt(thinned)
  n.m = melt(nodeList)
  l.m$value[t.m$value == 0] = 2
  l.m$value[n.m$value == 0] = 3
  n.m2 = n.m[n.m$value == 0,]
  p = ggplot(l.m, aes(Var2, rev(Var1))) + geom_raster(aes(fill = as.factor(value != 1), alpha = ifelse(value==0,.3,1))) + scale_alpha_continuous(guide = FALSE) + scale_fill_manual(values = c("white", "black"), guide = FALSE) + theme_void() + geom_point(data= n.m2, aes(x = Var2, y = dim(thinned)[1] - Var1 + 1), shape = I(17), size = I(nodeSize), color = I("red"))
  return(p)
}

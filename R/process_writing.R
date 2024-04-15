# The handwriter R package performs writership analysis of handwritten documents. 
# Copyright (C) 2021 Iowa State University of Science and Technology on behalf of its Center for Statistics and Applications in Forensic Evidence
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


## Junction Detection ##
# Provide skeletonized image from ThinText.
# A black pixel becomes a node if its removal creates exactly one or at least
# three 4-connected black components in its 1-neighborhood.
# Also from Zhang thinning paper (allegedly)


# EXPORTED ----------------------------------------------------------------


#' Process Document
#'
#' Load a handwriting sample from a PNG image. Then binarize, thin, and split the handwriting into graphs.
#'
#' @param path File path for handwriting document. The document must be in PNG file format.
#'
#' @return The processed document as a list
#' 
#' @examples
#' image_path <- system.file("extdata", "phrase_example.png", package = "handwriter")
#' doc <- processDocument(image_path)
#' plotImage(doc)
#' plotImageThinned(doc)
#' plotNodes(doc)
#' 
#' @export
#' @md
processDocument <- function(path) {
  doc <- list()
  # load image as matrix
  doc$image <- readPNGBinary(path) 
  # load writing as 1-column matrix of index locations of black pixels
  doc$thin <- thinImage(doc$image)  
  doc$process <- processHandwriting(doc$thin, dim(doc$image))
  doc$docname <- basename(path)
  doc$docname <- stringr::str_replace(doc$docname, ".png", "")
  doc$docname <- stringr::str_replace(doc$docname, ".PNG", "")
  return(doc)
}

#' Process Handwriting by Component
#'
#' The main driver of handwriting processing. Takes in an image of thinned handwriting created
#' with [`thinImage()`] and splits the the handwriting into component shapes called *graphs*.
#'
#' @param img Thinned binary image created with [`thinImage()`].
#' @param dims Dimensions of thinned binary image.
#' @return A list of the processed image
#'
#' @useDynLib handwriter, .registration = TRUE
#'
#' @importFrom Rcpp sourceCpp
#' @importFrom reshape2 melt
#' @importFrom grDevices as.raster
#' @importFrom graphics hist
#' @importFrom stats na.omit
#' @importFrom utils install.packages
#' @import igraph
#'
#' @examples
#' twoSent_document <- list()
#' twoSent_document$image <- twoSent
#' twoSent_document$thin <- thinImage(twoSent_document$image)
#' twoSent_processList <- processHandwriting(twoSent_document$thin, dim(twoSent_document$image))
#'
#' @export
#' @md
processHandwriting <- function(img, dims) {
  value <- from <- to <- node_only_dist <- man_dist <- euc_dist <- pen_dist <- NULL
  
  # Starting Processing ----
  message("Starting Processing...")
  # convert thinned handwriting from list of pixel indices to binary matrix: 0 for
  # handwriting and 1 elsewhere
  indices <- img  # thinned image as list of pixel indices
  img <- matrix(1, nrow = dims[1], ncol = dims[2])
  img[indices] <- 0
  
  # Getting Nodes ----
  message("Getting Nodes...", appendLF = FALSE)
  # create nodes. Nodes are placed in 3 locations: (1) at endpoints. These are
  # called terminal nodes and only have one connected edge, (2) at intersections. 
  # A node is placed at vertices with 3 or more connected edges, and (3) a node
  # is placed at 2x2 blocks of pixels in the thinned writing
  nodes <- getNodes(indices, dims)
  nodeList <- nodes$nodeList  # all nodes
  terminalNodes <- nodes$terminalNodes
  nodeConnections <- nodes$nodeConnections
  
  # Skeletonize writer ----
  message("Skeletonizing writing...", appendLF = FALSE)
  skeleton <- skeletonize(img = img, indices = indices, dims = dims, nodeList = nodeList)
  
  # Split into components ----
  message("Splitting document into components...")
  comps <- getComponents(skeleton = skeleton, img = img, dims = dims, nodes = nodes)

  # And merging them ----
  message("and merging them...")
  comps <- mergeAllNodes(comps = comps)
  
  # Finding paths ----
  message("Finding paths...", appendLF = FALSE)
  n <- length(comps)
  for (i in 1:n){
    paths <- getAllUniquePaths(comps[[i]]$nodes$adjm, 
                               comps[[i]]$graphs$skeleton0, 
                               comps[[i]]$nodes$nodeList)
    if (is.null(paths)){
      comps[[i]]$paths$pathList <- list()
    } else {
      comps[[i]]$paths$pathList <- paths
    }
  }
  
  # And loops ----
  message("and loops...")
  
  for (i in 1:n){
    comps[[i]]$paths$loopList <- getLoops(nodeList = comps[[i]]$nodes$nodeList,
                                          skeleton = comps[[i]]$graphs$skeleton,
                                          skeleton0 = comps[[i]]$graphs$skeleton0,
                                          pathList = comps[[i]]$paths$pathList,
                                          dims = dims)
    comps[[i]]$paths$allPaths <- append(comps[[i]]$paths$pathList, comps[[i]]$paths$loopList)
  }
  
  # set node_only_dist values: 1 = edge is connected to a node; 0 = edge is not connected to a node
  for (i in 1:n){
    updated <- updateSkeleton0(graphdf0 = comps[[i]]$graphs$skeleton_df0, 
                               skeleton0 = comps[[i]]$graphs$skeleton0, 
                               nodeList = comps[[i]]$nodes$nodeList)
    comps[[i]]$graphs$skeleton_df0 <- updated$graphdf0
    comps[[i]]$graphs$skeleton0 <- updated$skeleton0
  }
  
  # Looking for graph break points ----
  # Nominate and check candidate breakpoints
  message("Looking for graph break points...", appendLF = FALSE)
  # Find candidate and trough nodes
  for (i in 1:n){
    if (length(comps[[i]]$paths$pathList) >= 1) {
      candidates <- getBreakPoints(pathList = comps[[i]]$paths$pathList, dims = dims)
      comps[[i]]$paths$hasTrough <- candidates$hasTrough
      comps[[i]]$nodes$breakPoints <- addTroughNodes(breakPoints = candidates$breakPoints,
                                                     troughNodes = candidates$troughNodes,
                                                     dims = dims)
    } else {
      comps[[i]]$nodes$breakPoints <- list()
      comps[[i]]$paths$hasTrough <- list()
    }
  }
  
  # And discarding bad ones ----
  message("and discarding bad ones...")
  for (i in 1:n){
    tempPaths <- comps[[i]]$paths$pathList
    if (length(tempPaths) > 0){
      comps[[i]]$nodes$breakPoints <- updateBreakPoints(pathList = tempPaths, 
                                                        nodeList = comps[[i]]$nodes$nodeList, 
                                                        breakPoints = comps[[i]]$nodes$breakPoints, 
                                                        terminalNodes = comps[[i]]$nodes$terminalNodes, 
                                                        allPaths = comps[[i]]$paths$allPaths,
                                                        dims = dims)
    } else {
      comps[[i]]$nodes$breakPoints <- list()
    }
  }
  
  # Isolating graph paths ----
  message("Isolating graph paths...")
  # assign sequential IDs to paths within each component
  for (i in 1:n){
    temp_graph <- comps[[i]]$graphs$skeleton0
    if (length(igraph::V(temp_graph)$name) > 0) {
      isolated <- isolateGraphs(allPaths = comps[[i]]$paths$allPaths,
                                skeleton0 = comps[[i]]$graphs$skeleton0,
                                breakPoints = comps[[i]]$nodes$breakPoints,
                                pathList = comps[[i]]$paths$pathList,
                                loopList = comps[[i]]$paths$loopList,
                                nodeList = comps[[i]]$nodes$nodeList,
                                terminalNodes = comps[[i]]$nodes$terminalNodes,
                                hasTrough = comps[[i]]$paths$hasTrough,
                                dims = dims)
      comps[[i]]$paths$allGraphs <- isolated$allGraphs
      comps[[i]]$graphs$skeleton0 <- isolated$skeleton0
      comps[[i]]$nodes$breakPoints <- isolated$breakPoints
      comps[[i]]$nodes$nodeList <- isolated$nodeList
      comps[[i]]$paths$allPaths <- NULL
    } else {
      # empty
      comps[[i]]$nodes$breakPoints <- numeric(0)
      # rename allPaths as allGraphs
      comps[[i]]$paths$allGraphs <- comps[[i]]$paths$allPaths
      comps[[i]]$paths$allPaths <- NULL
      # NOTE: comps[[i]]$graphs$skeleton0, comps[[i]]$nodes$nodeList
      # don't change from before this loop
    }
  }
  
  # assign sequential IDs to paths across all components
  graph_counter <- 0
  for (i in 1:n){
    if (length(igraph::V(comps[[i]]$graphs$skeleton0)$graphID) > 0){
      igraph::V(comps[[i]]$graphs$skeleton0)$graphID <- igraph::V(comps[[i]]$graphs$skeleton0)$graphID + graph_counter
      # vertices that are break points have graphID = NA
      graph_counter <- max(igraph::V(comps[[i]]$graphs$skeleton0)$graphID, na.rm = TRUE)
    }
  }
  
  # Organizing letters ----
  message("Organizing letters...")
  for (i in 1:n){
    temp_graph <- comps[[i]]$graphs$skeleton0
    if (length(igraph::V(temp_graph)$graphID) > 0){
      comps[[i]]$paths$letters <- organizeLetters(temp_graph)
    } else {
      comps[[i]]$paths$letters <- list()
    }
  }
  
  # Creating letter lists ----
  message("Creating letter lists...")
  for (i in 1:n){
    if (length(comps[[i]]$paths$letters) > 0){
      comps[[i]]$paths$graphList <- createLetterLists(allPaths = comps[[i]]$paths$allGraphs, 
                                                      letters = comps[[i]]$paths$letters, 
                                                      nodeList = comps[[i]]$nodes$nodeList, 
                                                      nodeConnections = comps[[i]]$nodes$nodeConnections, 
                                                      terminalNodes = comps[[i]]$nodes$terminalNodes, 
                                                      dims = dims)
    } else {
      comps[[i]]$paths$graphList <- list()
    }
  }
  
  # Adding character features ----
  message("Adding character features...")
  for (i in 1:n){
    if (length(comps[[i]]$paths$graphList) > 0){
      comps[[i]]$paths$graphList <- addCharacterFeatures(img = img, 
                                                         graphList = comps[[i]]$paths$graphList, 
                                                         letters = comps[[i]]$paths$letters, 
                                                         dims = dims)
    }
  }
  
  # Flatten ----
  nodeList <- unique(unlist(sapply(comps, function(x) x[['nodes']][['nodeList']])))
  nodeConnections <- unique(unlist(sapply(comps, function(x) x[['nodes']][['nodeConnections']])))
  terminalNodes <- unique(unlist(sapply(comps, function(x) x[['nodes']][['terminalNodes']])))
  breakPoints <- unique(unlist(sapply(comps, function(x) x[['nodes']][['breakPoints']])))
  graphList <- flatten_list(sapply(comps, function(x) x[['paths']][['graphList']]))
  
  # Document processing complete ----
  message("Document processing complete.\n")
  
  return(list(nodes = nodeList, connectingNodes = nodeConnections, terminalNodes = terminalNodes, breakPoints = sort(breakPoints), letterList = graphList))
}


# Clean -------------------------------------------------------------------

getComponents <- function(skeleton, img, dims, nodes) {
  # split skeleton into connected components
  skeletons <- igraph::decompose(skeleton)
  
  initializeComponents <- function(skeletons){
    # create list of empty components
    n <- length(skeletons)
    comps <- list()
    for (i in 1:n){
      component <- sapply(c('graphs', 'image', 'nodes', 'paths'), function(x) NULL)
      comps[[i]] <- component
    }
    return(comps)
  }
  
  addSkeletons <- function(skeletons, comps) {
    # add skeleton to each component
    n <- length(comps)
    for (i in 1:n){
      comps[[i]]$graphs$skeleton <- skeletons[[i]]
    }
    return(comps)
  }
  
  addIndices <- function(skeletons, comps, dims) {
    # add indices to each component
    n <- length(comps)
    for (i in 1:n){
      # get vertex names
      comps[[i]]$image$indices <- as.numeric(igraph::V(skeletons[[i]])$name)
      # get rows and columns of vertices
      comps[[i]]$image$img_m <- i_to_rc(comps[[i]]$image$indices, dims)
    }  
    return(comps)
  }
  
  addNodes <- function(comps, nodes) {
    # add nodes to each component
    n <- length(comps)
    for (i in 1:n){
      # nodes
      comps[[i]]$nodes$nodeList <- intersect(comps[[i]]$image$indices, nodes$nodeList)
      comps[[i]]$nodes$terminalNodes <- intersect(comps[[i]]$nodes$nodeList, nodes$terminalNodes)
      comps[[i]]$nodes$nodeConnections <- intersect(comps[[i]]$nodes$nodeList, nodes$nodeConnections)
    }
    return(comps)
  }
  
  addSkeleton0s <- function(comps, img, dims) {
    # add (1) skeleton_df0 and (2) skeleton0 to each component
    n <- length(comps)
    for (i in 1:n){
      # same as skeleton_df except neighbors on the diagonal are removed if there
      # are neighbors on either side of the diagonal
      comps[[i]]$graphs$skeleton_df0 <- getSkeletonDF0(img_m=comps[[i]]$image$img_m, 
                                                       indices=comps[[i]]$image$indices, 
                                                       nodeList=comps[[i]]$nodes$nodeList, 
                                                       img=img, 
                                                       dims=dims)
      comps[[i]]$graphs$skeleton0 <- getSkeleton(skeleton_df=comps[[i]]$graphs$skeleton_df0, 
                                                 indices=comps[[i]]$image$indices, 
                                                 nodeList=comps[[i]]$nodes$nodeList)
    }
    return(comps)
  }
  
  getAdjMatrix <- function(skeleton0, nodeList) {
    # Weight each edge in skeleton0 with node_only_dist (1=neighbor of node, 0 otherwise).
    # Then find the shortest distance from each node to each other node.
    dists0 <- igraph::distances(skeleton0, 
                                v = as.character(format(nodeList, scientific = FALSE, trim = TRUE)), 
                                to = as.character(format(nodeList, scientific = FALSE, trim = TRUE)), 
                                weights = igraph::E(skeleton0)$node_only_dist)
    # Create adjacency matrix with 1 if the distance is 1 or 2 and 0 otherwise
    adj0 <- ifelse(dists0 == 1 | dists0 == 2, 1, 0)
    
    return(adj0)
  }
  
  addAdjMatrices <- function(comps) {
    # add node only distance adjacency matrix to each component
    n <- length(comps)
    for (i in 1:n){
      # get adjacency matrix: (1) weight each edge in skeleton0 with 1 if either
      # vertex is a node, this is the node_only_dist (2) find the shortest distance
      # between each pair of nodes (3) make an adjacency matrix for each pair of
      # nodes where 1 means the the shortest path between them is node_only_dist 1 or
      # 2, where node_only_dist 1 occurs when the nodes are joined by a single edge
      # and 2 occurs when the nodes are joined by more than one edge but do not have
      # another node on the path between them.
      comps[[i]]$nodes$adj0 <- getAdjMatrix(comps[[i]]$graphs$skeleton0, comps[[i]]$nodes$nodeList)
    }
    return(comps)
  }
  
  comps <- initializeComponents(skeletons = skeletons)
  comps <- addSkeletons(skeletons = skeletons, comps = comps)
  comps <- addIndices(skeletons = skeletons, comps = comps, dims = dims)
  comps <- addNodes(comps = comps, nodes = nodes)
  comps <- addSkeleton0s(comps = comps, img = img, dims = dims)
  comps <- addAdjMatrices(comps = comps)
  
  return(comps)
}

mergeAllNodes <- function(comps) {
  findMergeNodes <- function(skeleton, mergeMat) {
    newNodes <- rep(NA, dim(mergeMat)[1])
    for (i in 1:dim(mergeMat)[1])
    {
      fromNode <- as.character(format(mergeMat[i, 1], scientific = FALSE, trim = TRUE))
      toNode <- as.character(format(mergeMat[i, 2], scientific = FALSE, trim = TRUE))
      path <- igraph::shortest_paths(skeleton, from = fromNode, to = toNode, weights = igraph::E(skeleton)$pen_dist)$vpath[[1]]
      len <- length(path)
      newNodes[i] <- as.numeric(names(path[ceiling(len / 2)]))
    }
    return(newNodes)
  }
  
  migrateConnections <- function(adj0, mergeSets, newNodes) {
    toDelete <- NULL
    for (i in 1:dim(mergeSets)[1]) {
      whichRowCol <- which(colnames(adj0) %in% format(mergeSets[i, c(1, 2)], scientific = FALSE, trim = TRUE))
      newConnectivities <- apply(matrix(adj0[whichRowCol, ], nrow = length(whichRowCol)), 2, function(x) x[1] == 1 | x[2] == 1)
      newConnectivities[is.na(newConnectivities)] <- 0
      
      toAdd <- dim(adj0)[1] + 1
      toDelete <- c(toDelete, which(rownames(adj0) %in% format(mergeSets[i, c(1, 2)], scientific = FALSE, trim = TRUE)))
      
      adj0 <- rbind(cbind(adj0, 0), 0)
      adj0[, toAdd] <- c(newConnectivities, 0)
      adj0[toAdd, ] <- c(newConnectivities, 0)
      colnames(adj0)[toAdd] <- format(newNodes[i], scientific = FALSE, trim = TRUE)
      rownames(adj0)[toAdd] <- format(newNodes[i], scientific = FALSE, trim = TRUE)
    }
    if (length(toDelete) > 0) {
      adj0 <- as.matrix(adj0[, -toDelete])[-toDelete, ]
    }
    return(adj0)
  }
  
  mergeNodes <- function(nodeList, skeleton0, terminalNodes, skeleton, adj0) {
    # bind global variable to fix check() note
    value <- NULL
    
    emergencyBreak <- 100
    while (TRUE) {
      # count smallest number of edges between each pair of nodes (each edge weighted as 1?)
      distsFull <- igraph::distances(skeleton0, 
                                     v = as.character(format(nodeList, scientific = FALSE, trim = TRUE)), 
                                     to = as.character(format(nodeList, scientific = FALSE, trim = TRUE)), 
                                     weights = NA)
      # set all values on and below the diagonal to 0
      distsFull[!upper.tri(distsFull)] <- 0
      
      # flag nodes that are only 1 or 2 edges apart
      nodesToMerge <- which(distsFull <= 2 & distsFull > 0)
      
      if (length(nodesToMerge)==0) { 
        break 
      }
      
      rNodes <- i_to_r(nodesToMerge, length(nodeList))
      cNodes <- i_to_c(nodesToMerge, length(nodeList)) 
      mergeSets <- cbind(nodeList[rNodes], nodeList[cNodes])
      # add column: 1=neither node is terminal, 0 otherwise
      mergeSets <- cbind(mergeSets, apply(mergeSets, 1, function(x) {
        all(!(x %in% terminalNodes))
      }))
      # keep rows of nodes where neither node is terminal
      mergeSets <- mergeSets[mergeSets[, 3] == 1, c(1, 2)]
      mergeSets <- matrix(mergeSets, ncol = 2)
      
      # end while loop if no nodes need to be merged
      if (dim(mergeSets)[1] == 0) { 
        break 
      }
      
      newNodes <- findMergeNodes(skeleton, mergeSets)
      # combine nodes that were not in the mergeSets and the list of new nodes
      nodeList <- unique(c(nodeList[!(nodeList %in% c(mergeSets[, c(1, 2)]))], newNodes))
      
      # migrate connections from original nodes to new nodes
      adj0 <- migrateConnections(adj0 = adj0, mergeSets = mergeSets, newNodes = newNodes)
      
      emergencyBreak <- emergencyBreak - 1
      if (emergencyBreak == 0) {
        break
      }
    }
    
    # Set the diagonal and below of the adjacency matrix to 0
    adj0[lower.tri(adj0)] <- 0
    adj.m <- melt(adj0)
    adj.m <- subset(adj.m, value == 1)
    if (nrow(adj.m) > 0){
      names(adj.m) <- c("from", "to", "value")
    }
    
    return(list('nodeList'=nodeList, 'adjm'=adj.m))
  }
  
  n <- length(comps)
  for (i in 1:n){
    merged <- mergeNodes(nodeList = comps[[i]]$nodes$nodeList, 
                         skeleton0 = comps[[i]]$graphs$skeleton0, 
                         terminalNodes = comps[[i]]$nodes$terminalNodes, 
                         skeleton = comps[[i]]$graphs$skeleton, 
                         adj0 = comps[[i]]$nodes$adj0)
    comps[[i]]$nodes$nodeList <- merged$nodeList
    comps[[i]]$nodes$adjm <- merged$adjm
  }
  return(comps)
}


skeletonize <- function(img, indices, dims, nodeList) {
  # create skeleton graphs and dataframe. skeleton is created first using the
  # following rules: (1) Every pixel in the thinned writing is a vertex. (2) An
  # edge is added between two vertices if the corresponding pixels are neighbors
  # in the writing. (3) if a vertex is a node it is colored 1, otherwise it is
  # colored 0. skeleton0 is created next using the following rules (I think?):
  # (1) Every pixel in the thinned writing is a vertex. (2) An edge is added
  # between two vertices if the corresponding pixels are neighbors in the
  # writing. (3) If a vertex has a neighboring vertex on the diagonal AND
  # neighboring vertices on BOTH sides of the diagaonl, the edge between the
  # vertex and the diagonal neighbor is removed (this needs to be
  # double-checked) (4) if a vertex is a node it is colored 1, otherwise it is
  # colored 0
  
  getSkeletonDF <- function(img, indices, dims) {
    # build graph from thinned writing. Each pixel in the thinned writing is a
    # vertex, if two vertices are "neighbors" they are connected by an edge in
    # skeleton_df.
    
    # bind global variable to fix check() note
    value <- from <- to <- man_dist <- euc_dist <- pen_dist <- NULL
    
    img_m <- i_to_rc(indices, dims)
    
    # DEFINITION (neighborhood): for a given pixel in the thinned writing, its
    # neighborhood is the set of 8 pixels surrounding it in the thinned image: 
    # {top, top right, right, bottom right, bottom, bottom left, left, top left}.
    #
    # CONVENTION: by convention, the functions in this script list the pixels in the neighborhood
    # starting with the top pixel and moving clockwise
    #
    # DEFINITION (neighbor): for a given pixel p in the thinned writing, another pixel in the thinned
    # image is p's neighbor if it is in the neighborhood of p and it is also in the thinned writing. 
    #
    # NOTE: It need not be the case that every pixel in a neighborhood contains a neighbor. Think of a 
    # house that is surround by 8 lots and some of the lots contain other houses but the other lots 
    # are forest.
    #
    # make matrix of neighborhoods for each pixel in the thinned writing: 
    #   each row corresponds to a pixel in the thinned writing
    #   each column corresponds to a location in the pixel's neighborhood
    #   1=the neighborhood location contains a neighbor, 0=neighborhood location does not contain a neighbor
    neighborList <- t(apply(as.matrix(img_m, ncol = 2), 1, findNeighbors, img = img))
    # build skeleton_df = a dataframe of edges in the thinned writing. Each pixel in the thinned writing
    # is a vertex, if two vertices are "neighbors" they are connected by an edge in skeleton_df.
    # melt converts list to dataframe where 
    #   Var1 = pixel number (1, 2,..., total num. pixels in thinned writing), 
    #   Var2 = neighborhood location index (in 1, 2,...,8), 
    #   1=the neighborhood location contains a neighbor, 0=neighborhood location does not contain a neighbor
    skeleton_df <- melt(neighborList)
    # filter for neighbors
    skeleton_df <- subset(skeleton_df, value == 1)
    # get index in thinned image of each pixel
    skeleton_df$from <- indices[skeleton_df$Var1]
    # get the index in thinned image of the neighbor pixel for each pixel
    skeleton_df$to <- skeleton_df$from + c(-1, dims[1] - 1, dims[1], dims[1] + 1, 1, 1 - dims[1], -dims[1], -1 - dims[1])[skeleton_df$Var2]
    # Manhattan distance between each pixel and its neighbor. neighbors to the
    # top, right, bottom, and left are 1 in distance and neighbors on the
    # diagonals are 2 in distance.
    skeleton_df$man_dist <- rep(c(1, 2), 4)[skeleton_df$Var2]
    # Euclidean distance between each pixel and its neighbor
    skeleton_df$euc_dist <- c(1, sqrt(2), 1, sqrt(2), 1, sqrt(2), 1, sqrt(2))[skeleton_df$Var2]
    skeleton_df$pen_dist <- c(1, 3, 1, 3, 1, 3, 1, 3)[skeleton_df$Var2]
    
    # format from and to columns as character in skeleton_df
    skeleton_df$from <- as.character(format(skeleton_df$from, scientific = FALSE, trim = TRUE))
    skeleton_df$to <- as.character(format(skeleton_df$to, scientific = FALSE, trim = TRUE))
    # drop Var1, Var2, and value columns
    skeleton_df <- subset(skeleton_df, select = c(from, to, man_dist, euc_dist, pen_dist))
    
    return(skeleton_df)
  }

  skeleton_df <- getSkeletonDF(img=img, indices=indices, dims=dims)
  # create igraphs from skeleton_df. If two vertices are joined by more than one edge, merge
  # the edges by averaging their attributes. Color the nodes 1 and all other vertices 0.
  skeleton <- getSkeleton(skeleton_df=skeleton_df, indices=indices, nodeList=nodeList)
  
  return(skeleton)
}


# Internal Functions ------------------------------------------------------
#' add_character_features
#'
#' Internal method that adds features to characters
#'
#' @param img thinned binary image
#' @param graphList list containing letter characters
#' @param letters individual characters from graphList
#' @param dims image graph dimensions
#' @return a list of letters with features applied
#' @noRd
addCharacterFeatures <- function(img, graphList, letters, dims) {
  featureSets <- extract_character_features(img, graphList, dims)
  
  for (i in 1:length(letters))
  {
    graphList[[i]]$characterFeatures <- featureSets[[i]]
  }
  
  letterPlaces <- matrix(unlist(lapply(featureSets, FUN = function(x) {
    c(x$line_number, x$order_within_line)
  })), ncol = 2, byrow = TRUE)
  letterOrder <- order(letterPlaces[, 1], letterPlaces[, 2])
  graphList <- graphList[letterOrder]
  
  return(graphList)
}

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

#' getAllUniquePaths
#'
#' Internal function for getting a list of all non loop paths in a writing sample.
#'
#' @param adj adjacent matrix of nodes
#' @param graph0 skeletonized graph
#' @return a list of all non loop paths
#' @noRd
getAllUniquePaths <- function(adj, graph0, nodeList) {
  paths <- list()
  if (dim(adj)[1] == 0) {
    return(NULL)
  }
  
  # update graphdf0 with node_only_dist=1 if one or more of the edge's vertices is a node and 0.00001 otherwise
  graphdf0 <- igraph::as_data_frame(graph0)
  graphdf0$node_only_dist <- ifelse(graphdf0$from %in% nodeList | graphdf0$to %in% nodeList, 1, 0.00001)
  graph0 <- igraph::graph_from_data_frame(graphdf0, directed = FALSE)
  
  for (i in 1:dim(adj)[1])
  {
    fromNode <- as.character(format(adj[i, 1], scientific = FALSE, trim = TRUE))
    toNode <- as.character(format(adj[i, 2], scientific = FALSE, trim = TRUE))
    
    # calculate distances of shortest path between fromNode and toNode
    dists <- igraph::distances(graph0, v = fromNode, to = toNode, weights = igraph::E(graph0)$node_only_dist)
    # for paths from node to node that don't go through another node?
    while (dists < 3 & dists >= 1) {
      # CALCULATE STEP:
      # get shortest path between fromNode and toNode
      shortest <- igraph::shortest_paths(graph0, from = fromNode, to = toNode, weights = igraph::E(graph0)$node_only_dist)
      # count vertices in shortest path, including fromNode and toNode
      len <- length(unlist(shortest[[1]]))
      # add the shortest path to the list
      paths <- c(paths, list(as.numeric(names(shortest$vpath[[1]]))))
      
      # UPDATE STEP: If there are more than two vertices in the path, delete the
      # middle edge. If there are 2 vertices in the path, delete the single edge
      # between the vertices. 
      if (len > 2) {
        graph0 <- igraph::delete_edges(graph0, paste0(names(shortest$vpath[[1]])[len %/% 2], "|", names(shortest$vpath[[1]])[len %/% 2 + 1]))
      } else if (len == 2) {
        graph0 <- igraph::delete_edges(graph0, paste0(names(shortest$vpath[[1]])[1], "|", names(shortest$vpath[[1]])[2]))
      } else {
        stop("There must be some mistake. Single node should have node_only_dist path length of 0.")
      }
      # recalculate shortest path distance on updated graph
      dists <- igraph::distances(graph0, v = fromNode, to = toNode, weights = igraph::E(graph0)$node_only_dist)
    }
  }
  
  return(paths)
}

#' checkBreakPoints
#'
#' Internal function called by processHandwriting that eliminates breakpoints
#' based on rules to try to coherently separate letters.
#'
#' @param breakPoints possible breakpoints
#' @param allPaths list of paths
#' @param nodeGraph graph of nodes; call the getNodeGraph function
#' @param terminalNodes nodes at the endpoints of the graph
#' @param dims graph dimensions
#'
#' @return a graph without breakpoints and separated letters
#' @noRd
checkBreakPoints <- function(breakPoints, allPaths, nodeGraph, terminalNodes, dims) {
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

#' checkSimplicityBreaks
#'
#' Internal function for removing breakpoints that separate graphs that are too simple to be split. Remove break if graph on left and right of the break have 4 or fewer nodes and no loops or double paths. Never remove break on a trough.
#'
#' @param breakPoints possible breakpoints
#' @param pathList list of paths
#' @param loopList list of loops
#' @param letters list of individual letter characters
#' @param skeleton0 skeletonized graph
#' @param nodeList list of nodes
#' @param terminalNodes nodes at the ends of letters
#' @param hasTrough whether or not break has a trough
#' @param dims graph dimensions
#' @return removes breakpoints on simple graphs
#' @noRd
checkSimplicityBreaks <- function(breakPoints, pathList, loopList, letters, skeleton0, nodeList, terminalNodes, hasTrough, dims) {
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

#' checkStacking
#'
#' Internal function for removing breakpoints that follow all of the rules, but separate two letters that are stacked on top of each other.
#'
#' @param breakPoints possible breaks for letterpath
#' @param allPaths list of paths
#' @param letters list of individual letter characters
#' @param skeleton0 skeletonized graph
#' @param dims graph dimensions
#' @return stackPtFlag
#' @noRd
checkStacking <- function(breakPoints, allPaths, letters, skeleton0, dims) {
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


createLetterLists <- function(allPaths, letters, nodeList, nodeConnections, terminalNodes, dims) {
  # Assign nodes to each letter
  nodesinGraph <- replicate(length(letters), list(NA))
  connectingNodesinGraph <- replicate(length(letters), list(NA))
  terminalNodesinGraph <- replicate(length(letters), list(NA))
  
  for (i in 1:length(letters))
  {
    nodesinGraph[[i]] <- letters[[i]][which(letters[[i]] %in% nodeList)]
    connectingNodesinGraph[[i]] <- letters[[i]][which(letters[[i]] %in% nodeConnections)]
    terminalNodesinGraph[[i]] <- letters[[i]][which(letters[[i]] %in% terminalNodes)]
  }
  
  
  graphList <- replicate(length(letters), list(path = NA, nodes = NA), simplify = FALSE)
  for (i in 1:length(letters))
  {
    graphList[[i]]$path <- letters[[i]]
    # graphList[[i]]$nodes = nodesinGraph[[i]][nodeOrder[[i]]]
    graphList[[i]]$allPaths <- pathLetterAssociate(allPaths, letters[[i]])
  }
  
  letterAdj <- list()
  nodeOrder <- replicate(list(), n = length(letters))
  decCode <- rep("", length(letters))
  connectivityScores <- replicate(list(), n = length(letters))
  
  getConnectivity <- function(pathEndings, nodesSingle) {
    res <- rep(NA, length(nodesSingle))
    for (j in 1:length(nodesSingle))
    {
      res[j] <- sum(pathEndings == nodesSingle[j])
    }
    return(res)
  }
  
  for (i in 1:length(letters))
  {
    if (length(nodesinGraph[[i]]) > 0) {
      graphList[[i]]$adjMatrix <- matrix(0, ncol = length(nodesinGraph[[i]]), nrow = length(nodesinGraph[[i]]))
      
      pathStarts <- unlist(lapply(graphList[[i]]$allPaths, function(x) x[1]))
      pathEnds <- unlist(lapply(graphList[[i]]$allPaths, function(x) x[length(x)]))
      
      connectivityScores[[i]] <- getConnectivity(pathEndings = c(pathStarts, pathEnds), nodesSingle = nodesinGraph[[i]])
      
      nodeOrder[[i]] <- getNodeOrder(letters[[i]], nodesinGraph[[i]], connectivityScores[[i]], dims)
      
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

# convert a list of lists into a single 1-level list.
# Example: list(list('a', 'b', 'c'), list('d'), list('e', 'f')) 
# becomes list('a', 'b', 'c', 'd', 'e', 'f')
flatten_list <- function(actual) {
  new <- list()
  counter <- 1
  for (i in 1:length(actual)){
    current_list <- actual[[i]]
    if (length(current_list) >= 1){
      for (j in 1:length(current_list)){
        current_path <- current_list[[j]]
        if (length(current_path) >= 1){
          new[[counter]] <- current_path
          counter = counter + 1
        }
      }
    }
  }
  return(new)
}

getBreakPoints <- function(pathList, dims) {
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


getSkeletonDF0 <- function(img_m, img, indices, dims, nodeList) {
  # bind global variable to fix check() note
  value <- from <- to <- node_only_dist <- NULL
  
  # Step 1: make matrix of neighborhoods for each pixel in the thinned writing: 
  #   each row corresponds to a pixel in the thinned writing
  #   each column corresponds to a location in the pixel's neighborhood
  #   1=the neighborhood location contains a neighbor, 0=neighborhood location does not contain a neighbor
  # Step 2: remove neighbors on the diagonal if there are neighbors in the neighborhood on either side of the diagonal.
  # Example, if there is a neighbor in the top right pixel, and there are also neighbors in both the top and the right pixels.
  # Remove the neighbor in the top right pixel.
  neighborList0 <- t(apply(as.matrix(img_m, ncol = 2), 1, findNeighbors0, img = img))
  # converts to dataframe where 
  #   Var1 = pixel number (1, 2,..., total num. pixels in thinned writing), 
  #   Var2 = neighborhood location index (in 1, 2,...,8), 
  #   1=the neighborhood location contains a neighbor, 0=neighborhood location does not contain a neighbor
  graphdf0 <- melt(neighborList0)
  # filter for neighbors
  graphdf0 <- subset(graphdf0, value == 1)
  # get the index in thinned image of each pixel
  graphdf0$from <- indices[graphdf0$Var1]
  # get the index in thinned image of the neighbor pixel for each pixel
  graphdf0$to <- graphdf0$from + c(-1, dims[1] - 1, dims[1], dims[1] + 1, 1, 1 - dims[1], -dims[1], -1 - dims[1])[graphdf0$Var2]
  # node only distance = 1 if pixel is a node or its neighbor is a node, 0 otherwise
  graphdf0$node_only_dist <- ifelse(graphdf0$from %in% nodeList | graphdf0$to %in% nodeList, 1, 0)
  
  # format from and to columns as character
  graphdf0$from <- as.character(format(graphdf0$from, scientific = FALSE, trim = TRUE))  # trim=TRUE suppresses leading blanks for justification of numbers
  graphdf0$to <- as.character(format(graphdf0$to, scientific = FALSE, trim = TRUE))
  # drop Var1, Var2, and value columns
  graphdf0 <- subset(graphdf0, select = c(from, to, node_only_dist))
  
  return(graphdf0)
}

#' Get Loops
#'
#' Internal function for getting looped paths.
#'
#' @param nodeList A list of all found nodes
#' @param skeleton first skeletonized graph
#' @param skeleton0 second skeletonized graph
#' @param pathList The current path list to check for loops
#' @param dims dimensions of the image
#' @return A list of all loops found
#'
#' @importFrom utils combn
#' @noRd
getLoops <- function(nodeList, skeleton, skeleton0, pathList, dims) {
  vertexNames <- names(igraph::V(skeleton0))
  
  fullSkeleton0 <- skeleton0
  
  used <- unlist(lapply(pathList, function(x) {
    x[-c(1, length(x))]
  }))
  unused <- as.numeric(vertexNames)[which(!(as.numeric(vertexNames) %in% used))]
  unusedAdj <- matrix(1, ncol = length(unused), nrow = length(unused))
  colnames(unusedAdj) <- as.character(format(unused, scientific = FALSE, trim = TRUE))
  rownames(unusedAdj) <- as.character(format(unused, scientific = FALSE, trim = TRUE))
  if (length(nodeList) > 1) {
    unusedAdj[, which(unused %in% nodeList)][which(unused %in% nodeList), ] <- 0
  } else {
    unusedAdj[which(unused %in% nodeList), which(unused %in% nodeList)] <- 0
  }
  unusedSkeleton <- igraph::graph_from_adjacency_matrix(unusedAdj, mode = "undirected")
  
  skeleton0 <- intersection(skeleton0, unusedSkeleton, keep.all.vertices = TRUE)
  skeleton <- intersection(skeleton, skeleton0, byname = TRUE, keep.all.vertices = TRUE)
  check <- unused[degree(skeleton0, as.character(format(unused, scientific = FALSE, trim = TRUE))) > 1]
  check <- check[which(check %in% nodeList)]
  
  loopList <- list()
  
  tryCatch(
    expr = {
      neighbors <- igraph::neighborhood(skeleton, nodes = as.character(check))
      
      if (any(unlist(lapply(neighbors, length)) > 3)) {
        warning("At least 1 of the nodes in the potential loops has more than 2 neighbors after removal of the connections. Try again! \nThe nodes in question are: \n", dput(names(neighbors)[which(unlist(lapply(neighbors, length)) > 3)]))
      }
      
      ## Get paths that start and end at the same point, where that point is a node in nodeList
      if (length(neighbors) > 0) {
        for (i in 1:length(neighbors)) {
          neigh <- as.numeric(names(neighbors[[i]]))
          skeleton <- igraph::delete_edges(skeleton, paste0(neigh[1], "|", neigh[2]))
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
  
  ## Eliminate loop paths that we have found and find ones that dont have vertex on the loop. This is caused by combining of nodes that are close together.
  used <- as.numeric(unique(c(unlist(pathList), unlist(loopList))))
  unused <- as.numeric(vertexNames)[which(!(as.numeric(vertexNames) %in% used))]
  remaining0 <- igraph::induced_subgraph(skeleton0, vids = format(c(unused, nodeList), scientific = FALSE, trim = TRUE))
  numNeighbors <- lapply(igraph::neighborhood(remaining0, nodes = igraph::V(remaining0)), length)
  remaining0 <- igraph::induced_subgraph(remaining0, vids = igraph::V(remaining0)[numNeighbors > 1])
  
  roots <- format(nodeList[which(nodeList %in% names(igraph::V(remaining0)))], scientific = FALSE, trim = TRUE)
  
  if (length(roots) > 0) {
    for (i in 1:length(roots))
    {
      loopPart1 <- names(na.omit(igraph::dfs(remaining0, roots[i], unreachable = FALSE)$order))
      loopPart2 <- igraph::shortest_paths(fullSkeleton0, from = loopPart1[length(loopPart1)], to = roots[i])$vpath[[1]][-1]
      loopList <- append(loopList, list(as.numeric(c(loopPart1, names(loopPart2)))))
    }
  }
  
  ## Now get loops that are more difficult. They are close to nodes, but separated by paths already found previously. Have to dig a little further.
  remaining0 <- igraph::induced_subgraph(skeleton0, vids = format(unused, scientific = FALSE, trim = TRUE))
  used <- as.numeric(unique(c(unlist(pathList), unlist(loopList))))
  unused <- as.numeric(vertexNames)[which(!(as.numeric(vertexNames) %in% used))]
  if (length(unused) > 0) {
    ends <- lapply(igraph::neighborhood(fullSkeleton0, order = 2, nodes = format(unused, scientific = FALSE, trim = TRUE)), function(x) nodeList[which(format(nodeList, scientific = FALSE, trim = TRUE) %in% names(x))])
    
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

#' getNodes
#'
#' Detect intersection points of an image thinned with thinImage.
#'
#' @param indices Where to check for intersection. The indices of pixels locations in 
#' the thinned image created by thinImage
#' @param dims dimensions of the image
#' @return Returns image matrix. 1 is blank, 0 is a node.
#' @noRd
getNodes <- function(indices, dims) {
  countChanges <- function(coords, img) {
    rr <- coords[1]
    cc <- coords[2]
    # If the point isn't in the first or last row or in the first or last column
    if (rr > 1 & cc > 1 & rr < dim(img)[1] & cc < dim(img)[2]) {
      # 1. Get a 3x3 matrix with the (rr, cc) as the center point and 8 pixels surrounding it. 
      # 2. Transpose the 3x3 matrix.
      # 3. Flatten the 3x3 matrix to a vector. The vector lists the elements of the 3x3 matrix in 
      # step 1, NOT step 2, starting in the top left and going by row. I.e. the first three elements of 
      # the vector are the first row of the step 1 matrix, the next three elements are the second row, and 
      # the final three elements are the third row.
      # 4. Reorder the vector starting with the pixel located directly above (r, c) in step 1 and then 
      # moving clockwise and ending at the pixel located directly above (r, c)
      neighbs <- c(t(img[(rr - 1):(rr + 1), ][, (cc - 1):(cc + 1)]))[c(2, 3, 6, 9, 8, 7, 4, 1, 2)]
      ## Count the number of zeros directly preceeded by a one
      # 1. create logical vector where it is TRUE if neighbs = 1 FALSE if neighbs = 0
      # 2. circular-shift neighbs to the left by 1 and create logical vector where 
      # TRUE if shifts neighbs = 1 and FALSE otherwise
      # 3. Count the number of entries that are TRUE in the logical vectors from step 1 and step 2.
      # Example: neighbs = 1 0 1 1 1 1 1 1 1 
      #   Step 1. TRUE FALSE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
      #   Step 2. shifted = 0 1 1 1 1 1 1 1 1
      #           TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
      #   Step 3. The result is 1
      # Example: neighbs = 1 0 1 1 0 0 1 1 1
      #   Step 1. TRUE FALSE TRUE TRUE FALSE FALSE TRUE TRUE TRUE
      #   Step 2. shifted = 0 1 1 0 0 1 1 1 1
      #           TRUE FALSE FALSE TRUE TRUE FALSE FALSE FALSE FALSE
      #   Step 3. The result is 2
      # Example: neighbs = 0 1 1 1 0 1 0 1 0
      #   Step 1. FALSE TRUE TRUE TRUE FALSE TRUE FALSE TRUE FALSE
      #   Step 2. shifted =  1 1 1 0 1 0 1 0 0
      #           FALSE FALSE FALSE TRUE FALSE TRUE FALSE TRUE TRUE
      #   Step 3. The result is 3
      return(sum(neighbs == 1 & c(neighbs[-1], neighbs[1]) == 0))
    } else {
      stop("Please use `crop` to crop your image. Not padded around outside.")
    }
  }
  
  node2by2fill <- function(coords, img) {
    # If there is a 2x2 block in the thinned image and none of those pixels are
    # nodes, make one of them a node. All will have connectivity of 2. Choose
    # pixel with most neighbors as node. Also make opposite diagonal pixel a
    # node. When nodes are combined later this will form 1 node that absorbs all
    # connections.
    rr <- coords[1]
    cc <- coords[2]
    
    # Check 2x2 neighborhood around point (rr, cc) to see if it has these values:
    #      | cc | cc+1 |
    # rr   | 0  | 0    |
    # rr+1 | 0  | 0    |
    if (img[rr, cc] == 0 & img[rr + 1, cc] == 0 & img[rr, cc + 1] == 0 & img[rr + 1, cc + 1] == 0) {
      # create matrix of indices coordinate points of 2x2 neighborhood 
      index2by2 <- matrix(c(rr, cc, rr + 1, cc, rr, cc + 1, rr + 1, cc + 1), byrow = TRUE, ncol = 2)
      # count the number of neighbors for each pixel in the 2x2 neighborhood
      numNeighbors <- colSums(apply(X = index2by2, MARGIN = 1, FUN = findNeighbors, img = img))
      # make the pixel with the most neighbors a node
      newNode <- index2by2[which.max(numNeighbors), ]
      # make its opposite neighbor a node
      oppositeCorner <- index2by2[(4:1)[which.max(numNeighbors)], ]
      return(c(newNode[1] + (newNode[2] - 1) * dim(img)[1], 
               oppositeCorner[1] + (oppositeCorner[2] - 1) * dim(img)[1]))
    } else {
      return(c(NA, NA))
    }
  }
  
  # First, we find endpoints and intersections of skeleton.
  
  # convert thinned image from list of pixel indices to matrix of 0 for
  # handwriting and 1 elsewhere
  img <- matrix(1, ncol = dims[2], nrow = dims[1])
  img[indices] <- 0
  
  # create a node at each endpoint (only one connected edge) and at each pixel with three or more 
  # connected edges
  img_m <- i_to_rc(indices, dims=dims)   # convert thinned image indices to row and column
  # for each pixel in the thinned image, count the number of connected edges
  changeCount <- matrix(apply(X = img_m, MARGIN = 1, FUN = countChanges, img = img), byrow = F, nrow = 1)
  nodes <- matrix(1, dims[1], dims[2])
  # add nodes to matrix as 0
  nodes[indices] <- ifelse(changeCount == 1 | changeCount >= 3, 0, 1)
  
  # check every pixel in thinned image for 2x2 filled neighborhood and assign
  # nodes to that neighborhood
  nodes2by2 <- t(apply(img_m, 1, FUN = node2by2fill, img = img))
  # add 2x2 nodes to nodes matrix as 0
  nodes[c(nodes2by2[apply(nodes2by2, 1, function(x) {all(!is.na(x))}), ])] <- 0
  
  nodeConnections <- c(indices[changeCount >= 3], c(nodes2by2[apply(nodes2by2, 1, function(x) {all(!is.na(x))}), ]))
  nodeList <- which(nodes == 0)
  # if a node isn't connected, assign it to the terminal nodes list
  terminalNodes <- nodeList[!(nodeList %in% nodeConnections)]
  
  return(list('nodeConnections' = nodeConnections, 'nodeList' = nodeList, 'terminalNodes' = terminalNodes))
}

#' getNodeGraph
#'
#' Internal function for creating a graph from a path list and node list. More specifically,
#' create a graph with a vertex for each node in nodeList. Then add an edge between the first and last
#' node (pixel / vertex) in each path in allPaths.
#'
#' @param allPaths list of paths
#' @param nodeList list of nodes
#' @return a graph of nodes
#' @noRd
getNodeGraph <- function(allPaths, nodeList) {
  nodeGraph <- igraph::make_empty_graph(directed = FALSE)
  nodeGraph <- igraph::add_vertices(nodeGraph, length(nodeList), name = format(nodeList, scientific = FALSE, trim = TRUE))
  for (i in 1:length(allPaths))
  {
    nodeGraph <- igraph::add_edges(nodeGraph, format(c(allPaths[[i]][1], allPaths[[i]][length(allPaths[[i]])]), scientific = FALSE, trim = TRUE))
  }
  return(nodeGraph)
}




#' getNodeOrder
#'
#' Internal function for ordering nodes in a letter.
#'
#' @param letter letter graph containing nodes to be ordered
#' @param nodesInGraph how many nodes are in the letter
#' @param nodeConnectivity how nodes are connected to each other
#' @param dims graph dimensions
#' @return order of the nodes
#' @noRd
getNodeOrder <- function(letter, nodesInGraph, nodeConnectivity, dims) {
  toRC <- function(nodes, dims) {
    cs <- (nodes - 1) %/% dims[1] + 1
    rs <- (nodes - 1) %% dims[1] + 1
    return(matrix(c(rs, cs), ncol = 2))
  }
  angleDiff <- function(fromIndex, toIndex, dims) {
    vecs <- toRC(c(fromIndex, toIndex), dims)
    diff <- c(vecs[1, 1] - vecs[2, 1], vecs[2, 2] - vecs[1, 2])
    return(atan2(diff[1], diff[2]))
  }
  
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

updateBreakPoints <- function(pathList, nodeList, breakPoints, terminalNodes, dims, allPaths) {
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

getSkeleton <- function(skeleton_df, indices, nodeList) {
  # build undirected skeleton_df with vertices=indices of the thinned writing and edges listed in skeleton_df
  skeleton <- igraph::graph_from_data_frame(d = skeleton_df, vertices = as.character(format(indices, scientific = FALSE, trim = TRUE)), directed = FALSE)
  # if more than one edge connects the same two vertices, combine the edges into a single edge and combine their attributes using the mean
  skeleton <- igraph::simplify(skeleton, remove.multiple = TRUE, edge.attr.comb = "mean")
  # color vertex as 1 if vertex is a node, otherwise color as 0
  igraph::V(skeleton)$color <- ifelse(igraph::V(skeleton)$name %in% nodeList, 1, 0)
  return(skeleton)
}

isolateGraphs <- function(allPaths, skeleton0, breakPoints, dims, pathList, loopList, nodeList, terminalNodes, hasTrough) {
  # Break on breakPoints and group points by which graph they fall into
  # NOTE: skip if skeleton0 and (or?) allPaths are empty. Even if breakPoints
  # is empty, still need to run makeGraphs() to assign graph number.
  graphList <- makeGraphs(allPaths, skeleton0, breakPoints)
  
  # get number of vertices in each letter
  graph_lengths <- unlist(lapply(graphList[[1]], length))
  
  # list graphs with more than 5 vertices and those with 5 or fewer
  graphs <- graphList[[1]][graph_lengths > 5]

  # get letter IDs for all leters, even ones with 5 vertices or less
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

#' Make Graphs
#'
#' Internal function that splits skeleton0 at the breakPoints to form
#' individuals graphs. Each graph is assigned an ID number.
#'
#' @param allPaths list of every path
#' @param skeleton0 graph of all nodes
#' @param breakPoints list of break points
#' @return List of graphs and graph IDs
#' @noRd
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

organizeLetters <- function(skeleton0) {
  letters <- replicate(n = length(na.omit(unique(igraph::V(skeleton0)$graphID))), list())
  strs <- names(igraph::V(skeleton0))
  for (i in 1:length(na.omit(unique(V(skeleton0)$graphID))))
  {
    tmp <- as.numeric(as.factor(V(skeleton0)$graphID))
    letters[[i]] <- as.numeric(strs[which(tmp == i)])
  }
  return(letters)
}

#' pathLetterAssociate
#'
#' Function associating entries in allPaths to each letter
#'
#' @param allPaths list of paths
#' @param letter individual character
#' @return associated path to each letter
#' @noRd
pathLetterAssociate <- function(allPaths, letter) {
  associatedPaths <- list()
  for (i in 1:length(allPaths)) {
    if (all(allPaths[[i]] %in% letter)) {
      associatedPaths <- c(associatedPaths, list(allPaths[[i]]))
    }
  }
  return(associatedPaths)
}

updateSkeleton0 <- function(graphdf0, skeleton0, nodeList){
  graphdf0 <- igraph::as_data_frame(skeleton0)
  graphdf0$node_only_dist <- ifelse(graphdf0$from %in% nodeList | graphdf0$to %in% nodeList, 1, 0)
  skeleton0 <- igraph::graph_from_data_frame(graphdf0, directed = FALSE)
  return(list('graphdf0'=graphdf0, 'skeleton0'=skeleton0))
}

#' findNeighbors
#'
#' Internal function for identifying which neighbors are black.
#'
#' @param coords coordinates to consider
#' @param img The image as a bitmap
#' @return Return a matrix of which neighbors are a black pixel
#' @noRd
findNeighbors <- function(coords, img) {
  rr <- coords[1]
  cc <- coords[2]
  neighbs <- c(t(img[(rr - 1):(rr + 1), ][, (cc - 1):(cc + 1)]))[c(2, 3, 6, 9, 8, 7, 4, 1)]
  yesNeighbs <- which(neighbs == 0)
  res <- as.matrix(rep(0, 8), nrow = 1)
  res[yesNeighbs] <- 1
  
  return(res)
}

#' findNeighbors0
#'
#' Internal function for identifying which neighbors are black excluding diagonals
#' to the middle point when a non-diagonal between those two vertices exists. For example, 
#' if the pixel above and the pixel to the right are black then exclude (set to zero) the pixel
#' above and to the right (the diagonal between the two) from the neighbors list
#'
#' @param coords coordinates to consider
#' @param img The image as a bitmap
#' @return Return a list of which neighbors are a black pixel excluding diagonals to the
#' middle point when a non-diagonal between those two vertices exists.
#' @noRd
findNeighbors0 <- function(coords, img) {
  # get matrix of which neighboring pixels are black. 1=pixel is black, 0=pixel is white.
  res <- findNeighbors(coords, img)
  
  # if pixel above and pixel to the right are neighbors, remove pixel above to the
  # right from neighbors list
  if (res[1] == 1 | res[3] == 1) {
    res[2] <- 0
  }
  # if pixel to the right and pixel to the bottom are neighbors, remove pixel below to the
  # right from neighbors list
  if (res[3] == 1 | res[5] == 1) {
    res[4] <- 0
  }
  # if pixel below and pixel to the left are neighbors, remove pixel below to the
  # left from neighbors list
  if (res[5] == 1 | res[7] == 1) {
    res[6] <- 0
  }
  # if pixel to the left and pixel to the top are neighbors, remove pixel above to the
  # left from neighbors list
  if (res[7] == 1 | res[1] == 1) {
    res[8] <- 0
  }
  return(res)
}

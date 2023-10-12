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
  doc$image <- readPNGBinary(path)
  doc$thin <- thinImage(doc$image)
  doc$process <- processHandwriting(doc$thin, dim(doc$image))
  doc$docname <- basename(path)
  doc$docname <- stringr::str_replace(doc$docname, ".png", "")
  doc$docname <- stringr::str_replace(doc$docname, ".PNG", "")
  return(doc)
}


#' Process Handwriting
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
  value <- from <- to <- nodeOnlyDist <- man_dist <- euc_dist <- pen_dist <- NULL

  # Next, we have to follow certain rules to find non intersection breakpoints.
  
  message("Starting Processing...")
  # convert thinned image from list of pixel indices to binary matrix: 0 for
  # handwriting and 1 elsewhere
  indices <- img  # thinned image as list of pixel indices
  img <- matrix(1, nrow = dims[1], ncol = dims[2])
  img[indices] <- 0

  message("Getting Nodes...", appendLF = FALSE)
  # output: nodeList[[1]] = all nodes
  # output: nodeList[[2]] = indices of nodes with 3 or more connected edges and indices of 2x2 nodes 
  nodeList <- getNodes(indices, dims)  
  nodeConnections <- nodeList[[2]]
  nodeList <- nodeList[[1]]
  # convert indices of pixels in thinned image to row and column
  img.m <- i_to_rc(indices, dims)
  
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
  neighborList <- t(apply(as.matrix(img.m, ncol = 2), 1, whichNeighbors, img = img))
  # build graphdf = a dataframe of edges in the thinned writing. Each pixel in the thinned writing
  # is a vertex, if two vertices are "neighbors" they are connected by an edge in graphdf.
  # melt converts list to dataframe where 
  #   Var1 = pixel number (1, 2,..., total num. pixels in thinned writing), 
  #   Var2 = neighborhood location index (in 1, 2,...,8), 
  #   1=the neighborhood location contains a neighbor, 0=neighborhood location does not contain a neighbor
  graphdf <- melt(neighborList)
  # filter for neighbors
  graphdf <- subset(graphdf, value == 1)
  # get index in thinned image of each pixel
  graphdf$from <- indices[graphdf$Var1]
  # get the index in thinned image of the neighbor pixel for each pixel
  graphdf$to <- graphdf$from + c(-1, dims[1] - 1, dims[1], dims[1] + 1, 1, 1 - dims[1], -dims[1], -1 - dims[1])[graphdf$Var2]
  # Manhattan distance between each pixel and its neighbor
  graphdf$man_dist <- rep(c(1, 2), 4)[graphdf$Var2]
  # Euclidean distance between each pixel and its neighbor
  graphdf$euc_dist <- c(1, sqrt(2), 1, sqrt(2), 1, sqrt(2), 1, sqrt(2))[graphdf$Var2]
  graphdf$pen_dist <- c(1, 3, 1, 3, 1, 3, 1, 3)[graphdf$Var2]

  # Step 1: make matrix of neighborhoods for each pixel in the thinned writing: 
  #   each row corresponds to a pixel in the thinned writing
  #   each column corresponds to a location in the pixel's neighborhood
  #   1=the neighborhood location contains a neighbor, 0=neighborhood location does not contain a neighbor
  # Step 2: remove neighbors on the diagonal if there are neighbors in the neighborhood on either side of the diagonal.
  # Example, if there is a neighbor in the top right pixel, and there are also neighbors in both the top and the right pixels.
  # Remove the neighbor in the top right pixel.
  neighborList0 <- t(apply(as.matrix(img.m, ncol = 2), 1, whichNeighbors0, img = img))
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
  graphdf0$nodeOnlyDist <- ifelse(graphdf0$from %in% nodeList | graphdf0$to %in% nodeList, 1, 0)
  # format from and to columns as character
  graphdf0$from <- as.character(format(graphdf0$from, scientific = FALSE, trim = TRUE))  # trim=TRUE suppresses leading blanks for justification of numbers
  graphdf0$to <- as.character(format(graphdf0$to, scientific = FALSE, trim = TRUE))
  # drop Var1, Var2, and value columns
  graphdf0 <- subset(graphdf0, select = c(from, to, nodeOnlyDist))
  
  # format from and to columns as character in graphdf
  graphdf$from <- as.character(format(graphdf$from, scientific = FALSE, trim = TRUE))
  graphdf$to <- as.character(format(graphdf$to, scientific = FALSE, trim = TRUE))
  # drop Var1, Var2, and value columns
  graphdf <- subset(graphdf, select = c(from, to, man_dist, euc_dist, pen_dist))

  # build undirected graph with vertices=indices of the thinned writing and edges listed in graphdf
  skel_graph <- igraph::graph_from_data_frame(d = graphdf, vertices = as.character(format(indices, scientific = FALSE, trim = TRUE)), directed = FALSE)
  # build undirected graph with vertices=indices of the thinned writing and edges listed in graphdf0
  skel_graph0 <- igraph::graph_from_data_frame(d = graphdf0, vertices = as.character(format(indices, scientific = FALSE, trim = TRUE)), directed = FALSE)
  # if more than one edge connects the same two vertices, combine the edges into a single edge and combine their attributes using the mean
  skel_graph <- igraph::simplify(skel_graph, remove.multiple = TRUE, edge.attr.comb = "mean")
  skel_graph0 <- igraph::simplify(skel_graph0, remove.multiple = TRUE, edge.attr.comb = "mean")
  
  # color vertex as 1 if vertex is a node, otherwise color as 0
  V(skel_graph)$color <- ifelse(V(skel_graph)$name %in% nodeList, 1, 0)
  V(skel_graph0)$color <- ifelse(V(skel_graph0)$name %in% nodeList, 1, 0)

  # if a node isn't connected, assign it to the terminal nodes list
  terminalNodes <- nodeList[!(nodeList %in% nodeConnections)]
  
  # Weight each edge in skel_graph0 with nodeOnlyDist (1=neighbor of node, 0 otherwise).
  # Then find the shortest distance from each node to each other node.
  dists0 <- distances(skel_graph0, 
                      v = as.character(format(nodeList, scientific = FALSE, trim = TRUE)), 
                      to = as.character(format(nodeList, scientific = FALSE, trim = TRUE)), 
                      weights = E(skel_graph0)$nodeOnlyDist)
  adj0 <- ifelse(dists0 == 1 | dists0 == 2, 1, 0)

  message("and merging them...")
  emergencyBreak <- 100
  while (TRUE) {
    originalNodeList <- nodeList
    # count smallest number of edges between each pair of vertices (each edge weighted as 1?)
    distsFull <- distances(skel_graph0, 
                           v = as.character(format(nodeList, scientific = FALSE, trim = TRUE)), 
                           to = as.character(format(nodeList, scientific = FALSE, trim = TRUE)), 
                           weights = NA)
    distsFull[!upper.tri(distsFull)] <- 0
    # flag nodes that are only 1 or 2 edges apart
    nodesToMerge <- which(distsFull <= 2 & distsFull > 0)
    rNodes <- ((nodesToMerge - 1) %% length(nodeList)) + 1
    cNodes <- ((nodesToMerge - 1) %/% length(nodeList)) + 1
    mergeSets <- cbind(nodeList[rNodes], nodeList[cNodes])
    # add column: 0=one or both nodes is terminal, 1=neigther is terminal
    mergeSets <- cbind(mergeSets, apply(mergeSets, 1, function(x) {
      all(!(x %in% terminalNodes))
    }))
    mergeSets <- mergeSets[mergeSets[, 3] == 1, c(1, 2)]
    mergeSets <- matrix(mergeSets, ncol = 2)
    
    # end while loop if no nodes need to be merged
    if (dim(mergeSets)[1] == 0) break

    if (anyDuplicated(c(mergeSets)) > 0) {
      duplicates <- which(mergeSets %in% mergeSets[apply(matrix(duplicated(c(mergeSets)), ncol = 2), 1, any)])
      rduplicates <- ((duplicates - 1) %% dim(mergeSets)[1]) + 1
      duplicateDists <- distsFull[matrix(as.character(format(mergeSets[rduplicates, ], scientific = FALSE, trim = TRUE)), ncol = 2)]
    }
    newNodes <- findMergeNodes(skel_graph, mergeSets)
    nodeList <- unique(c(nodeList[!(nodeList %in% c(mergeSets[, c(1, 2)]))], newNodes))

    # At this point have the updated nodeList, if we wanted to break it letter by letter we would do that here

    ### Migrate connections from original nodes to new nodes
    toDelete <- NULL
    nRowCol <- dim(adj0)[1]
    for (i in 1:dim(mergeSets)[1])
    {
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

    emergencyBreak <- emergencyBreak - 1
    if (emergencyBreak == 0) {
      break()
    }
  }

  graphdf0 <- as_data_frame(skel_graph0)
  graphdf0$nodeOnlyDist <- ifelse(graphdf0$from %in% nodeList | graphdf0$to %in% nodeList, 1, 0.00001)
  skel_graph0 <- graph_from_data_frame(graphdf0, directed = FALSE)

  adj0[lower.tri(adj0)] <- 0
  adj.m <- melt(adj0)
  adj.m <- subset(adj.m, value == 1)
  names(adj.m) <- c("from", "to", "value")

  message("Finding direct paths...", appendLF = FALSE)
  pathList <- AllUniquePaths(adj.m, skel_graph, skel_graph0)

  message("and loops...")
  loopList <- getLoops(nodeList, skel_graph, skel_graph0, pathList, dim(img))

  allPaths <- append(pathList, loopList)

  graphdf0 <- as_data_frame(skel_graph0)
  graphdf0$nodeOnlyDist <- ifelse(graphdf0$from %in% nodeList | graphdf0$to %in% nodeList, 1, 0)
  skel_graph0 <- graph_from_data_frame(graphdf0, directed = FALSE)

  # Nominate and check candidate breakpoints
  message("Looking for letter break points...", appendLF = FALSE)
  hasTrough <- rep(FALSE, length(pathList))
  troughNodes <- c()
  candidateNodes <- c()
  for (i in 1:length(pathList))
  {
    # Look for troughs in edges.
    tempPath <- pathList[[i]]
    if (length(tempPath) > 10) {
      rows <- ((tempPath - 1) %% dims[1]) + 1
      for (j in 5:(length(rows) - 4))
      {
        if (any(rows[1:(j - 1)] < rows[j] - 1) & any(rows[(j + 1):length(rows)] < rows[j] - 1)) {
          lowerEnd <- max(which(rows[1:(j - 1)] < rows[j] - 1))
          upperEnd <- min(which(rows[(j + 1):length(rows)] < rows[j] - 1))
          if (!any(rows[lowerEnd:(j + upperEnd)] > rows[j])) {
            troughNodes <- c(troughNodes, tempPath[j])
            hasTrough[i] <- TRUE
          }
        }
      }
    }
    if (hasTrough[i] == FALSE) {
      candidateNodes <- c(candidateNodes, tempPath[ceiling(length(tempPath) / 2)])
    }
  }
  breaks <- which((((troughNodes[-1] - 1) %% dims[1]) + 1) != (((troughNodes[-length(troughNodes)] - 1) %% dims[1]) + 1) |
    ((((troughNodes[-1] - 1) %/% dims[1]) + 1) != (((troughNodes[-length(troughNodes)] - 1) %/% dims[1])) &
      (((troughNodes[-1] - 1) %/% dims[1])) != (((troughNodes[-length(troughNodes)] - 1) %/% dims[1]) + 1)))
  breaks <- c(1, breaks, length(troughNodes))
  candidateNodes <- c(candidateNodes, troughNodes[ceiling((breaks[-1] + breaks[-length(breaks)]) / 2)])

  message("and discarding bad ones...")

  goodBreaks <- checkBreakPoints(candidateNodes = candidateNodes, allPaths = pathList, nodeGraph = getNodeGraph(pathList, nodeList), terminalNodes = terminalNodes, dims)
  preStackBreaks <- candidateNodes[goodBreaks]

  pathsWithBreaks <- lapply(allPaths, function(x) {
    which(x %in% preStackBreaks)
  })
  breaksPerPath <- unlist(lapply(pathsWithBreaks, length))
  for (i in which(breaksPerPath > 1))
  {
    newBreak <- floor(mean(which(allPaths[[i]] %in% preStackBreaks)))
    preStackBreaks <- preStackBreaks[which(!(preStackBreaks %in% allPaths[[i]]))]
    preStackBreaks <- c(preStackBreaks, allPaths[[i]][newBreak])
  }


  message("Isolating letter paths...")

  ## Break on breakpoints and group points by which letter they fall into. Adjust graph accordingly.
  letterList <- letterPaths(allPaths, skel_graph0, preStackBreaks)
  letters <- letterList[[1]][unlist(lapply(letterList[[1]], length)) > 5]

  V(skel_graph0)$letterID <- letterList[[2]]
  skel_graph0 <- delete.vertices(skel_graph0, V(skel_graph0)[which(V(skel_graph0)$letterID %in% which(unlist(lapply(letterList[[1]], length)) <= 5))])
  V(skel_graph0)$letterID[!is.na(V(skel_graph0)$letterID)] <- as.numeric(as.factor(na.omit(V(skel_graph0)$letterID)))

  # Remove breakpoints that shouldn't have broken.
  finalBreaks <- preStackBreaks[!(checkStacking(preStackBreaks, allPaths, letters, skel_graph0, dims))]
  finalBreaks <- finalBreaks[!(checkSimplicityBreaks(finalBreaks, pathList, loopList, letters, skel_graph0, nodeList, terminalNodes, hasTrough, dims))]

  pathsWithBreaks <- lapply(allPaths, function(x) {
    which(x %in% preStackBreaks)
  })
  breaksPerPath <- unlist(lapply(pathsWithBreaks, length))
  for (i in which(breaksPerPath > 0))
  {
    newNodes <- pathsWithBreaks[[i]]
    if (allPaths[[i]][newNodes] %in% finalBreaks) {
      tryCatch(
        expr = {
          E(skel_graph0, P = format(allPaths[[i]][c(newNodes - 2, newNodes - 1)], scientific = FALSE, trim = TRUE))$nodeOnlyDist <- 1
          E(skel_graph0, P = format(allPaths[[i]][c(newNodes + 1, newNodes + 2)], scientific = FALSE, trim = TRUE))$nodeOnlyDist <- 1
        }, error = function(e) {

        }
      )

      newNodes <- c(newNodes - 1, newNodes + 1)
      nodeList <- c(nodeList, allPaths[[i]][newNodes])
      allPaths[[i]] <- list(allPaths[[i]][1:(newNodes[1])], allPaths[[i]][(newNodes[2]):length(allPaths[[i]])])
    }

    # letterIDs = range(V(skel_graph0)$letterID[names(V(skel_graph0)) %in% format(allPaths[[i]], scientific = FALSE, trim = TRUE)], na.rm = TRUE)
    # Had to break this above line into this to stop a range(numeric(0)) error from happening
    skelInPaths <- V(skel_graph0)$letterID[names(V(skel_graph0)) %in% format(allPaths[[i]], scientific = FALSE, trim = TRUE)]
    if (identical(skelInPaths, numeric(0))) {
      letterIDs <- c(Inf, -Inf)
    } else {
      letterIDs <- range(skelInPaths, na.rm = TRUE)
    }

    V(skel_graph0)$letterID[which(V(skel_graph0)$letterID == letterIDs[2])] <- letterIDs[1]
    V(skel_graph0)$letterID[which(names(V(skel_graph0)) %in% format(allPaths[[i]][newNodes], scientific = FALSE, trim = TRUE))] <- letterIDs[1]
  }

  allPaths <- lapply(rapply(allPaths, enquote, how = "unlist"), eval)

  message("Organizing letters...")
  letters <- organize_letters(skel_graph0)

  message("Creating letter lists...")
  letterList <- create_letter_lists(allPaths, letters, nodeList, nodeConnections, terminalNodes, dims)

  message("Adding character features...")
  letterList <- add_character_features(img, letterList, letters, dims)

  message("Document processing complete.\n")

  return(list(nodes = nodeList, connectingNodes = nodeConnections, terminalNodes = terminalNodes, breakPoints = sort(finalBreaks), letterList = letterList))
}


# Internal Functions ------------------------------------------------------
#' add_character_features
#'
#' Internal method that adds features to characters
#'
#' @param img thinned binary image
#' @param letterList list containing letter characters
#' @param letters individual characters from letterList
#' @param dims image graph dimensions
#' @return a list of letters with features applied
#' @noRd
add_character_features <- function(img, letterList, letters, dims) {
  featureSets <- extract_character_features(img, letterList, dims)

  for (i in 1:length(letters))
  {
    letterList[[i]]$characterFeatures <- featureSets[[i]]
  }

  letterPlaces <- matrix(unlist(lapply(featureSets, FUN = function(x) {
    c(x$line_number, x$order_within_line)
  })), ncol = 2, byrow = TRUE)
  letterOrder <- order(letterPlaces[, 1], letterPlaces[, 2])
  letterList <- letterList[letterOrder]

  return(letterList)
}

#' AllUniquePaths
#'
#' Internal function for getting a list of all non loop paths in a writing sample.
#'
#' @param adj adjacent matrix
#' @param graph first skeletonized graph
#' @param graph0 second skeletonized graph
#' @return a list of all non loop paths
#' @noRd
AllUniquePaths <- function(adj, graph, graph0) {
  # Gets all paths that are not loops
  # paths = apply(adj, 1, LooplessPaths, graph = graph, graph0 = graph0)
  paths <- list()
  if (dim(adj)[1] == 0) {
    return(NULL)
  }
  #
  for (i in 1:dim(adj)[1])
  {
    fromNode <- as.character(format(adj[i, 1], scientific = FALSE, trim = TRUE))
    toNode <- as.character(format(adj[i, 2], scientific = FALSE, trim = TRUE))

    while (shortest.paths(graph0, v = fromNode, to = toNode, weights = E(graph0)$nodeOnlyDist) < 3 & shortest.paths(graph0, v = fromNode, to = toNode, weights = E(graph0)$nodeOnlyDist) >= 1) {
      shortest <- shortest_paths(graph0, from = fromNode, to = toNode, weights = E(graph0)$nodeOnlyDist)
      len <- length(unlist(shortest[[1]]))
      paths <- c(paths, list(as.numeric(names(shortest$vpath[[1]]))))
      if (len > 2) {
        graph <- delete.edges(graph, paste0(names(shortest$vpath[[1]])[len %/% 2], "|", names(shortest$vpath[[1]])[len %/% 2 + 1]))
        graph0 <- delete.edges(graph0, paste0(names(shortest$vpath[[1]])[len %/% 2], "|", names(shortest$vpath[[1]])[len %/% 2 + 1]))
      } else if (len == 2) {
        graph0 <- delete.edges(graph0, paste0(names(shortest$vpath[[1]])[1], "|", names(shortest$vpath[[1]])[2]))
      } else {
        stop("There must be some mistake. Single node should have nodeOnlyDist path length of 0.")
      }
    }
  }

  return(paths)
}

#' checkBreakPoints
#'
#' Internal function called by processHandwriting that eliminates breakpoints based on rules to try to coherently separate letters.
#'
#' @param candidateNodes possible breakpoints
#' @param allPaths list of paths
#' @param nodeGraph graph of nodes; call the getNodeGraph function
#' @param terminalNodes nodes at the endpoints of the graph
#' @param dims graph dimensions
#'
#' @return a graph without breakpoints and separated letters
#' @noRd
checkBreakPoints <- function(candidateNodes, allPaths, nodeGraph, terminalNodes, dims) {
  # Check rules for candidate breakpoints
  breakFlag <- rep(TRUE, length(candidateNodes))

  for (i in 1:length(allPaths))
  {
    tempPath <- format(allPaths[[i]], scientific = FALSE, trim = TRUE)
    nodeChecks <- which(candidateNodes %in% tempPath)
    tempNodeGraph <- delete.edges(nodeGraph, paste0(tempPath[1], "|", tempPath[length(tempPath)]))

    if (distances(tempNodeGraph, v = tempPath[1], to = tempPath[length(tempPath)]) < Inf) {
      # No breaking on multiple paths between nodes.
      breakFlag[nodeChecks] <- FALSE
    } else if (any(tempPath %in% terminalNodes)) {
      # No break if path has an endpoint
      breakFlag[nodeChecks] <- FALSE
    } else if (any(which(tempPath %in% c(candidateNodes[nodeChecks])) <= 4 | which(tempPath %in% c(candidateNodes[nodeChecks])) >= length(tempPath) - 3) | length(tempPath) <= 10) {
      # No breaks too close to a vertex
      breakFlag[nodeChecks[which(candidateNodes[nodeChecks] <= 5 | candidateNodes[nodeChecks] >= length(tempPath) - 4)]] <- FALSE
    }
  }

  return(breakFlag)
}

#' checkSimplicityBreaks
#'
#' Internal function for removing breakpoints that separate graphs that are too simple to be split. Remove break if graph on left and right of the break have 4 or fewer nodes and no loops or double paths. Never remove break on a trough.
#'
#' @param candidateBreaks possible breakpoints
#' @param pathList list of paths
#' @param loopList list of loops
#' @param letters list of individual letter characters
#' @param nodeGraph0 skeletonized graph
#' @param nodeList list of nodes
#' @param terminalNodes nodes at the ends of letters
#' @param hasTrough whether or not break has a trough
#' @param dims graph dimensions
#' @return removes breakpoints on simple graphs
#' @noRd
checkSimplicityBreaks <- function(candidateBreaks, pathList, loopList, letters, nodeGraph0, nodeList, terminalNodes, hasTrough, dims) {
  tooSimpleFlag <- rep(FALSE, length(candidateBreaks))
  for (i in 1:length(pathList))
  {
    tempPath <- pathList[[i]]
    nodestoCheck <- which(candidateBreaks %in% tempPath)
    if (length(nodestoCheck) >= 1) {
      if (!hasTrough[i]) {
        pathIndex <- which(tempPath == candidateBreaks[nodestoCheck])

        borderLetters <- c(
          V(nodeGraph0)$letterID[which(V(nodeGraph0)$name == tempPath[pathIndex - 1])],
          V(nodeGraph0)$letterID[which(V(nodeGraph0)$name == tempPath[pathIndex + 1])]
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
#' @param candidateBreaks possible breaks for letterpath
#' @param allPaths list of paths
#' @param letters list of individual letter characters
#' @param nodeGraph0 skeletonized graph
#' @param dims graph dimensions
#' @return stackPtFlag
#' @noRd
checkStacking <- function(candidateBreaks, allPaths, letters, nodeGraph0, dims) {
  stackPtFlag <- rep(FALSE, length(candidateBreaks))

  for (i in 1:length(allPaths))
  {
    tempPath <- allPaths[[i]]
    tempRow <- ((tempPath - 1) %% dims[1]) + 1
    tempCol <- ((tempPath - 1) %/% dims[1]) + 1
    nodeChecks <- which(candidateBreaks %in% tempPath)
    if (length(nodeChecks) == 1) {
      if (abs((max(tempRow) - min(tempRow)) / (max(tempCol) + 1 - min(tempCol))) > 2) {
        stackPtFlag[nodeChecks] <- TRUE
      } else {
        pathIndex <- which(tempPath == candidateBreaks[nodeChecks])

        borderLetters <- c(
          V(nodeGraph0)$letterID[which(V(nodeGraph0)$name == tempPath[pathIndex - 1])],
          V(nodeGraph0)$letterID[which(V(nodeGraph0)$name == tempPath[pathIndex + 1])]
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


create_letter_lists <- function(allPaths, letters, nodeList, nodeConnections, terminalNodes, dims) {
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


  letterList <- replicate(length(letters), list(path = NA, nodes = NA), simplify = FALSE)
  for (i in 1:length(letters))
  {
    letterList[[i]]$path <- letters[[i]]
    # letterList[[i]]$nodes = nodesinGraph[[i]][nodeOrder[[i]]]
    letterList[[i]]$allPaths <- pathLetterAssociate(allPaths, letters[[i]])
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
      letterList[[i]]$adjMatrix <- matrix(0, ncol = length(nodesinGraph[[i]]), nrow = length(nodesinGraph[[i]]))

      pathStarts <- unlist(lapply(letterList[[i]]$allPaths, function(x) x[1]))
      pathEnds <- unlist(lapply(letterList[[i]]$allPaths, function(x) x[length(x)]))

      connectivityScores[[i]] <- getConnectivity(pathEndings = c(pathStarts, pathEnds), nodesSingle = nodesinGraph[[i]])

      nodeOrder[[i]] <- getNodeOrder(letters[[i]], nodesinGraph[[i]], connectivityScores[[i]], dims)

      nodeSet <- nodesinGraph[[i]][order(nodeOrder[[i]])]

      warn <- FALSE
      for (j in 1:length(pathStarts))
      {
        if (!(pathStarts[j] %in% nodeSet)) {
          warning(paste0("Maybe a loop that didn't merge with node. letterList[[", i, "]]"))
          warn <- TRUE
        } else {
          pathStarts[j] <- which(nodeSet == pathStarts[j])
        }

        if (!(pathEnds[j] %in% nodeSet)) {
          warning(paste0("Maybe a loop that didn't merge with node. letterList[[", i, "]]"))
          warn <- TRUE
        } else {
          pathEnds[j] <- which(nodeSet == pathEnds[j])
        }
      }
      if (warn) {
        next
      }

      letterList[[i]]$adjMatrix[cbind(pathStarts, pathEnds)] <- 1
      letterList[[i]]$adjMatrix[cbind(pathEnds, pathStarts)] <- 1
      binCode <- t(letterList[[i]]$adjMatrix)[!upper.tri(letterList[[i]]$adjMatrix)]
      lenBinCode <- length(binCode)
      binCode <- c(rep(0, (-1 * lenBinCode) %% 4), binCode)
      for (j in 1:(length(binCode) / 4))
      {
        decCode[i] <- paste0(decCode[i], LETTERS[sum(binCode[(4 * (j - 1) + 1):(4 * j)] * 2^((4:1) - 1)) + 1])
      }
      letterList[[i]]$letterCode <- decCode[i]
      letterList[[i]]$nodes <- sort(nodesinGraph[[i]][order(nodeOrder[[i]])])
      letterList[[i]]$connectingNodes <- sort(connectingNodesinGraph[[i]][order(nodeOrder[[i]])])
      letterList[[i]]$terminalNodes <- sort(terminalNodesinGraph[[i]][order(nodeOrder[[i]])])
      colnames(letterList[[i]]$adjMatrix) <- format(letterList[[i]]$nodes, scientific = FALSE, trim = TRUE)
      rownames(letterList[[i]]$adjMatrix) <- format(letterList[[i]]$nodes, scientific = FALSE, trim = TRUE)
    } else {
      letterList[[i]]$adjMatrix <- matrix(0, ncol = 0, nrow = 0)
      letterList[[i]]$nodes <- sort(nodesinGraph[[i]])
      letterList[[i]]$connectingNodes <- sort(connectingNodesinGraph[[i]])
      letterList[[i]]$terminalNodes <- sort(terminalNodesinGraph[[i]])
      letterList[[i]]$letterCode <- "A"
    }
  }

  return(letterList)
}


#' countChanges
#'
#' Internal function for counting the number of edges connected to a point
#'
#' @param coords \(row, column\) coordinates of a single point to consider
#' @param img The non-thinned image as binary bit map. Double-check: processHandwriting might call countChanges on thinned image
#' @return The number of edges connected to coords
#' @noRd
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

#' countNodes
#'
#' Function for counting nodes in a list of letters.
#'
#' @param letterList list containing letter characters
#' @param nodes list of nodes
#' @return number of nodes in letterList
#' @noRd
countNodes <- function(letterList, nodes) {
  unlist(lapply(letterList, function(x) {
    sum(x %in% nodes)
  }))
}

#' findMergeNodes
#'
#' Internal function to merge nodes that are very close together.
#'
#' @param skel_graph the skeltonized graph
#' @param mergeMat sets of the nodes to merge into a single nodes
#' @return The merged node
#' @noRd
findMergeNodes <- function(skel_graph, mergeMat) {
  newNodes <- rep(NA, dim(mergeMat)[1])
  for (i in 1:dim(mergeMat)[1])
  {
    fromNode <- as.character(format(mergeMat[i, 1], scientific = FALSE, trim = TRUE))
    toNode <- as.character(format(mergeMat[i, 2], scientific = FALSE, trim = TRUE))
    path <- shortest_paths(skel_graph, from = fromNode, to = toNode, weights = E(skel_graph)$pen_dist)$vpath[[1]]
    len <- length(path)
    newNodes[i] <- as.numeric(names(path[ceiling(len / 2)]))
  }
  return(newNodes)
}


#' getLoops
#'
#' Internal function for getting looped paths.
#'
#' @param nodeList A list of all found nodes
#' @param graph first skeletonized graph
#' @param graph0 second skeletonized graph
#' @param pathList The current path list to check for loops
#' @param dims dimensions of the image
#' @return A list of all loops found
#'
#' @importFrom utils combn
#' @noRd
getLoops <- function(nodeList, graph, graph0, pathList, dims) {
  vertexNames <- names(V(graph0))

  fullGraph0 <- graph0

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
  unusedGraph <- graph_from_adjacency_matrix(unusedAdj, mode = "undirected")

  graph0 <- intersection(graph0, unusedGraph, keep.all.vertices = TRUE)
  graph <- intersection(graph, graph0, byname = TRUE, keep.all.vertices = TRUE)
  check <- unused[degree(graph0, as.character(format(unused, scientific = FALSE, trim = TRUE))) > 1]
  check <- check[which(check %in% nodeList)]

  loopList <- list()

  tryCatch(
    expr = {
      neighbors <- neighborhood(graph, nodes = as.character(check))

      if (any(unlist(lapply(neighbors, length)) > 3)) {
        warning("At least 1 of the nodes in the potential loops has more than 2 neighbors after removal of the connections. Try again! \nThe nodes in question are: \n", dput(names(neighbors)[which(unlist(lapply(neighbors, length)) > 3)]))
      }

      ## Get paths that start and end at the same point, where that point is a node in nodeList
      if (length(neighbors) > 0) {
        for (i in 1:length(neighbors)) {
          neigh <- as.numeric(names(neighbors[[i]]))
          graph <- delete.edges(graph, paste0(neigh[1], "|", neigh[2]))
          if (distances(graph, v = as.character(neigh[1]), to = as.character(neigh[2])) < Inf) {
            newPath <- as.numeric(names(unlist(shortest_paths(graph, from = format(neigh[1], scientific = FALSE), to = format(neigh[2], scientific = FALSE), weights = E(graph)$pen_dist)$vpath)))
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
  remaining0 <- induced_subgraph(graph0, vids = format(c(unused, nodeList), scientific = FALSE, trim = TRUE))
  numNeighbors <- lapply(neighborhood(remaining0, nodes = V(remaining0)), length)
  remaining0 <- induced_subgraph(remaining0, vids = V(remaining0)[numNeighbors > 1])

  roots <- format(nodeList[which(nodeList %in% names(V(remaining0)))], scientific = FALSE, trim = TRUE)

  if (length(roots) > 0) {
    for (i in 1:length(roots))
    {
      loopPart1 <- names(na.omit(dfs(remaining0, roots[i], unreachable = FALSE)$order))
      loopPart2 <- shortest_paths(fullGraph0, from = loopPart1[length(loopPart1)], to = roots[i])$vpath[[1]][-1]
      loopList <- append(loopList, list(as.numeric(c(loopPart1, names(loopPart2)))))
    }
  }

  ## Now get loops that are more difficult. They are close to nodes, but separated by paths already found previously. Have to dig a little further.
  remaining0 <- induced_subgraph(graph0, vids = format(unused, scientific = FALSE, trim = TRUE))
  used <- as.numeric(unique(c(unlist(pathList), unlist(loopList))))
  unused <- as.numeric(vertexNames)[which(!(as.numeric(vertexNames) %in% used))]
  if (length(unused) > 0) {
    ends <- lapply(neighborhood(fullGraph0, order = 2, nodes = format(unused, scientific = FALSE, trim = TRUE)), function(x) nodeList[which(format(nodeList, scientific = FALSE, trim = TRUE) %in% names(x))])

    roots <- format(unique(unlist(ends)), scientific = FALSE, trim = TRUE)
    if (length(roots) > 0) {
      ends <- neighborhood(fullGraph0, order = 2, nodes = roots)
      ends <- unlist(lapply(ends, function(x) names(x)[which(names(x) %in% format(unused, scientific = FALSE, trim = TRUE))][1]))

      for (i in 1:length(roots))
      {
        loopPart1 <- names(na.omit(dfs(remaining0, ends[i], unreachable = FALSE)$order))
        loopPart2a <- shortest_paths(fullGraph0, from = loopPart1[1], to = roots[i])$vpath[[1]][-1]
        loopPart2b <- shortest_paths(fullGraph0, from = loopPart1[length(loopPart1)], to = roots[i])$vpath[[1]][-1]
        loopList <- append(loopList, list(as.numeric(c(rev(names(loopPart2a)), loopPart1, names(loopPart2b)))))
      }
    }
  }


  ## And a little deeper
  remaining0 <- induced_subgraph(graph0, vids = format(unused, scientific = FALSE, trim = TRUE))
  used <- as.numeric(unique(c(unlist(pathList), unlist(loopList))))
  unused <- as.numeric(vertexNames)[which(!(as.numeric(vertexNames) %in% used))]
  if (length(unused) > 0) {
    ends <- lapply(neighborhood(fullGraph0, order = 3, nodes = format(unused, scientific = FALSE, trim = TRUE)), function(x) nodeList[which(format(nodeList, scientific = FALSE, trim = TRUE) %in% names(x))])

    roots <- format(unique(unlist(ends)), scientific = FALSE, trim = TRUE)
    if (length(roots) > 0) {
      ends <- neighborhood(fullGraph0, order = 3, nodes = roots)
      ends <- unlist(lapply(ends, function(x) names(x)[which(names(x) %in% format(unused, scientific = FALSE, trim = TRUE))][1]))

      for (i in 1:length(roots))
      {
        loopPart1 <- names(na.omit(dfs(remaining0, ends[i], unreachable = FALSE)$order))
        loopPart2a <- shortest_paths(fullGraph0, from = loopPart1[1], to = roots[i])$vpath[[1]][-1]
        loopPart2b <- shortest_paths(fullGraph0, from = loopPart1[length(loopPart1)], to = roots[i])$vpath[[1]][-1]
        loopList <- append(loopList, list(as.numeric(c(rev(names(loopPart2a)), loopPart1, names(loopPart2b)))))
      }
    }
  }

  ## All that remains now is perfect loops. Start and end at same point with no intersections or end points.
  remaining0 <- induced_subgraph(remaining0, vids = V(remaining0)[!(names(V(remaining0)) %in% unlist(loopList))])
  while (TRUE) {
    if (length(V(remaining0)) > 0) {
      perfectLoop <- names(na.omit(dfs(remaining0, V(remaining0)[1], unreachable = FALSE)$order))
      remaining0 <- delete.vertices(remaining0, v = perfectLoop)
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
  ## First, we find endpoints and intersections of skeleton.
  
  # convert thinned image from list of pixel indices to matrix of 0 for
  # handwriting and 1 elsewhere
  img <- matrix(1, ncol = dims[2], nrow = dims[1])
  img[indices] <- 0
  
  # create a node at each endpoint (only one connected edge) and at each pixel with three or more 
  # connected edges
  img.m <- i_to_rc(indices, dims=dims)   # convert thinned image indices to row and column
  # for each pixel in the thinned image, count the number of connected edges
  changeCount <- matrix(apply(X = img.m, MARGIN = 1, FUN = countChanges, img = img), byrow = F, nrow = 1)
  nodes <- matrix(1, dims[1], dims[2])
  # add nodes to matrix as 0
  nodes[indices] <- ifelse(changeCount == 1 | changeCount >= 3, 0, 1)

  ## If there is a 2x2 block in the thinned image and none of those pixels are nodes, make one of them a node.
  ## All will have connectivity of 2. Choose pixel with most neighbors as node. Also make opposite diagonal pixel a node.
  ## When nodes are combined later this will form 1 node that absorbs all connections.
  node2by2fill <- function(coords, img) {
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
      numNeighbors <- colSums(apply(X = index2by2, MARGIN = 1, FUN = whichNeighbors, img = img))
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
  
  # check every pixel in thinned image for 2x2 filled neighborhood and assign
  # nodes to that neighborhood
  nodes2by2 <- t(apply(img.m, 1, FUN = node2by2fill, img = img))
  # add 2x2 nodes to nodes matrix as 0
  nodes[c(nodes2by2[apply(nodes2by2, 1, function(x) {all(!is.na(x))}), ])] <- 0

  return(list(which(nodes == 0), 
              c(indices[changeCount >= 3], c(nodes2by2[apply(nodes2by2, 1, function(x) {all(!is.na(x))}), ]))
              ))
}

#' getNodeGraph
#'
#' Internal function for creating a graph from a path list and node list.
#'
#' @param allPaths list of paths
#' @param nodeList list of nodes
#' @return a graph of nodes
#' @noRd
getNodeGraph <- function(allPaths, nodeList) {
  nodeGraph <- make_empty_graph(directed = FALSE)
  nodeGraph <- add_vertices(nodeGraph, length(nodeList), name = format(nodeList, scientific = FALSE, trim = TRUE))
  for (i in 1:length(allPaths))
  {
    nodeGraph <- add.edges(nodeGraph, format(c(allPaths[[i]][1], allPaths[[i]][length(allPaths[[i]])]), scientific = FALSE, trim = TRUE))
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

#' letterPaths
#'
#' Internal function that uses existing breakPoint list to assign letters to the nodes in nodeGraph0.
#'
#' @param allPaths list of every path
#' @param nodeGraph0 graph of all nodes
#' @param breakPoints breakpoint list
#' @return assigned letters to nodes in graph
#' @noRd
letterPaths <- function(allPaths, nodeGraph0, breakPoints) {
  oldVerts <- V(nodeGraph0)$name
  if (any(as.character(format(breakPoints, scientific = FALSE, trim = TRUE)) %in% names(V(nodeGraph0)))) {
    nodeGraph0 <- delete_vertices(nodeGraph0, v = as.character(format(breakPoints, scientific = FALSE, trim = TRUE)))
  }

  grIDs <- rep(NA, length(V(nodeGraph0)))
  tryCatch(
    expr = {
      dists <- distances(nodeGraph0, v = names(V(nodeGraph0)), to = names(V(nodeGraph0)), weights = E(nodeGraph0)$nodeOnlyDist)
    }, error = function(e) {
      message("Unusual amounts of interconnected paths being detected. Do you have crossed out writing in your document?")
    }
  )

  vertList <- V(nodeGraph0)$name

  grPaths <- list()
  i <- 1
  while (length(vertList) > 0) {
    tempIDs <- which(dists[which(V(nodeGraph0)$name == vertList[1]), ] < Inf)
    grPaths <- c(grPaths, list(as.numeric(V(nodeGraph0)$name[tempIDs])))
    #  grIDs[V(nodeGraph0)$name %in% as.character(grPaths[[i]])] = i
    grIDs[tempIDs] <- i
    vertList <- vertList[vertList %in% setdiff(vertList, format(grPaths[[i]], scientific = FALSE, trim = TRUE))]
    i <- i + 1
  }
  grIDs2 <- rep(NA, length(oldVerts))
  grIDs2[which(!(oldVerts %in% format(breakPoints, scientific = FALSE, trim = TRUE)))] <- grIDs

  return(list(grPaths, grIDs2))
}

organize_letters <- function(skel_graph0) {
  letters <- replicate(n = length(na.omit(unique(V(skel_graph0)$letterID))), list())
  strs <- names(V(skel_graph0))
  for (i in 1:length(na.omit(unique(V(skel_graph0)$letterID))))
  {
    tmp <- as.numeric(as.factor(V(skel_graph0)$letterID))
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

#' whichNeighbors
#'
#' Internal function for identifying which neighbors are black.
#'
#' @param coords coordinates to consider
#' @param img The image as a bitmap
#' @return Return a matrix of which neighbors are a black pixel
#' @noRd
whichNeighbors <- function(coords, img) {
  rr <- coords[1]
  cc <- coords[2]
  neighbs <- c(t(img[(rr - 1):(rr + 1), ][, (cc - 1):(cc + 1)]))[c(2, 3, 6, 9, 8, 7, 4, 1)]
  yesNeighbs <- which(neighbs == 0)
  res <- as.matrix(rep(0, 8), nrow = 1)
  res[yesNeighbs] <- 1

  return(res)
}

#' whichNeighbors0
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
whichNeighbors0 <- function(coords, img) {
  # get matrix of which neighboring pixels are black. 1=pixel is black, 0=pixel is white.
  res <- whichNeighbors(coords, img)
  
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

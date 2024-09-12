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
#' Load a handwriting sample from a PNG image. Then binarize, thin, and split
#' the handwriting into graphs.
#'
#' @param path File path for handwriting document. The document must be in PNG
#'   file format.
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
  
  if (!length(doc$process$letterList)){
    stop("The document does not contain any graphs.")
  }
  
  return(doc)
}

#' Process Handwriting by Component
#'
#' The main driver of handwriting processing. Takes in an image of thinned
#' handwriting created with [`thinImage()`] and splits the the handwriting into
#' shapes called *graphs*. Instead of processing the entire document at once,
#' the thinned writing is separated into connected components and each component
#' is split into graphs.
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
  message("Getting Nodes...")
  # create nodes. Nodes are placed in 3 locations: (1) at endpoints. These are
  # called terminal nodes and only have one connected edge, (2) at intersections. 
  # A node is placed at vertices with 3 or more connected edges, and (3) a node
  # is placed at 2x2 blocks of pixels in the thinned writing
  nodes <- getNodes(indices, dims)
  nodeList <- nodes$nodeList  # all nodes
  terminalNodes <- nodes$terminalNodes
  nodeConnections <- nodes$nodeConnections
  
  # Skeletonize writer ----
  message("Skeletonizing writing...")
  skeleton <- skeletonize(img = img, indices = indices, dims = dims, nodeList = nodeList)
  
  # Split into components ----
  message("Splitting document into components...")
  comps <- getComponents(skeleton = skeleton, img = img, dims = dims, nodes = nodes)
  
  # And merging them ----
  message("Merging nodes...")
  comps <- mergeAllNodes(comps = comps)
  
  # Finding paths ----
  message("Finding paths...")
  comps <- getPaths(comps = comps, dims = dims)
  
  # Split paths into graphs ----
  message("Split paths into graphs...")
  comps <- splitPathsIntoGraphs(comps = comps, dims = dims)
  
  # Organizing graphs ----
  message("Organizing graphs...")
  comps <- organizeGraphs(comps)
  
  # Creating graph lists ----
  message("Creating graph lists...")
  comps <- createGraphLists(comps = comps, dims = dims)

  # Adding character features ----
  message("Adding character features...")
  comps <- addGraphFeatures(comps = comps, img = img, dims = dims)
  
  # Flatten ----
  nodeList <- unique(unlist(sapply(comps, function(x) x[['nodes']][['nodeList']])))
  nodeConnections <- unique(unlist(sapply(comps, function(x) x[['nodes']][['nodeConnections']])))
  terminalNodes <- unique(unlist(sapply(comps, function(x) x[['nodes']][['terminalNodes']])))
  breakPoints <- unique(unlist(sapply(comps, function(x) x[['nodes']][['breakPoints']])))
  graphList <- flattenList(comps)
  
  # Document processing complete ----
  message("Document processing complete")
  
  return(list(nodes = nodeList, 
              connectingNodes = nodeConnections, 
              terminalNodes = terminalNodes, 
              breakPoints = sort(breakPoints), 
              letterList = graphList))
}

# Internal Functions ------------------------------------------------------

#' Find Neighbors
#'
#' The coordinates are given for a particular pixel in the image. This
#' function finds which neighboring pixels of the input pixel contain 
#' writing and which do not.
#'
#' @param coords coordinates to consider
#' @param img The image as a bitmap
#' @return Return a matrix of which neighbors contain writing
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

#' Flatten List
#' 
#' Convert a list of lists into a single 1-level list.
#'
#' @param comps A list of connected components
#'
#' @return A list
#'
#' @examples
#' flattenList(list(list('a', 'b', 'c'), list('d'), list('e', 'f')) )
#' 
#' @noRd
flattenList <- function(comps) {
  if (length(comps) > 1){
    actual <- sapply(comps, function(x) x[['paths']][['graphList']])
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
  } else {
    new <- comps[[1]][['paths']][['graphList']]
  }
  
  return(new)
}

#' Get igraph Skeleton
#'
#' Use the igraph package to create a *skeleton* graph from the thinned writing.
#' Each pixel in the image of thinned writing that contains writing is a vertex
#' in the skeleton. Two vertices / pixels are connected by an edge in the
#' skeleton if the vertices / pixels are neighbors in the image.
#'
#' The igraph skeleton in is a *graph* in the mathematical sense, but we reserve
#' the term "graph" for the small, component shapes output by
#' [`processHandwriting`]. We use the term "skeleton" to refer to igraph graphs.
#'
#' @param skeleton_df A data frame of edges in the skeleton
#' @param indices A vector of vertex / pixel indices in the image
#' @param nodeList A vector of node indices in the image
#'
#' @return An igraph graph
#'
#' @noRd
getSkeleton <- function(skeleton_df, indices, nodeList) {
  # build undirected skeleton_df with vertices=indices of the thinned writing and edges listed in skeleton_df
  skeleton <- igraph::graph_from_data_frame(d = skeleton_df, vertices = as.character(format(indices, scientific = FALSE, trim = TRUE)), directed = FALSE)
  # if more than one edge connects the same two vertices, combine the edges into a single edge and combine their attributes using the mean
  skeleton <- igraph::simplify(skeleton, remove.multiple = TRUE, edge.attr.comb = "mean")
  # color vertex as 1 if vertex is a node, otherwise color as 0
  igraph::V(skeleton)$color <- ifelse(igraph::V(skeleton)$name %in% nodeList, 1, 0)
  return(skeleton)
}

#' Associate Paths to Letters
#'
#' Creates a list of paths that are entirely contained in a letter or *graph*
#'
#' @param allPaths A list of paths to check
#' @param letter An individual letter or *graph*
#' @return A list
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

#' Get Connected Components
#'
#' Decomposed the igraph *skeleton* of thinned writing into connected
#' components. For each component, list (1) its skeleton (2)
#' the indices of the vertices in its skeleton (3) the row and column numbers of 
#' the vertices in its skeleton (4) its nodes (5) its adjacency matrix
#'
#'
#' @param skeleton An igraph *skeleton*
#' @param img A binary image of thinned handwriting created with [`thinImage`].
#' @param dims The dimensions of the image
#' @param nodes A vector of nodes
#'
#' @return A list of components
#'
#' @noRd
getComponents <- function(skeleton, img, dims, nodes) {
  initializeComponents <- function(skeletons){
    # create list of empty components
    n <- length(skeletons)
    comps <- list()
    for (i in 1:n){
      component <- sapply(c('skeletons', 'image', 'nodes', 'paths'), function(x) NULL)
      comps[[i]] <- component
    }
    return(comps)
  }
  
  addSkeletons <- function(skeletons, comps) {
    # add skeleton to each component
    n <- length(comps)
    for (i in 1:n){
      comps[[i]]$skeletons$skeleton <- skeletons[[i]]
    }
    return(comps)
  }
  
  addIndices <- function(skeletons, comps, dims) {
    # for each component in comps and each corresponding skeleton, create a
    # vector of each vertex in the skeleton and add it to the component. The
    # vertex names are the indices of the vertex / pixel in the binary image of
    # thinned writing. Also add the row and column number of each vertex to the
    # component as a data frame.
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
      comps[[i]]$skeletons$skeleton_df0 <- getSkeletonDF0(img_m=comps[[i]]$image$img_m, 
                                                          indices=comps[[i]]$image$indices, 
                                                          nodeList=comps[[i]]$nodes$nodeList, 
                                                          img=img, 
                                                          dims=dims)
      comps[[i]]$skeletons$skeleton0 <- getSkeleton(skeleton_df=comps[[i]]$skeletons$skeleton_df0, 
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
      comps[[i]]$nodes$adj0 <- getAdjMatrix(comps[[i]]$skeletons$skeleton0, comps[[i]]$nodes$nodeList)
    }
    return(comps)
  }
  
  findNeighbors0 <- function(coords, img) {
    # Internal function for identifying which neighboring pixels / vertices
    # contain writing excluding diagonals to the middle point when a
    # non-diagonal between those two vertices exists. For example, if the pixel
    # above and the pixel to the right contains writing then exclude (set to zero) the
    # pixel above and to the right (the diagonal between the two) from the
    # neighbors list
    
    # get matrix of which neighboring pixels contain writing. 1=pixel contains
    # writing, 0=pixel does not contain writing.
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
  
  # split skeleton into connected components
  skeletons <- igraph::decompose(skeleton)
  comps <- initializeComponents(skeletons = skeletons)
  comps <- addSkeletons(skeletons = skeletons, comps = comps)
  comps <- addIndices(skeletons = skeletons, comps = comps, dims = dims)
  comps <- addNodes(comps = comps, nodes = nodes)
  comps <- addSkeleton0s(comps = comps, img = img, dims = dims)
  comps <- addAdjMatrices(comps = comps)
  
  return(comps)
}

skeletonize <- function(img, indices, dims, nodeList) {
  # create skeletons and dataframe. skeleton is created first using the
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

getNodes <- function(indices, dims) {
  # Detect intersection points of an image thinned with thinImage.
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

mergeAllNodes <- function(comps) {
  findNewMergedNodes <- function(skeleton, mergeMat) {
    # mergeMat is a matrix of pairs of nodes to be merged. Each row is a single
    # pair of nodes to be merged. If the nodes are connected by a single edge,
    # pick the second node as the "new" merged node. If the nodes are connected by
    # two edges, pick the vertex between the two nodes as the new merged node.
    newNodes <- rep(NA, dim(mergeMat)[1])
    for (i in 1:dim(mergeMat)[1])
    {
      fromNode <- as.character(format(mergeMat[i, 1], scientific = FALSE, trim = TRUE))
      toNode <- as.character(format(mergeMat[i, 2], scientific = FALSE, trim = TRUE))
      path <- igraph::shortest_paths(skeleton, from = fromNode, to = toNode, weights = igraph::E(skeleton)$pen_dist)
      path_vertices <- path$vpath[[1]]
      len <- length(path_vertices)
      newNodes[i] <- as.numeric(names(path_vertices[ceiling(len / 2)]))
    }
    return(newNodes)
  }
  
  migrateConnections <- function(adj0, mergeSets, newNodes) {
    # adj0 shows the connectivity for each pair of nodes. The connectivity is 1
    # if the shortest path between two nodes does not run through another node,
    # and 0 otherwise. For each pair of nodes to be merged in mergeSets,
    # migrateConnections adds the new node to adj0. For the new node and every
    # other node in adj0, the connectivity is 1 if either of the nodes merged to
    # form the new node had connectivity with that node and 0 otherwise. The
    # nodes that were merged into new nodes are deleted from adj0. Return the
    # updated adj0 as a dataframe where each row is a pair of nodes with
    # connectivity 1. 
    
    toDelete <- NULL
    for (i in 1:dim(mergeSets)[1]) {
      # make a subset of adj0, keeping only the rows for the nodes to be merged
      whichRows <- which(rownames(adj0) %in% format(mergeSets[i, c(1, 2)], scientific = FALSE, trim = TRUE))
      subset <- matrix(adj0[whichRows, ], nrow = length(whichRows))
      # make logical vector. Each element corresponds to a node in adj0. TRUE =
      # 1 of the merge nodes is connected to the node in the corresponding
      # column. FALSE otherwise.
      newConnectivities <- apply(subset, 2, function(x) x[1] == 1 | x[2] == 1)
      # Convert to binary vector. Set any NAs to 0
      newConnectivities[is.na(newConnectivities)] <- 0
      
      # row number of new row
      toAdd <- dim(adj0)[1] + 1
      # row numbers of rows to delete
      toDelete <- c(toDelete, whichRows)
      
      # add connections for new node as the last column and row of adj0
      adj0 <- rbind(cbind(adj0, 0), 0)
      adj0[, toAdd] <- c(newConnectivities, 0)
      adj0[toAdd, ] <- c(newConnectivities, 0)
      colnames(adj0)[toAdd] <- format(newNodes[i], scientific = FALSE, trim = TRUE)
      rownames(adj0)[toAdd] <- format(newNodes[i], scientific = FALSE, trim = TRUE)
    }
    if (length(toDelete) > 0) {
      # delete merged nodes from adj0
      adj0 <- as.matrix(adj0[, -toDelete])[-toDelete, ]
    }
    return(adj0)
  }
  
  mergeNodes <- function(nodeList, skeleton0, terminalNodes, skeleton, adj0) {
    # Merge nodes that where: (1) they are connected by only one or two edges,
    # and (2) neither node is a terminal node
    
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
      
      # build matrix of pairs of nodes to merge
      rNodes <- i_to_r(nodesToMerge, length(nodeList))
      cNodes <- i_to_c(nodesToMerge, length(nodeList)) 
      mergeSets <- cbind(nodeList[rNodes], nodeList[cNodes])
      
      # Remove pairs where one or both nodes are terminal nodes. Add column:
      # 1=neither node is terminal, 0 otherwise
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
      
      newNodes <- findNewMergedNodes(skeleton, mergeSets)
      # Add the new nodes to the nodeList and remove the nodes that were merged from the list
      nodeList <- unique(c(nodeList[!(nodeList %in% c(mergeSets[, c(1, 2)]))], newNodes))
      
      # migrate connections from original nodes to new nodes
      adj0 <- migrateConnections(adj0 = adj0, mergeSets = mergeSets, newNodes = newNodes)
      
      emergencyBreak <- emergencyBreak - 1
      if (emergencyBreak == 0) {
        break
      }
    }
    
    # Create an adjacency data frame for the updated nodes
    adj.m <- adj0
    adj.m[lower.tri(adj.m)] <- 0
    adj.m <- melt(adj.m)
    adj.m <- subset(adj.m, value == 1)
    if (nrow(adj.m) > 0){
      names(adj.m) <- c("from", "to", "value")
    }
    
    return(list('nodeList'=nodeList, 'adjm'=adj.m))
  }
  
  n <- length(comps)
  for (i in 1:n){
    merged <- mergeNodes(nodeList = comps[[i]]$nodes$nodeList, 
                         skeleton0 = comps[[i]]$skeletons$skeleton0, 
                         terminalNodes = comps[[i]]$nodes$terminalNodes, 
                         skeleton = comps[[i]]$skeletons$skeleton, 
                         adj0 = comps[[i]]$nodes$adj0)
    comps[[i]]$nodes$nodeList <- merged$nodeList
    comps[[i]]$nodes$adjm <- merged$adjm
  }
  return(comps)
}
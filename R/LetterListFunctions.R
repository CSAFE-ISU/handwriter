#' MakeLetterListLetterSpecific
#'
#' Description
#' 
#' @param letterList List of letters in a handwriting sample
#' @param dims Dimensions of the handwriting sample
#' @return letterList with locations given with respect to each letter
#' 
#' @keywords ?
MakeLetterListLetterSpecific = function(letterList, dims)
{
  # NOTE: There are two ways to specify the location of the paths and nodes in a
  # letter. 1) by index number - each pixel in the letter's image (binary
  # matrix) is numbered moving top to bottom and left to right. index =
  # matrix(1:15, nrow=5, ncol=3) shows the index numbers of the pixels in a 5x3
  # image. 2) by row and column numbers - each pixel in the letter's image can
  # be referred to by its row number and column number. Rows are numbered from
  # top to bottom and columns are numbered from left to right. Much of the code
  # in this function switches between the two location types.
  
  # Make a list of the path from each letter. skeletons[[i]] contains the
  # locations (index numbers) of the path of the i-th letter.
  skeletons = lapply(letterList, function(x) x$path)
  
  # Make a list of the nodes in each letter. nodes[[i]] contains the
  # locations (index numbers) of the nodes in the i-th letter.
  nodes = lapply(letterList, function(x) x$nodes)
  
  # For each letter in the handwriting sample
  for(i in 1:length(letterList))
  { # Find locations (row and column numbers) of the path of the letter
    # relative to the handwriting sample
    path_rc = i_hs_to_rc_hs(index_nums = skeletons[[i]], hs_num_rows = dims[1])
    
    # Find the row number of the top of the letter
    letter_topmost_row = min(path_rc$row)
    
    # Find the row number of the bottom of the letter
    letter_bottom_row = max(path_rc$row)
    
    # Find the column number of the leftmost column of the letter
    letter_leftmost_col = min(path_rc$col)
    
    # Find the number of rows spanned by the letter
    letter_num_rows  = letter_bottom_row - letter_topmost_row + 1
    
    # Find the locations (row and column number) of the nodes relative to the
    # top and left side of the letter
    nodes_rc = i_hs_to_rc_letter(index_nums = nodes[[i]], 
                            hs_num_rows = dims[1], 
                            letter_topmost_row = letter_topmost_row,
                            letter_leftmost_col = letter_leftmost_col)
    
    # Find the locations (index numbers) of each path in the letter relative to the top and left
    # side of the letter
    letterList[[i]]$allPaths = lapply(letterList[[i]]$allPaths, 
                                      function(x){
                                        # Find the locations (index numbers) of the path
                                        # in the letter image
                                        i_hs_to_i_letter(index_nums = x, 
                                                    hs_num_rows = dims[1],
                                                    letter_num_rows = letter_num_rows,
                                                    letter_topmost_row = letter_topmost_row,
                                                    letter_leftmost_col = letter_leftmost_col)
                                        })
    
    # Find the locations (index numbers) of the nodes in the adjacency matrix relative to the 
    # handwriting sample
    nameVect_i = i_hs_to_i_letter(index_nums = as.numeric(colnames(letterList[[i]]$adjMatrix)), 
                           hs_num_rows = dims[1], 
                           letter_num_rows = letter_num_rows,
                           letter_topmost_row = letter_topmost_row,
                           letter_leftmost_col = letter_leftmost_col)
    
    # Change the row and column names of the adjacency matrix to the locations (index numbers)
    # of the nodes relative to the top and left of the letter
    colnames(letterList[[i]]$adjMatrix) = format(nameVect_i, scientific = FALSE, trim = TRUE)
    rownames(letterList[[i]]$adjMatrix) = colnames(letterList[[i]]$adjMatrix)
    
    # Find the locations (row and column numbers) of the letter's centroid relative to the 
    # top and left of the letter
    centroid_rc = rc_hs_to_rc_letter(row_nums = letterList[[i]]$characterFeatures$centroid_y,
                                  col_nums = letterList[[i]]$characterFeatures$centroid_x,
                                  letter_topmost_row = letter_topmost_row,
                                  letter_leftmost_col = letter_leftmost_col)
    letterList[[i]]$characterFeatures$centroid_y = centroid_rc$row
    letterList[[i]]$characterFeatures$centroid_x = centroid_rc$col
    
    # Store the locations (row and column numbers) of lHalf relative to the
    # top and left of the letter.
    lHalf_rc = i_hs_to_rc_letter(index_nums = letterList[[i]]$characterFeatures$lHalf, 
                             hs_num_rows = dims[1], 
                             letter_topmost_row = letter_topmost_row,
                             letter_leftmost_col = letter_leftmost_col)
    letterList[[i]]$characterFeatures$lHalfr = lHalf_rc$row
    letterList[[i]]$characterFeatures$lHalfc = lHalf_rc$col
    
    # Store the locations (row and column numbers) of rHalf relative to the
    # top and left of the letter.
    rHalf_rc = i_hs_to_rc_letter(index_nums = letterList[[i]]$characterFeatures$rHalf, 
                                 hs_num_rows = dims[1], 
                                 letter_topmost_row = letter_topmost_row,
                                 letter_leftmost_col = letter_leftmost_col)
    letterList[[i]]$characterFeatures$rHalfr = rHalf_rc$row
    letterList[[i]]$characterFeatures$rHalfc = rHalf_rc$col
    
    # Find the locations (row and column numbers) of the left and right centroids
    # relative to the top and left of the letter
    letterList[[i]]$characterFeatures$lCentroid = c(mean(lHalf_rc$row), mean(lHalf_rc$col))
    letterList[[i]]$characterFeatures$rCentroid = c(mean(rHalf_rc$row), mean(rHalf_rc$col))
    
    # Store the locations (index number) of lHalf and rHalf relative to the
    # top and left of the letter
    letterList[[i]]$characterFeatures$lHalf = i_hs_to_i_letter(index_nums = letterList[[i]]$characterFeatures$lHalf, 
                                                               hs_num_rows = dims[1], 
                                                               letter_num_rows = letter_num_rows,
                                                               letter_topmost_row = letter_topmost_row,
                                                               letter_leftmost_col = letter_leftmost_col)
    letterList[[i]]$characterFeatures$rHalf = i_hs_to_i_letter(index_nums = letterList[[i]]$characterFeatures$rHalf, 
                                                               hs_num_rows = dims[1], 
                                                               letter_num_rows = letter_num_rows,
                                                               letter_topmost_row = letter_topmost_row,
                                                               letter_leftmost_col = letter_leftmost_col)
    
    # Set the locations (row and column numbers) of the bottom row and rightmost column of the letter
    # relative to the top and left of the letter. Set the topmost row and leftmost column both to 1.
    bottom_right_rc = rc_hs_to_rc_letter(row_nums = letterList[[i]]$characterFeatures$bottom_row,
                                         col_nums = letterList[[i]]$characterFeatures$rightmost_col,
                                         letter_topmost_row = letter_topmost_row,
                                         letter_leftmost_col = letter_leftmost_col)
    letterList[[i]]$characterFeatures$bottom_row = bottom_right_rc$row
    letterList[[i]]$characterFeatures$rightmost_col = bottom_right_rc$col
    letterList[[i]]$characterFeatures$topmost_row = 1
    letterList[[i]]$characterFeatures$leftmost_col = 1
    
    # Set the locations (index numbers) of the path of the letter 
    # relative to the top and left of the letter
    letterList[[i]]$path = i_hs_to_i_letter(index_nums = letterList[[i]]$path,
                                            hs_num_rows = dims[1],
                                            letter_num_rows = letter_num_rows,
                                            letter_topmost_row = letter_topmost_row,
                                            letter_leftmost_col = letter_leftmost_col)
    
    # Set the locations (index numbers) of the nodes in the letter 
    # relative to the top and left of the letter
    letterList[[i]]$nodes = i_hs_to_i_letter(index_nums = letterList[[i]]$nodes,
                                            hs_num_rows = dims[1],
                                            letter_num_rows = letter_num_rows,
                                            letter_topmost_row = letter_topmost_row,
                                            letter_leftmost_col = letter_leftmost_col)
  }
  return(letterList)
}


#' MakeCenterStarts
#'
#' Select K graphs as the starting centers of the K clusters. All graphs from
#' the training handwriting samples are grouped by stratum -- the number of
#' loops if the graph has one or two loops or the number of paths in the graph.
#' Then the number of samples specified by numstrat are randomly selected from
#' each stratum group.
#'
#' @param procList List of handwriting samples
#' @param K Integer number of clusters
#' @param numPathCuts Integer number of cuts to make when comparing segments of paths
#' @param imagesList A list of graphs
#' @return centerstarts List of K graphs
#'
#' @keywords ?
#' @noRd
MakeCenterStarts = function(procList, K, numPathCuts, imagesList)
{
  # Add the numloops and stratum fields to the characterFeatures for each letter
  # in each handwriting sample. numloops is the number of loops in a letter found
  # with handwriter::loop_extract. stratum is "1loop", "2loops", or the length of
  # the letter's allPaths field
  procList = lapply(procList, function(x){x$process$letterList = AddSamplingStrata(x$process$letterList); return(x)})
  
  # Make sampling dataframe
  samplingdf <- MakeSamplingDF(procList = procList)
  
  # Make a list of K empty lists
  # centerstarts = replicate(K, list())
  # 
  # # For each letter in samplingdf, format the letter to be the starting center
  # # of a cluster
  # for(k in 1:K){
  #   # Find the index for the k-th letter
  #   letter_ind <- samplingdf$ind[k]
  #   # Cut the paths into 8 segments and find (x,y) coordinates of the pathEnds,
  #   # pathQuarters, and pathCenter with the letter's centroid at (0,0). Also find
  #   # the lengths of the paths in the letter.
  #   centerstarts[[k]] = letterToPrototype(imagesList[[letter_ind]], numPathCuts, imagesList)
  # }
  
  centerstarts = list()
  startingIndices = sample(1:length(procList), K, replace = FALSE)
  for(j in 1:K) {
    centerstarts[[j]] = letterToPrototype(procList[[startingIndices[j]]], numPathCuts, imagesList)
  }
  
  return(centerstarts)
}


#' AddSamplingStrata
#'
#' Add two new fields, numloops and stratum, to the characterFeatures list for
#' each letter in letterList.
#'
#' @param letterList List of letters
#' @return List of letters
#'
#' @keywords ?
AddSamplingStrata = function(letterList){
  # For each letter in the sample
  for(i in 1:length(letterList)){
    # Count the number of loops in the letter and store as a new field called numloops under characterFields
    letterList[[i]]$characterFeatures$numloops = length(loop_extract(letterList[[i]]$allPaths))
    # Add a new field called stratum under characterFields, where stratum is "1loop", "2loops", or the length of allPaths
    if(letterList[[i]]$characterFeatures$numloops == 2){
      letterList[[i]]$characterFeatures$stratum = "2loop"
    } else if(letterList[[i]]$characterFeatures$numloops == 1){
      letterList[[i]]$characterFeatures$stratum = "1loop"
    } else {
      letterList[[i]]$characterFeatures$stratum = length(letterList[[i]]$allPaths)
    }
  }
  return(letterList)
}


#' MakeSamplingDF
#'
#' Group each letter in procList by stratum -- the number of loops if the letter has one or two loops
#' or the number of paths in the letter -- and randomly sample letters from each group. The numstrat
#' parameter sets the number of letters to sample from each group.
#'
#' @param procList List of handwriting samples
#' @return Dataframe
#'
#' @keywords ?
MakeSamplingDF <- function(procList)
{ # Initialize
  doc = letter = stratum_a = c()
  
  # Create a vector called doc that numbers each handwriting sample. Create a
  # vector called letter that numbers each letter within a handwriting sample.
  # Create a vector called stratum_a that shows the number of loops if the
  # letter has one or two loops and otherwise shows the number of paths in the
  # letter
  for(i in 1:length(procList)){
    # For each letter in handwriting sample
    for(j in 1:length(procList[[i]]$process$letterList)){
      doc = c(doc,i)
      letter = c(letter,j)
      stratum_a = c(stratum_a, procList[[i]]$process$letterList[[j]]$characterFeatures$stratum)
    }
  }
  
  # Make a vector of unique values in stratum_a. Make "1loop" and "2loop" the first two entries.
  lvls = c("1loop", "2loop", sort(as.numeric(unique(stratum_a[!(stratum_a %in% c("1loop", "2loop"))]))))
  
  # Number of strat to sample at each level. Pad vector with zeros to make the same length as levels.
  if (length(lvls) > 17){
    numstrat = c(5, 2, 5, 6, 5, 3, 2, 2, 2, rep(1,8), rep(0, length(lvls)-17))
  } else if (length(lvls)==17) {
    numstrat = c(5, 2, 5, 6, 5, 3, 2, 2, 2, rep(1,8))
  } else {
    numstrat = c(5, 2, 5, 6, 5, 3, 2, 2, 2, rep(1,8))
    numstrat = numstrat[1:length(lvls)]
  }
  
  # Make the dataframe
  samplingdf = data.frame(doc = doc, letter = letter, stratum = stratum_a, ind = 1:length(stratum_a))
  
  # Randomly select graphs by stratum level
  samplingdf = samplingdf %>%
    mutate(stratumfac = factor(stratum, levels = lvls)) %>%  # convert to factor
    group_by(stratumfac) %>%
    nest() %>%  # make nested dataframe for each factor level            
    ungroup() %>% 
    arrange(stratumfac) %>%  # sort by factor levels
    mutate(n = numstrat) %>%  # add column with number to sample from each nested dataframe
    mutate(samp = purrr::map2(data, n, dplyr::sample_n)) %>%  # sample n graphs from each nested dataframe
    select(-data) %>%  # Remove old rows
    unnest(samp)  # Keep sampled rows
  
  return(samplingdf)
  
}


#' loop_extract
#'
#' Iterates through all available paths from processHandwriting()
#' Picks out loops for later character association.
#' 
#' @param allPaths All character (formerly letter) paths from processHandwriting()
#' @keywords character loops line
#' 
#' @return List of all loops 
loop_extract = function(allPaths){
  loops = list()
  for(i in 1:length(allPaths)){
    if(length(allPaths)<1){
      next
    }
    if(allPaths[[i]][[1]]==allPaths[[i]][[length(allPaths[[i]])]]){
      loops = c(loops,list(allPaths[[i]]))
    }
  }
  return(loops)
}


#' centeredImage
#'
#' Find the letter's centroid and proptroid relative to the bottom left corner of the
#' letter's image.
#'
#' @param letter A letter
#' @return a named list with fields nodes, centroid, proptroid, image and allPaths
#'
#' @keywords ?
centeredImage = function(letter)
{
  res = list()
  res$nodes = letter$nodes
  # Find the location (column and row numbers) of the centroid relative
  # to the bottom left of the lettter image. Like (x,y) coordinates with 
  # (0,0) in the bottom left corner of the image.
  res$centroid = round(c(letter$characterFeatures$centroid_x, letter$characterFeatures$height - letter$characterFeatures$centroid_y + 1))
  # Calculate the proptroid
  res$proptroid = c(letter$characterFeatures$centroid_x, letter$characterFeatures$height - letter$characterFeatures$centroid_y + 1)/c(letter$characterFeatures$width, letter$characterFeatures$height)
  res$image = letter$image
  res$allPaths = letter$allPaths
  return(res)
}

#' centeredImageOnCentroid
#'
#' Place the letter's centroid at (0,0) and find the locations (column and row
#' numbers) of a letter's paths and path ends relative to the letter's centroid.
#'
#' @param letter A letter from a handwriting sample
#' @return The letter with it's paths and path ends locations given relative to the 
#' letter's centroid.
#'
#' @keywords ?
centeredImageOnCentroid <- function(letter){
  # column number of node from the left of the letter image
  nodes_c = ((letter$nodes-1) %/% dim(letter$image)[1]) + 1
  # row number of node from the bottom of the letter image
  nodes_r = dim(letter$image)[1] - ((letter$nodes-1) %% dim(letter$image)[1])
  # Make a matrix of column and row pairs. Like (x,y) coordinates with 
  # (0,0) in the bottom left corner of the letter image
  letter$nodesrc = cbind(nodes_c, nodes_r)
  # Shift the nodes so that the origin (0,0) is on the centroid. Do this by subtracting the centroid from 
  # each node. 
  letter$nodesrc = letter$nodesrc - matrix(rep(letter$centroid, each = dim(letter$nodesrc)[1]), ncol = 2)
  # Find the ends locations (column and row numbers) of the ends of each relative to the bottom left corner
  # of the letter image
  letter$pathEndsrc = lapply(letter$allPaths, function(z){cbind(((z[c(1, length(z))]-1) %/% dim(letter$image)[1]) + 1, dim(letter$image)[1] - ((z[c(1,length(z))]-1) %% dim(letter$image)[1]))})
  # Shift the path ends so that the origin (0,0) is on the centroid. Do this by subtracting the centroid from 
  # each set of path ends.
  letter$pathEndsrc = lapply(letter$pathEndsrc, function(z){z - matrix(rep(letter$centroid, each = 2), ncol = 2)})
  return(letter)
}


#' i_to_r
#'
#' Convert index number location in a matrix to 
#' the row number location in that matrix.
#' 
#' @param index_num Numeric Index number location in matrix
#' @param num_rows Number of rows in matrix
#' @return Row number location in matrix
#' 
#' @keywords ?
i_to_r = function(index_num, num_rows){
  r = ((index_num-1) %% num_rows) + 1
  rc[['col']] = ((index_num-1) %/% hs_num_rows) + 1
  return(r)
}


#' i_to_c
#'
#' Convert index number location in a matrix to 
#' the column number location in that matrix.
#' 
#' @param index_num Numeric Index number location in matrix
#' @param num_rows Number of rows in matrix
#' @return Column number location in matrix
#' 
#' @keywords ?
i_to_c = function(index_num, num_rows){
  c = ((index_num-1) %/% num_rows) + 1
  return(c)
}


#' i_to_x
#'
#' Convert index number location in a matrix to 
#' the x-coordinate location in that matrix where (0,0)
#' is the bottom left corner of the matrix
#' 
#' @param index_num Numeric Index number location in matrix
#' @param num_rows Number of rows in matrix
#' @return x-coordinate location in matrix
#' 
#' @keywords ?
i_to_x = function(index_num, num_rows){
  x = ((index_num-1) %/% num_rows) + 1
  return(x)
}


#' i_to_y
#'
#' Convert index number location in a matrix to 
#' the y-coordinate location in that matrix where (0,0)
#' is the bottom left corner of the matrix
#' 
#' @param index_num Numeric Index number location in matrix
#' @param num_rows Number of rows in matrix
#' @return y-coordinate location in matrix
#' 
#' @keywords ?
i_to_y = function(index_num, num_rows){
  y = num_rows - (index_num-1) %% num_rows
  return(y)
}


#' i_hs_to_rc_hs
#'
#' Convert index number(s) locations in a handwriting sample to 
#' the row and column number(s) locations in that handwriting sample.
#' 
#' @param index_nums Vector of index number(s) in handwriting sample
#' @param hs_num_rows Number of rows in handwriting sample
#' @return List of row and column number(s) locations in handwriting sample
#' 
#' @keywords ?
i_hs_to_rc_hs = function(index_nums, hs_num_rows){
  rc = list()
  rc[['row']] = ((index_nums-1) %% hs_num_rows) + 1
  rc[['col']] = ((index_nums-1) %/% hs_num_rows) + 1
  return(rc)
}


#' i_hs_to_rc_letter
#'
#' Convert index number(s) locations in a handwriting sample to 
#' the row and column number(s) locations in a letter in that 
#' handwriting sample.
#' 
#' @param index_nums Vector of index number(s) in handwriting sample
#' @param hs_num_rows Integer number of rows in handwriting sample
#' @param letter_topmost_row Integer number of the top row of letter
#' @param letter_leftmost_col Integer number of the left column of letter
#' @return List of row and column number(s) locations in letter
#' 
#' @keywords ?
i_hs_to_rc_letter = function(index_nums, hs_num_rows, letter_topmost_row, letter_leftmost_col){
  rc = list()
  
  # Find locations (row and column numbers) in handwriting sample
  rc = i_hs_to_rc_hs(index_nums = index_nums, hs_num_rows = hs_num_rows)
  
  # Find locations (row and column numbers) in letter
  rc[['row']] <- rc$row - letter_topmost_row + 1
  rc[['col']] <- rc$col - letter_leftmost_col + 1
  return(rc)
}


#' i_hs_to_i_hs
#'
#' Convert index number(s) locations in a handwriting sample to 
#' index number(s) locations in a letter in that handwriting sample.
#' 
#' @param index_nums Vector of index number(s) in handwriting sample
#' @param hs_num_rows Integer number of rows in handwriting sample
#' @param letter_num_rows Integer number of rows in letter
#' @param letter_topmost_row Integer number of the top row of letter
#' @param letter_leftmost_col Integer number of the left column of letter
#' @return Integer index number(s) locations in letter
#' 
#' @keywords ?
i_hs_to_i_letter = function(index_nums, hs_num_rows, letter_num_rows, letter_topmost_row, letter_leftmost_col){
  
  # Find locations (row and column numbers) in letter
  rc <- i_hs_to_rc_letter(index_nums, hs_num_rows, letter_topmost_row, letter_leftmost_col)
  
  # Find the locations (index numbers) in letter
  i <- rc$row + (rc$col - 1)*letter_num_rows
  
  return(i)
}


#' rc_hs_to_rc_letter
#'
#' Convert row and column number(s) locations in a handwriting sample to 
#' row and column number(s) locations in a letter in that handwriting sample.
#' 
#' @param row_nums Integer row numbers in handwriting sample
#' @param col_nums Integer column numbers in handwriting sample
#' @param letter_topmost_row Integer number of the top row of letter
#' @param letter_leftmost_col Integer number of the left column of letter
#' @return List of row and column number(s) locations in letter
#' 
#' @keywords ?
rc_hs_to_rc_letter = function(row_nums, col_nums, letter_topmost_row, letter_leftmost_col){
  rc = list()
  
  # Find locations (row and column numbers) in letter
  rc[['row']] <- row_nums - letter_topmost_row + 1
  rc[['col']] <- col_nums - letter_leftmost_col + 1
  return(rc)
}

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


# Internal Functions ------------------------------------------------------


#' MakeLetterListLetterSpecific
#'
#' Description
#' 
#' @param letterList List of letters in a handwriting sample
#' @param dims Dimensions of the handwriting sample
#' @return letterList with locations given with respect to each letter
#' @noRd
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

#' loop_extract
#'
#' Iterates through all available paths from processHandwriting()
#' Picks out loops for later character association.
#' 
#' @param allPaths All character (formerly letter) paths from processHandwriting()
#' 
#' @return List of loops
#' 
#' @noRd
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
#' @noRd
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
#' @noRd
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

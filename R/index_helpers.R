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


#' i_to_r
#'
#' Convert index number location in a matrix to 
#' the row number location in that matrix.
#' 
#' @param index_num Numeric Index number location in matrix
#' @param num_rows Number of rows in matrix
#' @return Row number location in matrix
#' 
#' @noRd
i_to_r = function(index_num, num_rows){
  r = ((index_num-1) %% num_rows) + 1
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
#' @noRd
i_to_c = function(index_num, num_rows){
  c = ((index_num-1) %/% num_rows) + 1
  return(c)
}

#' i_to_rc
#'
#' Function for converting indices to respective row, col.
#' 
#' @param nodes nodes to be converted.
#' @param dims dimensions of binary image
#' @return returns matrix mapping nodes to respective row, 
#' @noRd
i_to_rc = function(nodes, dims)
{
  cs = (nodes-1)%/%dims[1] + 1
  rs = (nodes-1)%%dims[1] + 1
  return(matrix(c(rs,cs), ncol = 2))
}

#' i_to_rci
#'
#' Function for converting indices to respective row, col and associates the original index.
#' 
#' @param nodes nodes to be converted.
#' @param dims dimensions of binary image
#' @param fixed instead of normal computation of rows, put it in a fixed location.
#' @return returns matrix mapping nodes' indices to respective row, col
#' @noRd
i_to_rci = function(nodes, dims, fixed = FALSE)
{
  cs = (nodes-1)%/%dims[1] + 1
  rs = (nodes-1)%%dims[1] + 1
  if(fixed) rs = dims[1] - rs + 1
  rowcolmatrix = cbind(rs,cs,nodes)
  colnames(rowcolmatrix) = c('y','x','index')
  return(rowcolmatrix)
}

#' i_hs_to_rc_hs
#'
#' Convert index number(s) locations in a handwriting sample to 
#' the row and column number(s) locations in that handwriting sample.
#' 
#' @param index_nums Vector of index number(s) in handwriting sample
#' @param hs_num_rows Number of rows in handwriting sample
#' @return List of row and column number(s) locations in handwriting sample
#' @noRd
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
#' @noRd
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
#' @noRd
i_hs_to_i_letter = function(index_nums, hs_num_rows, letter_num_rows, letter_topmost_row, letter_leftmost_col){
  
  # Find locations (row and column numbers) in letter
  rc <- i_hs_to_rc_letter(index_nums, hs_num_rows, letter_topmost_row, letter_leftmost_col)
  
  # Find the locations (index numbers) in letter
  i <- rc$row + (rc$col - 1)*letter_num_rows
  
  return(i)
}

#' rc_to_i
#'
#' Convert rows and columns to their respective indices.
#' This is index sensitive, so row_y[[1]] should correspond to col_x[[1]]
#' 
#' @param row_y Row(s) to be converted to an index
#' @param col_x Columns(s) to be converted to an index
#' @param dims Dimensions of binary image
#' @param fixed Logical value asking if row_y is fixed to a point.
#' @return Returns index(icies) of all row_y's and col_x's
#' @noRd
rc_to_i = function(row_y,col_x,dims, fixed = FALSE)
{
  row_y = as.integer(row_y)
  if(fixed) row_y = dims[1] - row_y + 1
  col_x = as.integer(col_x)
  return((col_x-1)*dims[1]+row_y)
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
#' @noRd
rc_hs_to_rc_letter = function(row_nums, col_nums, letter_topmost_row, letter_leftmost_col){
  rc = list()
  
  # Find locations (row and column numbers) in letter
  rc[['row']] <- row_nums - letter_topmost_row + 1
  rc[['col']] <- col_nums - letter_leftmost_col + 1
  return(rc)
}

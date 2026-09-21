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


# Exported ----------------------------------------------------------------

#' Extract CSAFE writer IDs
#'
#' Extract writer IDs from CSAFE Handwriting Database document names or file
#' paths. Writer IDs are the first five characters of each file name (e.g.,
#' "w0001").
#'
#' @param x A character vector of CSAFE document names, file names, or full
#'   file paths, such as "w0001_s01_pLND_r01.png" or
#'   "path/to/w0001_s01_pLND_r01.png".
#' @param only_unique Logical. If `TRUE`, return only unique writer IDs.
#'   Defaults to `FALSE`.
#' @param as_integer Logical. If `TRUE`, remove the leading "w" and return the
#'   writer IDs as integers. Defaults to `FALSE`.
#'
#' @return A character vector of writer IDs, or an integer vector if
#'   `as_integer = TRUE`.
#'
#' @examples
#' docs <- c("path/to/w0001_s01_pLND_r01.png", "w0001_s02_pWOZ_r01.png",
#'           "w0238_s01_pPHR_r02.png")
#' get_csafe_writerIDs(docs)
#' get_csafe_writerIDs(docs, only_unique = TRUE)
#' get_csafe_writerIDs(docs, only_unique = TRUE, as_integer = TRUE)
#'
#' @export
get_csafe_writerIDs <- function(x, only_unique = FALSE, as_integer = FALSE) {
  writers <- substr(basename(x), 1, 5)
  
  if (only_unique) {
    writers <- unique(writers)
  }
  
  if (as_integer) {
    writers <- as.integer(stringr::str_replace(writers, "w", ""))
  }
  
  return(writers)
}

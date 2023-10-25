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


# EXPORTED ----------------------------------------------------------------

#' Plot Cluster Template
#' 
#' Plot the graphs and cluster center in each cluster of a cluster template.
#'
#' @param template A cluster template created with [`make_clustering_templates()`]
#'
#' @return A list of plots
#' @export
#'
#' @examples
#' clusters <- plotClusterTemplate(example_cluster_template)
#' # show plot of first cluster
#' clusters[[1]]
#' # show plot of second cluster
#' clusters[[2]]
#' 
#' @md
plotClusterTemplate <- function(template) {
  # Set variable to NULL to fix no visible binding NOTE
  Var1 <- Var2 <- value <- NULL
  
  # list template graph images
  all_images <- lapply(template$template_graphs, function(x) x$image)
  
  # find dimensions of largest image
  dims <- lapply(all_images, function(x) dim(x))
  pad_row <- max(sapply(dims, function(x) x[1]))
  pad_col <- max(sapply(dims, function(x) x[2]))
  
  # for each cluster
  plots <- list()
  for (i in 1:template$K){
    cluster <- all_images[template$cluster == i]
    
    # format cluster center graph
    center <- all_images[[template$centers_graph_ids[i]]]
    center <- switch_zeros_and_ones(center)
    center_padded <- pad_image(center, pad_row, pad_col)
    
    master_image <- matrix(0, nrow=pad_row, ncol=pad_col)
    for (j in 1:length(cluster)){
      image <- cluster[[j]]
      image <- switch_zeros_and_ones(image)
      image_padded <- pad_image(image, pad_row, pad_col)
      master_image = master_image + image_padded
    }
    # set center graph to NA in master image
    master_image[center_padded == 1] <- NA
    
    # change matrix to data frame for use with ggplot
    df <- reshape2::melt(master_image)
    
    # plot
    p <- df %>% 
      ggplot2::ggplot(ggplot2::aes(Var2, rev(Var1))) +
      ggplot2::geom_raster(ggplot2::aes(fill = value)) +
      ggplot2::scale_fill_gradient(low="white", high="grey60", guide="none", na.value = "red") +
      ggplot2::theme_void()
    plots[[i]] <- p
  }
  return(plots)
}


# Internal Functions ------------------------------------------------------

#' Pad Image with Zeros
#' 
#' Pad a black-and-white (binary) image with zeros so that the original image is 
#' centered in the padded image.
#'
#' @param image A black-and-white image represented as a matrix
#' @param pad_row An integer number of rows in the padded image
#' @param pad_col An integer number of columns in the padded image
#'
#' @return A matrix
#'
#' @examples
#' # extract image from example cluster template
#' image <- example_cluster_template$template_graphs[[1]]$image
#' pad_image(image, 20, 20)
#' 
#' @noRd
pad_image <- function(image, pad_row, pad_col) {
  n <- dim(image)[1]
  m <- dim(image)[2]
  center_row = pad_row %/% 2  # floor division
  center_col = pad_col %/% 2  # floor division
  start_row <- center_row - (n %/% 2) + 1
  start_col <- center_col - (m %/% 2) + 1
  end_row <- start_row + n - 1
  end_col <- start_col + m - 1
  image_padded <- matrix(0, nrow=pad_row, ncol=pad_col)
  image_padded[start_row:end_row, start_col:end_col] <- image
  return(image_padded)
}


#' Switch Zeros and Ones
#' 
#' In a black-and-white (binary) image, switch the zeros to ones and the ones to zeros.
#'
#' @param image A black-and-white image represented in a matrix
#'
#' @return A matrix
#'
#' @examples
#' image <- example_cluster_template$template_graphs[[1]]$image
#' image <- switch_zeros_and_ones(image)
#' 
#' @noRd
switch_zeros_and_ones <- function(image){
  image <- +(!image)
  return(image)
}

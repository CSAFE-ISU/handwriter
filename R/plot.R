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


#' Plot Image
#'
#' This function plots a basic black and white image.
#' 
#' @param doc A document processed with [`processDocument()`] or a binary matrix (all entries are 0 or 1)
#' @return ggplot plot
#' 
#' @examples
#' csafe_document <- list()
#' csafe_document$image <- csafe
#' plotImage(csafe_document)
#' 
#' \dontrun{
#' document <- processDocument('path/to/image.png')
#' plotImage(document)
#' }
#' 
#' @export
#' @md
plotImage = function(doc)
{
  Var2 <- Var1 <- value <- NULL
  if ('image' %in% names(doc)){
    xm = melt(doc$image)
  } else {
    xm = melt(doc)
  }
  names(xm) = c("Var1", "Var2", "value")
  p = ggplot2::ggplot(xm, ggplot2::aes(Var2, rev(Var1))) + 
    ggplot2::geom_raster(aes(fill = as.factor(value)), na.rm=TRUE) + 
    ggplot2::scale_fill_manual(values = c("black", "white"), guide = "none") + 
    ggplot2::coord_fixed() + 
    ggplot2::theme_void()
  return(p)
}

#' Plot Thinned Image
#'
#' This function returns a plot with the full image plotted in light gray and the thinned skeleton printed in black on top.
#' 
#' @param doc A document processed with [`processHandwriting()`]
#' @return gpplot plot of thinned image
#' 
#' @examples
#' csafe_document <- list()
#' csafe_document$image <- csafe
#' csafe_document$thin <- thinImage(csafe_document$image)
#' plotImageThinned(csafe_document)
#' 
#' @export
#' @md
plotImageThinned = function(doc)
{
  Var2 <- Var1 <- value <- NULL
  # melt converts matrix to dataframe with 3 columns. "Var1" column denotes
  # a row number in the image. "Var2" is a column number in the image. "value"
  # column gives the matrix value in the "Var1" row and "Var2" column.
  l.m = melt(doc$image)
  names(l.m) = c("Var1", "Var2", "value")
  l.m$value[doc$thin] = 2
  p = ggplot2::ggplot(l.m, ggplot2::aes(Var2, rev(Var1))) + ggplot2::geom_raster(ggplot2::aes(fill = as.factor(value), alpha = as.factor(value)), na.rm=TRUE) + ggplot2::scale_alpha_manual(values = c(.1, NA, 1), guide = "none") + ggplot2::scale_fill_manual(values = c("black", "white", "black"), guide = "none") + ggplot2::coord_fixed() + ggplot2::theme_void()
  return(p)
}

#' Plot Nodes
#'
#' This function returns a plot with the full image plotted in light gray and the skeleton printed in black, with red triangles over the vertices.
#' Also called from plotPath, which is a more useful function, in general.
#' 
#' @param doc A document processed with [`processHandwriting()`]
#' @param plot_break_pts Logical value as to whether to plot nodes or break points. plot_break_pts=FALSE plots nodes and plot_break_pts=TRUE plots break point.
#' @param nodeSize Size of triangles printed. 3 by default. Move down to 2 or 1 for small text images.
#' @param nodeColor Which color the nodes should be
#' @return Plot of full and thinned image with vertices overlaid.
#' 
#' @examples
#' csafe_document <- list()
#' csafe_document$image <- csafe
#' csafe_document$thin <- thinImage(csafe_document$image)
#' csafe_document$process <- processHandwriting(csafe_document$thin, dim(csafe_document$image))
#' plotNodes(csafe_document)
#' plotNodes(csafe_document, nodeSize=6, nodeColor="black")
#' 
#' @import ggplot2
#' 
#' @export
#' @md
plotNodes = function(doc, plot_break_pts = FALSE, nodeSize = 3, nodeColor = "red")
{
  X <- Y <- NULL
  
  p = plotImageThinned(doc)
  if (plot_break_pts){
    nodeList <- doc$process$breakPoints
  } else {
    nodeList <- doc$process$nodes
  }
  
  pointSet = data.frame(X = ((nodeList - 1) %/% dim(doc$image)[1]) + 1, Y = dim(doc$image)[1] - ((nodeList - 1) %% dim(doc$image)[1]))
  p = p + ggplot2::geom_point(data = pointSet, ggplot2::aes(X, Y), size = nodeSize, shape = I(16), color = I(nodeColor), alpha = I(.4))
  return(p)
}

#' Plot Line
#'
#' This function returns a plot of a single line extracted from a document. 
#' It uses the letterList parameter from the processHandwriting function and accepts a single value as whichLetter. 
#' Dims requires the dimensions of the entire document, since this isn't contained in processHandwriting.
#' 
#' @param letterList Letter list from processHandwriting function
#' @param whichLine Single value denoting which line to plot - checked if too big inside function.
#' @param dims Dimensions of the original document
#' @return ggplot plot of single line
#' 
#' @examples
#' twoSent_document = list()
#' twoSent_document$image = twoSent
#' twoSent_document$thin = thinImage(twoSent_document$image)
#' twoSent_processList = processHandwriting(twoSent_document$thin, dim(twoSent_document$image))
#' 
#' dims = dim(twoSent_document$image)
#' plotLine(twoSent_processList$letterList, 1, dims)
#' 
#' @import ggplot2
#' @export
#' @md
plotLine = function(letterList, whichLine, dims)
{
  pathList = list()
  letterListIndex = list()
  #stitch all paths together
  count = 1
  for(i in letterList){
    
    lineNumber = i$characterFeatures$line_number
    if(lineNumber == whichLine)
    {
      pathList <- append(pathList, i$path)
      letterListIndex <- append(letterListIndex, count)
    }
    
    count = count + 1
  }
  
  #if nothing was found on that line, just exit out because it is too big (or small)
  if (length(pathList) == 0){
    #print("ERROR: no letters found on that path - valid lines are 1:max")
    stop("ERROR: no letters found on that path - valid lines are 1:max")
  }
  
  pathVec <- unlist(pathList)
  countVec <- unlist(letterListIndex)
  
  r = ((pathVec-1) %% dims[1]) + 1
  c = ((pathVec-1) %/% dims[1]) + 1
  
  img = matrix(1, nrow = diff(range(r))+1, ncol = diff(range(c))+1)
  
  nodeList = list()
  for(i in letterList[c(countVec)]){
    nodes = i$nodes
    nodesr = ((nodes-1) %% dims[1]) + 1
    nodesc = ((nodes-1) %/% dims[1]) + 1
    nodesr = nodesr - min(r) + 1
    nodesc = nodesc - min(c) + 1
    
    nodes = ((nodesc - 1)*(diff(range(r))+1)) + nodesr
    nodeList <- append(nodeList, nodes)
  }
  
  nodeList <- unlist(nodeList)
  
  rnew = r-min(r)+1
  cnew = c-min(c)+1
  
  img[cbind(rnew,cnew)] = 0
  
  #Plot line
  p = plotImage(img)
  
  #plot nodes
  nodeSize = 3 
  nodeColor = "red"
  pointSet = data.frame(X = ((nodeList - 1) %/% dim(img)[1]) + 1, Y = dim(img)[1] - ((nodeList - 1) %% dim(img)[1]))
  #p = p + geom_point(data = pointSet, aes(X, Y), size = nodeSize, shape = I(16), color = I(nodeColor), alpha = I(.4))
  
  return(p)
}

#' Plot Letter
#'
#' This function returns a plot of a single graph extracted from a document. It
#' uses the letterList parameter from the [`processHandwriting()`] or [`processDocument()`] function and
#' accepts a single value as `whichLetter`. Dims requires the dimensions of the
#' entire document, since this isn't contained in [`processHandwriting()`] or [`processDocument()`].
#'
#' @param doc A document processed with [`processHandwriting()`] or [`processDocument()`]
#' @param whichLetter Single value in 1:length(letterList) denoting which letter
#'   to plot.
#' @param showPaths Whether the calculated paths on the letter should be shown
#'   with numbers.
#' @param showCentroid Whether the centroid should be shown
#' @param showSlope Whether the slope should be shown
#' @param showNodes Whether the nodes should be shown
#' @return Plot of single letter.
#'
#' @examples
#' twoSent_document = list()
#' twoSent_document$image = twoSent
#' twoSent_document$thin = thinImage(twoSent_document$image)
#' twoSent_document$process = processHandwriting(twoSent_document$thin, dim(twoSent_document$image))
#' plotLetter(twoSent_document, 1)
#' plotLetter(twoSent_document, 4, showPaths = FALSE)
#'
#' @export
#' @md
plotLetter = function(doc, whichLetter, showPaths = TRUE, showCentroid = TRUE, showSlope = TRUE, showNodes = TRUE)
{
  X <- Y <- NULL
  
  # dimensions of original image
  dims <- dim(doc$image)
  
  path = doc$process$letterList[[whichLetter]]$path
  # convert index to row and column
  rc <- i_to_rc(path, dims)
  r = rc[,1]
  c = rc[,2]
  
  # matrix of ones
  img = matrix(1, nrow = diff(range(r))+1, ncol = diff(range(c))+1)
  
  nodes = doc$process$letterList[[whichLetter]]$nodes
  # convert index to row and column
  nodesrc = i_to_rc(nodes, dims)
  nodesr = nodesrc[,1]
  nodesc = nodesrc[,2]
  nodesr = nodesr - min(r) + 1
  nodesc = nodesc - min(c) + 1
  
  rnew = r-min(r)+1
  cnew = c-min(c)+1
  nodes = ((nodesc - 1)*(diff(range(r))+1)) + nodesr
  img[cbind(rnew,cnew)] = 0
  
  # format like output of processDocument so that we can use plotNodes() or plotImageThinned()
  img_doc <- list()
  img_doc$image <- img
  img_doc$thin <- which(img == 1)
  img_doc$process$nodes <- nodes
  
  if (showNodes){
    p = plotNodes(img_doc)
  } else {
    p = plotImageThinned(img_doc)
  }
  
  #End of plotting the Nodes, 
  #Start finding info for optional features to display
  centroid_y = doc$process$letterList[[whichLetter]]$characterFeatures$centroid_y - min(r) + 1
  centroid_x = doc$process$letterList[[whichLetter]]$characterFeatures$centroid_x - min(c) + 1
  # convert index to row and column
  lHalfrc <- i_to_rc(doc$process$letterList[[whichLetter]]$characterFeatures$lHalf, dims)
  lHalfr = lHalfrc[,1]
  lHalfc = lHalfrc[,2]
  rHalfrc <- i_to_rc(doc$process$letterList[[whichLetter]]$characterFeatures$rHalf, dims)
  rHalfr = rHalfrc[,1]
  rHalfc = rHalfrc[,2]
  
  lHalfr = lHalfr - min(r) + 1
  lHalfc = lHalfc - min(c) + 1
  rHalfr = rHalfr - min(r) + 1
  rHalfc = rHalfc - min(c) + 1
  
  lCentroid = c(mean(lHalfr), mean(lHalfc))
  rCentroid = c(mean(rHalfr), mean(rHalfc))
  
  ranger = doc$process$letterList[[whichLetter]]$characterFeatures$height
  rangec = doc$process$letterList[[whichLetter]]$characterFeatures$width
  
  centroidDat = data.frame(X = centroid_x, 
                           Y = ranger - centroid_y + 1)
  halfCentroidDat = data.frame(X = c(lCentroid[2], rCentroid[2]), 
                               Y = c(ranger - c(lCentroid[1], rCentroid[1]) + 1))
  tightnessDat = data.frame(x0 = centroid_x, y0 = ranger - centroid_y + 1)
  tightness = doc$process$letterList[[whichLetter]]$characterFeatures$compactness
  
  pathPoints = NULL
  pathSets = doc$process$letterList[[whichLetter]]$allPaths
  for(i in 1:length(pathSets))
  {
    pathr = ((pathSets[[i]]-1) %% dims[1]) + 1
    pathr = pathr - min(r) + 1
    pathc = ((pathSets[[i]]-1) %/% dims[1]) + 1
    pathc = pathc - min(c) + 1
    
    pathPoints = rbind(pathPoints, cbind(pathr, pathc, i))
  }
  
  #Plot paths as numbers
  if (showPaths){
    p = p + ggplot2::geom_text(data = as.data.frame(pathPoints), ggplot2::aes(x = pathc, y = max(rnew) - pathr + 1, label = i))
  }
  
  #Plot Centroid
  if (showCentroid){
    p = p + ggplot2::geom_point(data = centroidDat, aes(x = X, y = Y, color = I("red"), size = I(3), shape = I(7)))
  }
  
  #Plot Slope of Letter
  if (showSlope){
    p = p + ggplot2::geom_point(data = halfCentroidDat, ggplot2::aes(x = X, y = Y, color = I("red"), shape = I(4))) + 
      ggplot2::geom_line(data = halfCentroidDat, ggplot2::aes(x = X, y = Y, color = I("red")))
  }
  
  return(p)
}

#' Plot Graphs
#'
#' @param doc A PNG image of handwriting processed with [`processDocument`].
#'
#' @return A plot of all graphs in the document
#' 
#' @export
#'
#' @examples
#' image_path <- system.file("extdata", "phrase_example.png", package = "handwriter")
#' doc <- processDocument(image_path)
#' plot_graphs(doc)
#' 
plot_graphs <- function(doc) {
  format_graph_df <- function(graph, i) {
    # bind global variables to fix check() note
    Var1 <- NULL
    
    # reshape the image matrix as a data frame where Var1 is the matrix row,
    # Var2 is the matrix column and value is the matrix value in that row and
    # column
    df <- melt(graph$image)
    
    # flip row number to represent y coordinate (Rows are indexed top to bottom,
    # non-negative y values are plotted bottom to top)
    df <- df %>% dplyr::mutate(Var1 = max(Var1) - Var1 + 1)
    
    colnames(df) <- c("y", "x", "value")
    
    # add graph number for facet_wrap
    df$graph <- i
    
    return(df)
  }
  
  # bind global variables to fix check() note
  x <- y <- value <- NULL
  
  letterList <- AddLetterImages(doc$process$letterList, dim(doc$image))
  
  dfs <- lapply(1:length(letterList), function(i) format_graph_df(letterList[[i]], i))
  df <- do.call(rbind, dfs)
  
  p <- df %>% 
    ggplot2::ggplot(ggplot2::aes(x, y)) + 
    ggplot2::geom_raster(aes(fill = as.factor(value)), na.rm=TRUE) + 
    ggplot2::scale_fill_manual(values = c("black", "white"), guide = "none") + 
    ggplot2::coord_fixed() + 
    ggplot2::theme_void() +
    ggplot2::facet_wrap(~graph)
  
  return(p)
}


# Internal Functions ------------------------------------------------------

#' Add Letter Images
#'
#' Pulls out letterlist as its own object, and adds the image matrix as well
#' 
#' @param letterList Letter list from processHandwriting function
#' @param dims Dimensions of the original document
#' @return letterList with a new matrix `image` value for each sublist.
#' 
#' @examples
#' twoSent_document = list()
#' twoSent_document$image = twoSent
#' twoSent_document$thin = thinImage(twoSent_document$image)
#' twoSent_processList = processHandwriting(twoSent_document$thin, dim(twoSent_document$image))
#' 
#' dims = dim(twoSent_document$image)
#' withLetterImages = AddLetterImages(twoSent_processList$letterList, dims)
#' 
#' @noRd
AddLetterImages <- function(letterList, dims)
{
  skeletons = lapply(letterList, function(x) x$path)
  r = lapply(skeletons, function(x) {((x-1) %% dims[1]) + 1})
  c = lapply(skeletons, function(x) {((x-1) %/% dims[1]) + 1})
  for(i in 1:length(letterList))
  {
    letterList[[i]]$image = matrix(1, nrow = diff(range(r[[i]]))+1, ncol = diff(range(c[[i]]))+1)
    r[[i]] = r[[i]]-min(r[[i]])+1
    c[[i]] = c[[i]]-min(c[[i]])+1
    letterList[[i]]$image[cbind(r[[i]],c[[i]])] = 0
  }
  return(letterList)
}

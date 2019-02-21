#' plotImage
#'
#' This function plots a basic binary image.
#' @param x Binary matrix, usually from readPNGBinary
#' @keywords plot
#' @return Returns plot of x.
#' @import ggplot2
#' @export
plotImage = function(x)
{
  xm = melt(x)
  names(xm) = c("Var1", "Var2", "value")
  p = ggplot(xm, aes(Var2, rev(Var1))) + geom_raster(aes(fill = as.factor(value)), na.rm=TRUE) + scale_fill_manual(values = c("black", NA), guide = FALSE) + coord_fixed() + theme_void()
  return(p)
}


#' plotImageThinned
#'
#' This function returns a plot with the full image plotted in light gray and the skeleton printed in black on top.
#' @param img Full image matrix
#' @param thinned Thinned image matrix
#' @return Plot of full and thinned image.
#' 
#' @import ggplot2
#' 
#' @examples
#' 
#' ## Not Run
#' # plotImageThinned(london, london_thin)
#' # plotImageThinned(message, message_thin)
#'
#' @export

plotImageThinned = function(img, thinned)
{
  l.m = melt(img)
  names(l.m) = c("Var1", "Var2", "value")
  l.m$value[thinned] = 2
  p = ggplot(l.m, aes(Var2, rev(Var1))) + geom_raster(aes(fill = as.factor(value), alpha = as.factor(value)), na.rm=TRUE) + scale_alpha_manual(values = c(.1, NA, 1), guide = FALSE) + scale_fill_manual(values = c("black", NA, "black"), guide = FALSE) + coord_fixed() + theme_void()
  return(p)
}


#' plotNodes
#'
#' This function returns a plot with the full image plotted in light gray and the skeleton printed in black, with red triangles over the vertices.
#' Also called from plotPath, which is a more useful function, in general.
#' @param img Full image matrix, unthinned.
#' @param thinned Thinned image matrix
#' @param nodeList Nodelist returned from getNodes.
#' @param nodeSize Size of triangles printed. 3 by default. Move down to 2 or 1 for small text images.
#' @return Plot of full and thinned image with vertices overlaid.
#' 
#' @import ggplot2
#' 
#' @examples
#' # See getNodes() examples first.
#' # plotNodes(london, london_thin, london_nodes)
#' # plotNodes(message, message_thin, message_nodes)
#'
#' @export

plotNodes = function(img, thinned, nodeList, nodeSize = 3, nodeColor = "red")
{
  p = plotImageThinned(img, thinned)
  pointSet = data.frame(X = ((nodeList - 1) %/% dim(img)[1]) + 1, Y = dim(img)[1] - ((nodeList - 1) %% dim(img)[1]))
  p = p + geom_point(data = pointSet, aes(X, Y), size = nodeSize, shape = I(16), color = I(nodeColor), alpha = I(.4))
  return(p)
  #l.m = melt(img)
  #l.m$value[thinned] = 2
  #l.m$value[nodeList] = 3
  #n.m2 = n.m[nodeList,]
  #p = ggplot(l.m, aes(Var2, rev(Var1))) + geom_raster(aes(fill = as.factor(value != 1), alpha = ifelse(value==0,.3,1))) + scale_alpha_continuous(guide = FALSE) + scale_fill_manual(values = c("white", "black"), guide = FALSE) + theme_void() + geom_point(data= n.m2, aes(x = Var2, y = dim(img)[1] - Var1 + 1), shape = I(17), size = I(nodeSize), color = I("red"))
}


#' plotLetter
#'
#' This function returns a plot of a single letter extracted from a document. It uses the letterList parameter from the processHandwriting function and accepts a single value as whichLetter. Dims requires the dimensions of the entire document, since this isn't contained in processHandwriting.
#' @param letterList Letter list from processHandwriting function
#' @param whichLetter Single value in 1:length(letterList) denoting which letter to plot.
#' @param dims Dimensions of the original document
#' @param showPaths Whether the calculated paths on the letter should be shown with numbers.
#' @return Plot of single letter.
#' 
#' @import ggplot2
#' @export
plotLetter = function(letterList, whichLetter, dims, showPaths = TRUE, showCentroid = TRUE, showSlope = TRUE)
{
  path = letterList[[whichLetter]]$path
  r = ((path-1) %% dims[1]) + 1
  c = ((path-1) %/% dims[1]) + 1
  
  img = matrix(1, nrow = diff(range(r))+1, ncol = diff(range(c))+1)
  
  nodes = letterList[[whichLetter]]$nodes
  nodesr = ((nodes-1) %% dims[1]) + 1
  nodesc = ((nodes-1) %/% dims[1]) + 1
  nodesr = nodesr - min(r) + 1
  nodesc = nodesc - min(c) + 1
  
  rnew = r-min(r)+1
  cnew = c-min(c)+1
  nodes = ((nodesc - 1)*(diff(range(r))+1)) + nodesr
  
  centroid_y = letterList[[whichLetter]]$characterFeatures$centroid_y - min(r) + 1
  centroid_x = letterList[[whichLetter]]$characterFeatures$centroid_x - min(c) + 1
  
  lHalfr = ((letterList[[whichLetter]]$characterFeatures$lHalf - 1) %% dims[1]) + 1
  lHalfc = ((letterList[[whichLetter]]$characterFeatures$lHalf - 1) %/% dims[1]) + 1
  rHalfr = ((letterList[[whichLetter]]$characterFeatures$rHalf - 1) %% dims[1]) + 1
  rHalfc = ((letterList[[whichLetter]]$characterFeatures$rHalf - 1) %/% dims[1]) + 1
  
  lHalfr = lHalfr - min(r) + 1
  lHalfc = lHalfc - min(c) + 1
  rHalfr = rHalfr - min(r) + 1
  rHalfc = rHalfc - min(c) + 1
  
  lCentroid = c(mean(lHalfr), mean(lHalfc))
  rCentroid = c(mean(rHalfr), mean(rHalfc))
  
  ranger = letterList[[whichLetter]]$characterFeatures$height
  rangec = letterList[[whichLetter]]$characterFeatures$width
  
  centroidDat = data.frame(X = centroid_x, 
                           Y = ranger - centroid_y + 1)
  halfCentroidDat = data.frame(X = c(lCentroid[2], rCentroid[2]), 
                               Y = c(ranger - c(lCentroid[1], rCentroid[1]) + 1))

  img[cbind(rnew,cnew)] = 0
  
  pathPoints = NULL
  pathSets = letterList[[whichLetter]]$allPaths
  for(i in 1:length(pathSets))
  {
    pathr = ((pathSets[[i]]-1) %% dims[1]) + 1
    pathr = pathr - min(r) + 1
    pathc = ((pathSets[[i]]-1) %/% dims[1]) + 1
    pathc = pathc - min(c) + 1
    
    pathPoints = rbind(pathPoints, cbind(pathr, pathc, i))
  }
  
  p = plotNodes(img, which(img == 1), nodes)
  if(showPaths) p = p + geom_text(data = as.data.frame(pathPoints), aes(x = pathc, y = max(rnew) - pathr + 1, label = i))
  if(showCentroid) p = p + geom_point(data = centroidDat, aes(x = X, y = Y, color = I("red"), size = I(3), shape = I(7)))
  if(showSlope) p = p + geom_point(data = halfCentroidDat, aes(x = X, y = Y, color = I("red"), shape = I(4))) + 
    geom_line(data = halfCentroidDat, aes(x = X, y = Y, color = I("red")))
  return(p)
}

#' AddLetterImages
#'
#' This function returns a plot of a single letter extracted from a document. It uses the letterList parameter from the processHandwriting function and accepts a single value as whichLetter. Dims requires the dimensions of the entire document, since this isn't contained in processHandwriting.
#' @param letterList Letter list from processHandwriting function
#' @param docDims Dimensions of the original document
#' @return letterList with a new matrix `image` value for each sublist.
#' 
#' @export
AddLetterImages = function(letterList, docDims)
{
  skeletons = lapply(letterList, function(x) x$path)
  r = lapply(skeletons, function(x) {((x-1) %% docDims[1]) + 1})
  c = lapply(skeletons, function(x) {((x-1) %/% docDims[1]) + 1})
  for(i in 1:length(letterList))
  {
    letterList[[i]]$image = matrix(1, nrow = diff(range(r[[i]]))+1, ncol = diff(range(c[[i]]))+1)
    r[[i]] = r[[i]]-min(r[[i]])+1
    c[[i]] = c[[i]]-min(c[[i]])+1
    letterList[[i]]$image[cbind(r[[i]],c[[i]])] = 0
    #letterList[[i]]$image = cbind(1,rbind(1,letterList[[i]]$image,1),1)
  }
  return(letterList)
}

#' SaveAllLetterPlots
#'
#' This function returns a plot of a single letter extracted from a document. It uses the letterList parameter from the processHandwriting function and accepts a single value as whichLetter. Dims requires the dimensions of the entire document, since this isn't contained in processHandwriting. Requires the \pkg{\link{magick}} package.
#' @param letterList Letter list from processHandwriting function
#' @param filePaths Folder path to save images to
#' @param documentDimensions Dimensions of original document
#' @return Nothing
#' @seealso \pkg{\link{magick}}
#' @export
SaveAllLetterPlots = function(letterList, filePaths, documentDimensions, bgTransparent = TRUE)
{
  if(is.null(letterList[[1]]$image))
    letterList = AddLetterImages(letterList, documentDimensions)
  
  for(i in 1:length(letterList))
  {
    img= magick::image_read(as.raster(letterList[[i]]$image))
    if(bgTransparent)
      img  = magick::image_transparent(img, "white")
    magick::image_write(path = paste0(filePaths, "letter", i, ".png"), img)
  }
}


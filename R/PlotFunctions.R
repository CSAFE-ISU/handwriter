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
  Var2 <- Var1 <- value <- NULL
  xm = melt(x)
  names(xm) = c("Var1", "Var2", "value")
  p = ggplot(xm, aes(Var2, rev(Var1))) + geom_raster(aes(fill = as.factor(value)), na.rm=TRUE) + scale_fill_manual(values = c("black", "white"), guide = "none") + coord_fixed() + theme_void()
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
  Var2 <- Var1 <- value <- NULL
  l.m = melt(img)
  names(l.m) = c("Var1", "Var2", "value")
  l.m$value[thinned] = 2
  p = ggplot(l.m, aes(Var2, rev(Var1))) + geom_raster(aes(fill = as.factor(value), alpha = as.factor(value)), na.rm=TRUE) + scale_alpha_manual(values = c(.1, NA, 1), guide = "none") + scale_fill_manual(values = c("black", "white", "black"), guide = "none") + coord_fixed() + theme_void()
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
  X <- Y <- NULL
  
  p = plotImageThinned(img, thinned)
  pointSet = data.frame(X = ((nodeList - 1) %/% dim(img)[1]) + 1, Y = dim(img)[1] - ((nodeList - 1) %% dim(img)[1]))
  p = p + geom_point(data = pointSet, aes(X, Y), size = nodeSize, shape = I(16), color = I(nodeColor), alpha = I(.4))
  return(p)
}

#' plotWord
#'
#' This function returns a plot of a single Word extracted from a document. It uses the letterList parameter from the processHandwriting function and accepts a single value as whichLetter. Dims requires the dimensions of the entire document, since this isn't contained in processHandwriting.
#' @param letterList Letter list from processHandwriting function
#' @param whichWord Single word value denoting which line to plot - checked if too big inside function.
#' @param dims Dimensions of the original document
#' @return Plot of single word.
#' 
#' @import ggplot2
#' @export
plotWord = function(letterList, whichWord, dims)
{
  X <- Y <- NULL
  
  pathList = list()
  wordListIndex = list()
  #stitch all paths together
  count = 1
  for(i in letterList){
    
    wordIndex = i$characterFeatures$wordIndex
    if(wordIndex == whichWord)
    {
      pathList <- append(pathList, i$path)
      wordListIndex <- append(wordListIndex, count)
    }
    
    count = count + 1
  }
  
  #if nothing was found on that line, just exit out because it is too big (or small)
  if (length(pathList) == 0){
    stop("ERROR: no letters found on that path - valid lines are 1:max")
  }
  
  pathVec <- unlist(pathList)
  countVec <- unlist(wordListIndex)
  
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
  nodeSize = 4
  nodeColor = "red"
  pointSet = data.frame(X = ((nodeList - 1) %/% dim(img)[1]) + 1, Y = dim(img)[1] - ((nodeList - 1) %% dim(img)[1]))
  p = p + geom_point(data = pointSet, aes(X, Y), size = nodeSize, shape = I(16), color = I(nodeColor), alpha = I(.6))
  
  return(p)
}

#' plotLine
#'
#' This function returns a plot of a single line extracted from a document. It uses the letterList parameter from the processHandwriting function and accepts a single value as whichLetter. Dims requires the dimensions of the entire document, since this isn't contained in processHandwriting.
#' @param letterList Letter list from processHandwriting function
#' @param whichLine Single value denoting which line to plot - checked if too big inside function.
#' @param dims Dimensions of the original document
#' @return Plot of single line.
#' 
#' @import ggplot2
#' @export
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
plotLetter = function(letterList, whichLetter, dims, showPaths = TRUE, showCentroid = TRUE, showSlope = TRUE)#, showTightness = TRUE, showLoopDims = TRUE)
{
  X <- Y <- NULL
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
  img[cbind(rnew,cnew)] = 0
  p = plotNodes(img, which(img == 1), nodes)
  
  #End of plotting the Nodes, 
  #Start finding info for optional features to display
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
  tightnessDat = data.frame(x0 = centroid_x, y0 = ranger - centroid_y + 1)
  tightness = letterList[[whichLetter]]$characterFeatures$compactness
  
  
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
  
  
  #Plot paths as numbers
  if(showPaths) p = p + geom_text(data = as.data.frame(pathPoints), aes(x = pathc, y = max(rnew) - pathr + 1, label = i))
  
  #Plot Centroid
  if(showCentroid) p = p + geom_point(data = centroidDat, aes(x = X, y = Y, color = I("red"), size = I(3), shape = I(7)))
  
  #Plot Slope of Letter
  if(showSlope) p = p + geom_point(data = halfCentroidDat, aes(x = X, y = Y, color = I("red"), shape = I(4))) + 
    geom_line(data = halfCentroidDat, aes(x = X, y = Y, color = I("red")))
  
  # #Plot tightness of letter, where tightness scales area (not radius)
  # if(showTightness) p = p + geom_point(data = tightnessDat, aes(x = x0, y=y0, size = (sqrt(tightness/pi))*25, pch = 1, color = I("red"), stroke = 2))
  # 
  # #Plot centroid and longest line through centroid (and its perpendicular line) of all loops
  # if(showLoopDims){
  #   loops = letterList[[whichLetter]]$characterFeatures$loopInfo$loopPlottingInfo
  #   for(i in 1:length(loops)){
  #     if(length(loops) == 0) break
  #     loop = loops[[i]]
  #     loopCentroid = loop[[1]]
  #     longestLine = loop[[2]]
  #     shortestLine = loop[[3]]
  #     
  #     
  #     #Plot Centroid of loop
  #     #loopCentroid_y = (dims[[1]] - loopCentroid[2]) - min(r)+1
  #     loopCentroid_y = loopCentroid[2] - min(r)
  #     loopCentroid_x = loopCentroid[1] - min(c)
  #     loopCentroidDat = data.frame(X=loopCentroid_x+1, Y=ranger-loopCentroid_y)
  #     p=p+geom_point(data = loopCentroidDat, aes(x=X, y=Y, color = I("blue"), size = I(2), shape = I(7)))
  #     
  #     #Plot long line of loop
  #     longp1 = longestLine[[1]]
  #     longp2 = longestLine[[2]]
  #     
  #     longx1 = longp1[[1]]-min(c)
  #     #longy1 = (dims[[1]] - longp1[2]) - min(r)+1
  #     longy1 = longp1[[2]] - min(r)
  #     longx2 = longp2[[1]]-min(c)
  #     #longy2 = (dims[[1]] - longp2[2]) - min(r)+1
  #     longy2 = longp2[[2]] - min(r)
  #     
  #     loopLongestLine = data.frame(X = c(longx1, longx2)+1, 
  #                                  Y = c(ranger - c(longy1, longy2)))
  #     p=p+geom_line(data = loopLongestLine, aes(x = X, y = Y, color = I("blue")))
  #     
  #     #Plot short line of loop
  #     shortp1 = shortestLine[[1]]
  #     shortp2 = shortestLine[[2]]
  #     
  #     shortx1 = shortp1[1]-min(c)
  #     #shorty1 = (dims[[1]] - shortp1[2]) - min(r)+1
  #     shorty1 = shortp1[2] - min(r)
  #     shortx2 = shortp2[1]-min(c)
  #     #shorty2 = (dims[[1]] - shortp2[2]) - min(r)+1
  #     shorty2 = shortp2[2] - min(r)
  #     
  #     loopShortestLine = data.frame(X = c(shortx1, shortx2)+1, 
  #                                  Y = c(ranger - c(shorty1, shorty2)))
  #     p=p+geom_line(data = loopShortestLine, aes(x = X, y = Y, color = I("blue")))
  #   }
  # }
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
  }
  return(letterList)
}

#' SaveAllLetterPlots
#'
#' This function returns a plot of a single letter extracted from a document. It uses the letterList parameter from the processHandwriting function and accepts a single value as whichLetter. Dims requires the dimensions of the entire document, since this isn't contained in processHandwriting. Requires the \pkg{\link{magick}} package.
#' @param letterList Letter list from processHandwriting function
#' @param filePaths Folder path to save images to
#' @param documentDimensions Dimensions of original document
#' @param bgTransparent Logical determines if the image is transparent
#' @return Nothing
#' 
#' @seealso \code{\link[magick]{image_transparent}}  
#' @seealso \code{\link[magick]{image_write}}  
#' @seealso \code{\link[magick]{image_read}}
#' 
#' @export
SaveAllLetterPlots = function(letterList, filePaths, documentDimensions, bgTransparent = TRUE)
{
  if(is.null(letterList[[1]]$image))
    letterList = AddLetterImages(letterList, documentDimensions)
  
  for(i in 1:length(letterList))
  {
    img= magick::image_read(as.raster(letterList[[i]]$image))
    if(bgTransparent){
      img  = magick::image_transparent(img, "white")
      magick::image_write(path = paste0(filePaths, "letter", i, ".png"), img)
    }
  }
}


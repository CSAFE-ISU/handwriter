#' plotColorNodes
#'
#' This function returns a plot of a single Word extracted from a document.
#' It plots the color as well.
#' 
#' @param letterList Letter list from processHandwriting function
#' @param whichWord Single word value denoting which line to plot - checked if too big inside function.
#' @param dims Dimensions of the original document
#' @param wordInfo Word information list
#' @return Plot of single word.
#' 
#' @import ggplot2
#' @import stringr
#' @export
plotColorNodes = function(letterList, whichWord, dims, wordInfo) #TODO - CLEAN THIS UP: DONT NEED LETTERLIST
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
  
  rnew = r-min(r)+1
  cnew = c-min(c)+1
  
  img[cbind(rnew,cnew)] = 0
  
  #Plot line
  p = plotImage(img)
  
  #Reconfigure NodeList
  
  colorpoints_df = wordInfo[[whichWord]]$colorpoints_df
  nodesL = list(colorpoints_df[,1])
  nodeList = list()
  
  for(i in length(nodesL)){
    nodes = nodesL[[i]]
    nodesr = ((nodes-1) %% dims[1]) + 1
    nodesc = ((nodes-1) %/% dims[1]) + 1
    nodesr = nodesr - min(r) + 1
    nodesc = nodesc - min(c) + 1
    
    nodes = ((nodesc - 1)*(diff(range(r))+1)) + nodesr
    nodeList <- append(nodeList, nodes)
  }
  
  nodeList <- unlist(nodeList)
  
  #change colors to look nicer when plotting -- https://greggilbertlab.sites.ucsc.edu/teaching/rtransition/
  colorpoints_df$color = str_replace(colorpoints_df$color,"pink","hotpink1")
  colorpoints_df$color = str_replace(colorpoints_df$color,"orange","darkorange2")
  
  nodeSize = 4
  
  #Initializing X and Y to bind them for the dev check
  X <- 0
  Y <- 0
  pointSet = data.frame(X = ((nodeList - 1) %/% dim(img)[1]) + 1, Y = dim(img)[1] - ((nodeList - 1) %% dim(img)[1]))
  p = p + geom_point(data = pointSet, aes(X, Y), size = nodeSize, shape = I(16), color = I(colorpoints_df$color), alpha = I(.7))
  
  return(p)
}
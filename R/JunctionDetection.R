## Junction Detection.
## Provide skeletonized image from ThinText.

# A black pixel becomes a node if its removal creates exactly one or at least 
# three 4-connected black components in its 1-neighborhood.

# Also from Zhang thinning paper (allegedly)

#' Internal function for counting 4-connected components around a pixel.
countChanges = function(coords, img)
{
  rr = coords[1]
  cc = coords[2]
  if(rr>1 & cc>1 & rr<dim(img)[1] & cc<dim(img)[2])
  {
    neighbs = c(t(img[(rr-1):(rr+1),][,(cc-1):(cc+1)]))[c(2,3,6,9,8,7,4,1,2)]
    return(sum(neighbs == 1 & c(neighbs[-1], neighbs[1]) == 0))
  }
  else
  {
    stop("Please use `crop` to crop your image. Not padded around outside.")
  }
}

#' getNodes
#'
#' Detect intersection points of an image thinned with thinImage.
#' @param img Thinned binary image.
#' @keywords vertex detection, Zhang, Suen
#' @return Returns image matrix. 1 is blank, 0 is a node.
#' @examples
#' data(london)
#' london = crop(london)
#' london_thin = thinImage(london, verbose = TRUE)
#' london_nodes = getNodes(london_thin)
#' 
#' data(cells)
#' cells = crop(cells)
#' cells_thin = thinImage(cells, verbose = TRUE)
#' cells_nodes = getNodes(cells_thin)
#' 
#' data(message)
#' message = crop(message)
#' message_thin = thinImage(message, verbose = TRUE)
#' message_nodes = getNodes(message_thin)
#' 
#' @export

getNodes = function(img)
{
  indices = which(img == 0)
  img.m = cbind(((indices-1) %% dim(img)[1]) + 1, ((indices - 1) %/% dim(img)[1]) + 1)
  changeCount = matrix(apply(X = img.m, MARGIN = 1, FUN = countChanges, img = img), byrow = F, nrow = 1)
  nodes = matrix(1, dim(img)[1], dim(img)[2])
  nodes[indices] = ifelse(changeCount == 1 | changeCount >= 3, 0, 1)
  return(nodes)
}

#' plotNodes
#' 
#' This function returns a plot with the full image plotted in light gray and the skeleton printed in black, with red triangles over the vertices.
#' @param img Full image matrix, unthinned.
#' @param thinned Thinned image matrix
#' @param nodeList Nodelist returned from getNodes.
#' @param nodeSize Size of triangles printed. 3 by default. Move down to 2 or 1 for small text images.
#' @return Plot of full and thinned image with vertices overlaid.
#' @examples
#' # See getNodes() examples first.
#' plotNodes(london, london_thin, london_nodes)
#' plotNodes(cells, cells_thin, cells_nodes)
#' plotNodes(message, message_thin, message_nodes)
#' 
#' @export

plotNodes = function(img, thinned, nodeList, nodeSize = 3)
{
  l.m = melt(img)
  t.m = melt(thinned)
  n.m = melt(nodeList)
  l.m$value[t.m$value == 0] = 2
  l.m$value[n.m$value == 0] = 3
  n.m2 = n.m[n.m$value == 0,]
  p = ggplot(l.m, aes(Var2, rev(Var1))) + geom_raster(aes(fill = as.factor(value != 1), alpha = ifelse(value==0,.3,1))) + scale_alpha_continuous(guide = FALSE) + scale_fill_manual(values = c("white", "black"), guide = FALSE) + theme_void() + geom_point(data= n.m2, aes(x = Var2, y = dim(thinned)[1] - Var1 + 1), shape = I(17), size = I(nodeSize), color = I("red"))
  return(p)
}

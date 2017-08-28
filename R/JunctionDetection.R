## Junction Detection.
## Provide skeletonized image from ThinText.

# A black pixel becomes a node if its removal creates exactly one or at least 
# three 4-connected black components in its 1-neighborhood.

# Also from Zhang thinning paper

countNeighbors = function(coords, img)
{
  rr = coords[1]
  cc = coords[2]
  print(img)
  return(any(c(t(img[(rr-1):(rr+1),(cc-1):(cc+1)]))[c(2,4,6,8)] == 0))
}

postRemovalCount = function(coords, img)
{
  rr = coords[1]
  cc = coords[2]
  neighbs = img[((rr-2):(rr+2)), ((cc-2):(cc+2))]
  neighbs[3,3] = 1
  check = matrix(c(2,3,4,2,4,2,3,4,2,2,2,3,3,4,4,4), ncol = 2, byrow = F)
  #check = matrix(c(2,3,3,4,3,2,4,3), ncol = 2, byrow = F)
  newNeighCount = ifelse(img[rr,cc] == 0, matrix(apply(X = check, MARGIN = 1, FUN = countNeighbors, img = neighbs), byrow = F, nrow = 1), 0)
  return(sum(newNeighCount))
}
getNodes = function(img)
{
  indices = which(img == 0)
  img.m = cbind(((indices-1) %% dim(img)[1]) + 1, ((indices - 1) %/% dim(img)[1]) + 1)
  offEdge = between(img.m[,1], 3, dim(img)[1] - 2) & between(img.m[,2], 3, dim(img)[2] - 2)
  img.m = img.m[offEdge,]
  indices = indices[between(img.m[,1], 3, dim(img)[1] - 2) & between(img.m[,2], 3, dim(img)[2] - 2)]
  postRemovalCount = matrix(apply(X = img.m, MARGIN = 1, FUN = postRemovalCount, img = img), byrow = F, nrow = 1)
  nodes = matrix(1, dim(img)[1], dim(img)[2])
  nodes[indices[offEdge]] = ifelse(postRemovalCount == 1 | postRemovalCount >= 3, 0, 1)
  return(nodes)
}
plotNodes = function(thinned, nodeList)
{
  t.m = melt(thinned)
  n.m = melt(nodeList)
  t.m$value[n.m$value == 0] = 2
  p = ggplot(t.m, aes(Var2, rev(Var1))) + geom_raster(aes(fill = as.factor(value))) + scale_fill_manual(values = c("black", "white", "red"), guide = FALSE) + theme_void()
  return(p)
}
#letter_nodes = getNodes(letter_thin)
#plotNodes(letter_thin, letter_nodes)

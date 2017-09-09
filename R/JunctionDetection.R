## Junction Detection.
## Provide skeletonized image from ThinText.

# A black pixel becomes a node if its removal creates exactly one or at least 
# three 4-connected black components in its 1-neighborhood.

# Also from Zhang thinning paper (allegedly)

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

getNodes = function(img)
{
  indices = which(img == 0)
  img.m = cbind(((indices-1) %% dim(img)[1]) + 1, ((indices - 1) %/% dim(img)[1]) + 1)
  changeCount = matrix(apply(X = img.m, MARGIN = 1, FUN = countChanges, img = img), byrow = F, nrow = 1)
  nodes = matrix(1, dim(img)[1], dim(img)[2])
  nodes[indices] = ifelse(changeCount == 1 | changeCount >= 3, 0, 1)
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
#' readPNGBinary
#' This function reads in and binarizes PNG images from the specified file path.
#' @param path File path for image.
#' @param cutoffAdjust Multiplicative adjustment to the K-means estimated binarization cutoff.
#' @param clean Whether to fill in white pixels with 7 or 8 neighbors. This will help a lot when thinning -- keeps from getting little white bubbles in text.
#' @param inversion Boolean dictating whether or not to flip each pixel of binarized image. Flipping happens after binarization. FALSE by default.
#' @keywords binary
#' @importFrom png readPNG
#' @return Returns image from path. 0 represents black, and 1 represents white by default.
#' @export

readPNGBinary = function(path, cutoffAdjust = 1, clean = TRUE, crop = TRUE, inversion = FALSE)
{
  img = png::readPNG(path)
  img = as.array(img)
  if(length(dim(img)) > 2)
  {
    if(dim(img)[3] == 4)
    {
      img = rgba2rgb(img)
    }
    if(dim(img)[3] > 1)
    {
      img = rgb2grayscale(img)
    }
  }
  if(inversion)
    img = 1-img
  
  # Threshold Image
  km1 = kmeans(c(img), c(0,1))
  m = which.min(km1$centers)
  m1 = max(img[km1$cluster == m])
  m2 = min(img[km1$cluster == 3-m])
  thresh = cutoffAdjust*mean(c(m1, m2))
  img = img > thresh
  
  if(clean)
  {
    img = cleanBinaryImage(img)
  }
  
  if(crop)
  {
    img = crop(img)
  }
  
  return(img + 0)
}

#' plotImage
#'
#' This function plots a basic binary image.
#' @param x Binary matrix, usually from readPNGBinary
#' @keywords plot
#' @return Returns plot of x.
#' @import ggplot2
#' @export
coordsToIndex = function(rowVals,colVals,dimImageAt1){
  #remove this
  # (column-1)*dim(image)[1] + row
}
plotImage = function(x)
{
  xm = melt(x)
  names(xm) = c("Var1", "Var2", "value")
  p = ggplot(xm, aes(Var2, rev(Var1))) + geom_raster(aes(fill = as.factor(value)), na.rm=TRUE) + scale_fill_manual(values = c("black", NA), guide = FALSE) + coord_fixed() + theme_void()
  return(p)
}

#' ben
#' probably remove this ://
#' hmm, takes in list of lists [ [x,y], [x,y], [x,y] .. ] of corrections to be made
plotImagePoints = function(x){
  xm = melt(x)
  names(xm) = c("Var1", "Var2", "value")
  p = ggplot(xm, aes(Var2, rev(Var1))) + geom_point(ppdf, aes(x = col, y = row)) + geom_raster(aes(fill = as.factor(value)), na.rm=TRUE) + scale_fill_manual(values = c("black", NA), guide = FALSE) + coord_fixed() + theme_void()
  return(p)
}
# #' neighborChanges
# #'
# #' Internal function for thinImage. Counts switches from 1 to 0 around a point.
# #' @param coords Point location.
# #' @param img Image object.
# #' @return Returns count of switches.

# neighborChanges = function(coords, img)
# {
#   rr = coords[1]
#   cc = coords[2]
#   if(rr>1 & cc>1 & rr<dim(img)[1] & cc<dim(img)[2])
#   {
#     neighbs = c(t(img[(rr-1):(rr+1),][,(cc-1):(cc+1)]))[c(2,3,6,9,8,7,4,1,2)]
#     if(rr + (cc-1)*dim(img)[1] == 511884)
#     {
#       print(neighbs)
#     }
#     
#     return(sum(neighbs == 1 & c(neighbs[-1], neighbs[1]) == 0))
#   }
#   else
#     return(0)
# }
# 
# # numNeighbors
# #
# # Internal function for thinImage. Counts neighbors with value of 0.
# # @param coords Point location.
# # @param img Image object.
# # @return Returns number of 0 neighbors.
# 
# numNeighbors = function(coords, img)
# {
#   rr = coords[1]
#   cc = coords[2]
#   if(rr>1 & cc>1 & rr<dim(img)[1] & cc<dim(img)[2])
#   {
#     neighbs = c(t(img[(rr-1):(rr+1),][,(cc-1):(cc+1)]))[c(2,3,6,9,8,7,4,1)]
#     return(sum(neighbs == 0))
#   }
#   else
#     return(0)
# }
# 
# # neighbs246468
# #
# # Internal function for thinImage. Returns 1 if one of the points to the top, bottom and left AND one of right, bottom, left are non-zero. 0 otherwise.
# # @param coords Point location.
# # @param img Image object.
# # @return Returns 1 or 0 for condition mentioned above.
# #
# neighbs246468 = function(coords, img)
# {
#   rr = coords[1]
#   cc = coords[2]
#   if(rr>1 & cc>1 & rr<dim(img)[1] & cc<dim(img)[2])
#   {
#     neighbs246 = c(t(img[(rr-1):(rr+1),][,(cc-1):(cc+1)]))[c(2,6,8)]
#     neighbs468 = c(t(img[(rr-1):(rr+1),][,(cc-1):(cc+1)]))[c(4,6,8)]
#     return(as.numeric(any(neighbs246 == 1) & any(neighbs468 == 1)))
#   }
#   else
#     return(0)
# }
# 
# # neighbs246468
# #
# # Internal function for thinImage. Returns 1 if at least one of the points to the top, right and bottom AND at least one of top, right, left are non-zero. 0 otherwise.
# # @param coords Point location.
# # @param img Image object.
# # @return Returns 1 or 0 for condition mentioned above.
# 
# neighbs248268 = function(coords, img)
# {
#   rr = coords[1]
#   cc = coords[2]
#   if(rr>1 & cc>1 & rr<dim(img)[1] & cc<dim(img)[2])
#   {
#     neighbs248 = c(t(img[(rr-1):(rr+1),][,(cc-1):(cc+1)]))[c(2,4,6)]
#     neighbs268 = c(t(img[(rr-1):(rr+1),][,(cc-1):(cc+1)]))[c(2,4,8)]
#     return(as.numeric(any(neighbs248 == 1) & any(neighbs268 == 1)))
#   }
#   else
#     return(0)
# }
# 
# # stepA
# #
# # Internal function for thinImage. First iteration of checks for thinning algorithm
# # @param img Image object.
# # @param coords Locations of points to check.
# # @return TRUE or FALSE for each point in coords, based on if they pass the checks.
# 
# stepA = function(img, coords)
# {
#   imgA = matrix(apply(X = coords, MARGIN = 1, FUN = neighborChanges, img = img), byrow = F, nrow = 1)
#   cat("A1: ", sum(imgA != 1), "\n")
#   imgB = matrix(apply(X = coords, MARGIN = 1, FUN = numNeighbors, img = img), byrow = F, nrow = 1)
#   cat("B1: ", sum(imgA == 1 & !(imgB >= 2 & imgB <= 6)), "\n")
#   img246468 = matrix(apply(X = coords, MARGIN = 1, FUN =  neighbs246468, img = img), byrow = F, nrow = 1)
#   cat("246468: ", sum(imgA == 1 & (imgB >= 2 & imgB <= 6) & img246468 != 1), "\n")
#   imgFlag = as.numeric(imgB >= 2 & imgB <= 6 & imgA == 1 & img246468 == 1)
#   return(imgFlag)
# }
# 
# # stepA
# #
# # Internal function for thinImage. Second iteration of checks for thinning algorithm
# # @param img Image object.
# # @param coords Locations of points to check.
# # @return TRUE or FALSE for each point in coords, based on if they pass the checks.
# 
# stepB = function(img, coords)
# {
#   imgA = matrix(apply(X = coords, MARGIN = 1, FUN = neighborChanges, img = img), byrow = F, nrow = 1)
#   cat("A2: ", sum(imgA != 1), "\n")
#   imgB = matrix(apply(X = coords, MARGIN = 1, FUN = numNeighbors, img = img), byrow = F, nrow = 1)
#   cat("B2: ", sum(imgA == 1 & !(imgB >= 2 & imgB <= 6)), "\n")
#   img248268 = matrix(apply(X = coords, MARGIN = 1, FUN =  neighbs248268, img = img), byrow = F, nrow = 1)
#   cat("248268: ", sum(imgA == 1 & (imgB >= 2 & imgB <= 6) & img248268 != 1), "\n")
#   imgFlag = as.numeric(imgB >= 2 & imgB <= 6 & imgA == 1 & img248268 == 1)
#   return(imgFlag)
# }
# 
# # thinImage
# #
# # Fast implementation of the Zhang-Suen Thinning algorithm (1984).
# # @param img Binary Image matrix.
# # @param verbose Whether you want detailed output as code is run. Defaults to FALSE.
# # @keywords thinning, Zhang, Suen
# # @return Returns thinned image matrix.
# # @examples
# # data(london)
# # london = crop(london)
# # london_thin = thinImage(london, verbose = TRUE)
# #
# #
# # data(message)
# # message = crop(message)
# # message_thin = thinImage(message, verbose = TRUE)
# #
# # @export
# 
# thinImage = function(img, verbose = FALSE)
# {
#   flag = TRUE
#   if(verbose)
#   {
#     iterCount = 1
#     total.time.start = Sys.time()
#   }
#   thinned = img
#   change = which(img == 0)
#   while(flag == TRUE)
#   {
#     if(verbose != FALSE) start.time <- Sys.time()
#     index = change[thinned[change] == 0]
#     img.m = cbind(((index - 1) %% dim(img)[1]) + 1, ((index - 1) %/% dim(img)[1]) + 1)
#     flagA = stepA(thinned, img.m)
#     thinned[index] = ifelse(c(thinned[index] == 0) & flagA == 0, 0, 1)
#     flagB = stepB(thinned, img.m)
#     thinned[index] = ifelse(c(thinned[index] == 0) & flagB == 0, 0, 1)
# 
#     if(sum(flagA + flagB, na.rm = T) == 0)
#     {
#       flag = FALSE
#     }
#     else
#     {
#       change = index[(flagA | flagB)]
#       change = unique(rep(change, each = 9) + rep(c(0,1, -1, dim(img)[1]-1, dim(img)[1]+1, -dim(img)[1]+1, -dim(img)[1]-1, dim(img)[1], -dim(img)[1]), length(change)))
#       change = change[change>=1 & change<=prod(dim(img))]
#     }
# 
#     if(verbose == TRUE)
#     {
#       cat("\nIteration", iterCount, "done:", sum(flagA | flagB), "changes.")
#       cat("\nLeft to check:", ifelse(sum(flagA | flagB) != 0, length(change), 0))
#       iterCount = iterCount + 1
#     }
#     flagA[] = 0
#     flagB[] = 0
# 
#     if(verbose == TRUE)
#     {
#       #temp = matrix(1, dim(img)[1], dim(img)[2])
#       #temp[change] = 0
#       #image(temp, main = "Need to update")
# 
#       end.time <- Sys.time()
#       cat("\nIteration Time:", end.time - start.time, "\n")
#     }
#   }
#   if(verbose)
#   {
#     total.time.end = Sys.time()
#     cat("\n------------------------\nTotal Run Time:", difftime(total.time.end, total.time.start, units = "secs"), "\n------------------------\n")
#   }
#   return(which(thinned == 0))
# }

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

#' crop
#'
#' This function crops an image down so that there is 1 pixel of padding on each side of the outermost 0 points.
#' @param img Full image matrix to be cropped
#' @return Cropped image matrix.
#'
#' @export

crop = function(img)
{
  if(any(img[,1] != 1)) {img = cbind(rep(1, dim(img)[1]), img)}
  if(any(img[,dim(img)[2]] != 1)) {img = cbind(img, rep(1, dim(img)[1]))}
  if(any(img[1,] != 1)) {img = rbind(rep(1, dim(img)[2]), img)}
  if(any(img[dim(img)[1],] != 1)) {img = rbind(img, rep(1, dim(img)[2]))}

  rows = apply(img, 1, FUN = function(x){any(x != 1)})
  cols = apply(img, 2, FUN = function(x){any(x != 1)})
  x.min = max(which(rows)[1] - 1, 1)
  x.max = min(which(rows)[sum(rows)] + 1, length(rows))
  y.min = max(which(cols)[1] - 1, 1)
  y.max = min(which(cols)[sum(cols)] + 1, length(cols))

  return(img[x.min:x.max,y.min:y.max])
}

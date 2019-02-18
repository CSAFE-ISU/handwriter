#' readPNGBinary
#' This function reads in and binarizes PNG images from the specified file path.
#' @param path File path for image.
#' @param cutoffAdjust Multiplicative adjustment to the K-means estimated binarization cutoff.
#' @param clean Whether to fill in white pixels with 7 or 8 neighbors. This will help a lot when thinning -- keeps from getting little white bubbles in text.
#' @param inversion Boolean dictating whether or not to flip each pixel of binarized image. Flipping happens after binarization. FALSE by default.
#' @keywords binary
#' @importFrom png readPNG
#' @useDynLib handwriter, .registration = TRUE
#' @importFrom Rcpp sourceCpp
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
  thresh = otsuBinarization(img, 512)*cutoffAdjust
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


#' otsuBinarization
otsuBinarization = function(img, breaks = 512)
{
  histVals = hist(img, breaks = breaks, plot = FALSE)
  numBins = length(histVals$counts)
  w1 = cumsum(histVals$counts)
  w2 = w1[numBins] + histVals$counts - w1
  mu1 = cumsum(histVals$counts*histVals$mids)
  mu2 = mu1[numBins] + histVals$counts*histVals$mids - mu1
  var = log(w1) + log(w2) + 2*log((mu2/w2 - mu1/w1))
  peak = which.max(var)

  return(histVals$mids[peak])
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

#'preprocess_mutations
#'expidite the testing process for the new preprocessing algorithm
#'@param path path to binary image, function will handle most of the rest
#'
preprocess_mutations = function(path, nodesize = 1, init = FALSE){
  if(init){
    library(handwriter)
    library(reticulate)
    library(ggplot2)
    library(reshape)
    setwd("/home/esc/git_repos/fall_18/work/handwriter/R/")
    source_python("./preprocess_mutation_oct.py")
  }
  binImg = readPNGBinary(path)
  binImg = crop(binImg)
  binImgthin = thinImage(binImg)
  binImgpp = preprocess(binImg)
  binImgDif = compareBinaries(binImgpp,binImg)
  binImgpp_v = preprocessToIndexVector(dim(binImg),binImgDif)
  print(binImgpp_v)
  plotCleaningChanges(binImg,binImgthin,NULL,binImgpp_v,nodesize)
}

#'preprocess_mutations
#'expidite the testing process for the new preprocessing algorithm
#'@param path path to binary image, function will handle most of the rest
#'
preprocess_mutations_dif = function(path, clean = TRUE, nodesize = 1, init = FALSE){
  if(init){
    library(handwriter)
    library(reticulate)
    library(ggplot2)
    library(reshape)
    setwd("/home/esc/git_repos/fall_18/work/handwriter/R/")
    source_python("./preprocess_mutation_oct.py")
  }
  binImg = readPNGBinary(path,clean = clean)
  print(summary(c(binImg)))
  #print(plotImage(binImg))
  plotImage(binImg)
  binImg = crop(binImg)
  print("we done croppin")
  binImgthin = thinImage(binImg)
  plotImageThinned(binImg,binImgthin)
  print("image has been plot thind")
  print(dim(binImg))
  print(typeof(binImg))
  #print(plotImageThinned(binImg,binImgthin))
  binImgpp = preprocess(binImg)
  binImgDif = compareBinariesDif(binImg,binImgpp)
  binImgpp_v = preprocessToIndexVector(dim(binImg),binImgDif)
  binImg_df = data.frame(index=binImgpp_v,type=binImgDif[,3])
  print("binary image dataframe below \n")
  print(binImg_df)
  plotCleaningChanges(binImg,binImgthin,binImg_df$index[binImg_df$type=="fill"],binImg_df$index[binImg_df$type=="clean"],nodesize)
  #plotCleaningChanges(img,thinned,preprocess_df$index[preprocess_df$cleantype=="blue"],preprocess_df$index[preprocess_df$cleantype=="red"],nodesize)
  return(binImgpp)
}

#'preprocess_mutations tester
#'expidite the testing process for the new preprocessing algorithm
#'trying reordering some steps of the thinning program
#'@param path path to binary image, function will handle most of the rest
#'
preprocess_mutations_test = function(path, clean = TRUE, nodesize = 1, init = FALSE){
  if(init){
    install.packages(handwriter)
    library(handwriter)
    library(reticulate)
    library(ggplot2)
    library(reshape)
    setwd("/home/esc/git_repos/fall_18/work/handwriter/R/")
    source_python("./preprocess_mutation.py")
  }
  binImg = readPNGBinary(path,clean)
  print(plotImage(binImg))
  plotImage(binImg)
  binImg = crop(binImg)
  print("we made it") 
  binImgpp = preprocess(binImg)
  binImgthinog = thinImage(binImg)
  binImgthin = thinImage(binImgpp)
  binImgDif = compareBinariesDif(binImgthin,binImgthinog)
  binImgpp_v = preprocessToIndexVector(dim(binImg),binImgDif)
  binImg_df = data.frame(index=binImgpp_v,type=binImgDif[,3])
  print("binary image dataframe below \n")
  print(binImg_df)
  plotCleaningChanges(binImg,binImgthin,binImg_df$index[binImg_df$type=="fill"],binImg_df$index[binImg_df$type=="clean"],nodesize)
  #plotCleaningChanges(img,thinned,preprocess_df$index[preprocess_df$cleantype=="blue"],preprocess_df$index[preprocess_df$cleantype=="red"],nodesize)
}

preprocessThinPlotPNG = function(path){
  png = readPNGBinary(path)
  png= crop(png)
  png_thinned = thinImage(png)
  png_pp = preprocessCroppedBinary(png)
  png_pp_v = preprocessToIndexVector(dim(png),png_pp)
  plotCleaningChangesFromVector(png,png_thinned,1,png_pp_v)
}
#' preprocessCroppedBinary
#' This function is intended after crop(), preprocesses images based on masks, uses Python
#' @param img Cropped binary image (pass through readPNGBinary then crop, found below)
#' @param reticulate_init If you do not have reticulate installed, will handle the installation and sourcing of script
#' @param pypath path to preprocess.py
#' @return list of values corresponding to fill / clean. 18 sept 18: red = clean, blue = fill, i don't know how to make use of this
#'
preprocessCroppedBinary = function(img, reticulate_init = FALSE, pypath = "./"){
  if(reticulate_init){
    libraries.install("reticulate")
    library("reticulate")
  }
  source_python(paste(pypath,"preprocess.py",sep =""))
  x <- preprocess(img)
  return(x)
}

loadLibaries = function(){
  library("reticulate")
  library("handwriter")
  library("ggplot2")
  library("reshape2")
}
plotMutationsFromVector = function(img,thinned,nodesize=1,mutations){
  mutation_index = preprocessToIndexVector(dim(img),mutations)
  mutation_df = data.frame(index=mutation_index)
  plotCleaningChanges(img,thinned,NULL,mutation_df$index,nodesize)
}

plotCleaningChangesFromVector = function(img, thinned,nodesize = 1,preprocess_result){
  preprocess_index = preprocessToIndexVector(dim(img),preprocess_result)
  preprocess_df = data.frame(cleantype=preprocess_result[,3],index = preprocess_index)
  plotCleaningChanges(img,thinned,preprocess_df$index[preprocess_df$cleantype=="blue"],preprocess_df$index[preprocess_df$cleantype=="red"],nodesize)
}
#' preprocessToIndexVector
#' This function is intended after preprocessCroppedBinary
#' @param preprocessList list resultant after preprocessCroppedBinary
#' @param img Cropped binary image (pass through readPNGBinary then crop, found below)
#' @return vector of indicies to plot points via ggplot
#'
preprocessToIndexVector = function(dims,preprocessList){
  #(column-1)*dim(image)[1] + row changed as python indexes from 0 
  #preprocessList = unlist(preprocessList)
  row = unlist(preprocessList[,1]) #y
  col = unlist(preprocessList[,2]) #y
  return((col)*dims[1]+row+1)
  # if(FALSE){
  #   fcl = split(preprocessList,preprocessList$type)
  #   #so fill clean list will have fills at fcl[1] and cleans at fcl[2]
  #   #uhh cleans seem guarenteed if a fill is ever present
  #   if(length(fcl)<2){
  #     clean_data = fcl[[1]]
  #     cd_rt = (as.numeric(clean_data[[2]])-1)*dim(img)[1] + as.numeric(clean_data[[1]])
  #     ul_cd = unlist(cd_rt, use.names=FALSE)
  #     return_list = list("clean" = ul_cd)
  #   }
  #   else{
  #     fill_data = fcl[[1]]
  #     clean_data = fcl[[2]]
  #     fd_rt = (as.numeric(fill_data[2])-1)*dim(img)[1] + as.numeric(fill_data[1])
  #     cd_rt = (as.numeric(clean_data[2])-1)*dim(img)[1] + as.numeric(clean_data[1])
  #     ul_fd = unlist(fd_rt, use.names=FALSE)
  #     ul_cd = unlist(cd_rt, use.names=FALSE)
  #     return_list = list("fill" = ul_fd, "clean" = ul_cd)
  #   }
  #   return(return_list)
  # }
}

#nic made this, thank you
#nicktext_pp_vectors["clean"][[1]]
plotCleaningChanges = function(img, thinned, added, removed,nodesize = 1)
{
  p = plotImageThinned(img, thinned)
  pointsAdded = data.frame(X = ((added - 1) %/% dim(img)[1]) + 1, Y = dim(img)[1] - ((added - 1) %% dim(img)[1]))
  pointsRemoved = data.frame(X = ((removed - 1) %/% dim(img)[1]) + 1, Y = dim(img)[1] - ((removed - 1) %% dim(img)[1]))
  p = p + geom_point(data = pointsRemoved, aes(X, Y), size = nodesize, shape = I(16), color = I("red"), alpha = I(.4)) + 
    geom_point(data = pointsAdded, aes(X, Y), size = nodesize, shape = I(16), color = I("darkgreen"), alpha = I(.4))
  return(p)
}
#appendages above, nic will likely want me to move / remove them from this package


makeassignment = function(imageListElement, templateCenterList, outliercut){
  dist = min(unlist(lapply(templateCenterList, function(x){getGraphDistance(imageList1 = imageListElement, imageList2 = x, isProto2 = TRUE)$matching_weight})))
  cluster = which.min(unlist(lapply(templateCenterList, function(x){getGraphDistance(imageList1 = imageListElement, imageList2 = x, isProto2 = TRUE)$matching_weight})))
  return(cluster)
}


#' get_clusterassignment
#'
#' @param clustertemplate Cluster template created with `make_clustering_templates`
#' @param input_dir Directory containing handwriting processed with `process_batch_list` or `process_batch_dir`
#' @param num_cores Integer number of cores to use for parallel processing
#'
#' @return list of processed handwriting with cluster assignments for each graph
#' @export
#'
get_clusterassignment = function(clustertemplate, input_dir, num_cores){
  
  # list files in input dir
  input_paths = list.files(input_dir, full.names = TRUE)
  df = data.frame(input_paths, stringsAsFactors = FALSE)
  
  # load processed handwriting
  proclist =  df$input_paths %>%
    purrr::map(readRDS) %>%
    purrr::list_merge()
  
  my_cluster = parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(my_cluster)
  
  proclist <- foreach::foreach(i = 1:length(proclist), 
                               .export = c("AddLetterImages", "MakeLetterListLetterSpecific", "centeredImage", "makeassignment")) %dopar% {  # for each document i
    # extra processing
    proclist_mod = proclist[[i]]
    proclist_mod$process$letterList = AddLetterImages(proclist_mod$process$letterList, dim(proclist_mod$image))
    proclist_mod$process$letterList = MakeLetterListLetterSpecific(proclist_mod$process$letterList, dim(proclist_mod$image)) ### THIS SCREWS UP PLOTLETTER AND OTHER PLOTTING!!!
    
    imagesList = list()
    imagesList = c(imagesList, lapply(proclist_mod$process$letterList, function(x){centeredImage(x)}))
    imagesList = lapply(imagesList, function(x){
      x$nodesrc = cbind(((x$nodes-1) %/% dim(x$image)[1]) + 1, dim(x$image)[1] - ((x$nodes-1) %% dim(x$image)[1]))
      x$nodesrc = x$nodesrc - matrix(rep(x$centroid, each = dim(x$nodesrc)[1]), ncol = 2)
      x$pathEndsrc = lapply(x$allPaths, function(z){cbind(((z[c(1,length(z))]-1) %/% dim(x$image)[1]) + 1, dim(x$image)[1] - ((z[c(1,length(z))]-1) %% dim(x$image)[1]))})
      x$pathEndsrc = lapply(x$pathEndsrc, function(z){z - matrix(rep(x$centroid, each = 2), ncol = 2)})
      return(x)
    })
    
    # get cluster assignments
    cluster_assign = sapply(imagesList, makeassignment, templateCenterList = clustertemplate$centers, outliercut = outliercut)
    
    # add cluster assignments to proclist_mod
    for(j in 1:length(proclist_mod$process$letterList)){  # for each graph j in document i
      proclist_mod$process$letterList[[j]]$cluster = cluster_assign[j]
    }
    
    return(proclist_mod)
  }

  return(proclist)
}


GetImageMatrix = function(letterList, maxImageSize = 50)
{
  imagesList = list()
  imagesList = c(imagesList, lapply(letterList, function(x){centeredImage(x)}))
  
  # letterList = unlist(letListFull,recursive=FALSE)
  
  for(i in 1:length(imagesList)){
    l = imagesList[[i]]$centroid[1]
    r = dim(imagesList[[i]]$image)[2] - imagesList[[i]]$centroid[1] + 1
    t = dim(imagesList[[i]]$image)[1] - imagesList[[i]]$centroid[2] + 1
    b = imagesList[[i]]$centroid[2]
    if(l > r){
      imagesList[[i]]$image = cbind(imagesList[[i]]$image, matrix(1, ncol = l-r, nrow = dim(imagesList[[i]]$image)[1]))
    }
    else if(l < r){
      imagesList[[i]]$image = cbind(matrix(1, ncol = r-l, nrow = dim(imagesList[[i]]$image)[1]), imagesList[[i]]$image)
    }
    if(t > b){
      imagesList[[i]]$image = rbind(imagesList[[i]]$image, matrix(1, nrow = t-b, ncol = dim(imagesList[[i]]$image)[2]))
    }
    else if(t < b){
      imagesList[[i]]$image = rbind(matrix(1, nrow = b-t, ncol = dim(imagesList[[i]]$image)[2]), imagesList[[i]]$image)
    }
  }
  
  for(i in 1:length(imagesList)){
    if(any(dim(imagesList[[i]]$image) > maxImageSize)){
      imagesList[[i]]$image = imagesList[[i]]$image %>% as.raster() %>% image_read() %>% image_resize(paste0(maxImageSize,"x",maxImageSize)) %>% image_quantize(max = 2, dither = FALSE, colorspace = "gray") %>% `[[`(1) %>% as.numeric() %>% `[`(,,1)
      imagesList[[i]]$image = rbind(1,cbind(1,imagesList[[i]]$image,1),1)
      thinned = thinImage(imagesList[[i]]$image)
      imagesList[[i]]$image[] = 1
      imagesList[[i]]$image[thinned] = 0
      
      imagesList[[i]]$image = imagesList[[i]]$image[-c(1,dim(imagesList[[i]]$image)[1]),-c(1,dim(imagesList[[i]]$image)[2])]
      #print(plotImage(imagesList[[i]]$image) + theme_bw())
      cat(i, " ")
    }
  }
  
  for(i in 1:length(imagesList)){
    dims = dim(imagesList[[i]]$image)
    lrPad = maxImageSize + 2 - dims[2]
    tbPad = maxImageSize + 2 - dims[1]
    l = floor(lrPad/2)
    r = ceiling(lrPad/2)
    b = ceiling(tbPad/2)
    t = floor(tbPad/2)
    
    imagesList[[i]]$image = rbind(matrix(1,nrow = t, ncol = dims[2]), imagesList[[i]]$image, matrix(1,nrow = b, ncol = dims[2]))
    imagesList[[i]]$image = cbind(matrix(1,ncol = l, nrow = dim(imagesList[[i]]$image)[1]), imagesList[[i]]$image, matrix(1,ncol = r, nrow = dim(imagesList[[i]]$image)[1]))
    
    imagesList[[i]]$image = imagesList[[i]]$image[,-c(1,maxImageSize + 2)]
    imagesList[[i]]$image = imagesList[[i]]$image[-c(1,maxImageSize + 2),]
  }
  #apply(matrix(unlist(lapply(imagesList, function(x){dim(x$image)})), ncol = 2, byrow = TRUE), 2, function(x){all(x == maxImageSize)})
  
  
  images = array(NA, c(maxImageSize, maxImageSize, length(imagesList)))
  for(i in 1:length(imagesList)){
    images[,,i] = imagesList[[i]]$image
  }
  return(images)
}

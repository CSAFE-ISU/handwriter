modelquantities_wrappedcauchy_share = function(bucketData, measData, a, b, c, d, e)
{
  dat = list(Y = bucketData[,-c(1,2)],         # multinomial data
             G = ncol(bucketData[,-c(1,2)]),   # number of clusters (40)
             D = nrow(bucketData[,-c(1,2)]),   # total number of documents
             W = length(unique(bucketData$writer)), # number of unique writers
             #docwise
             docN = apply(bucketData[,-c(1,2)], FUN = sum, MARGIN =  1),  # number of letters in each doc, e.g. N[1] = 354
             docwriter = as.numeric(as.factor(bucketData$writer)),  # vector of writers for each document
             #letterwise
             zero_vec = rep(0, times = length(measData$pc_wrapped)),
             ones = rep(1, times = length(measData$pc_wrapped)),
             Gsmall = length(unique(measData$cluster)), # number of clusters (20)
             numletters = length(measData$pc_wrapped), # total number of letters 
             pc_wrapped = measData$pc_wrapped, #principal component rotation observations
             letterwriter = as.numeric(measData$writer), #vector of writers for each letter
             lettercluster = as.numeric(measData$cluster), #vector of cluster assignments, one for each letter
             zero_mat = matrix(0, nrow = length(unique(bucketData$writer)), ncol = length(unique(measData$cluster))),
             a = a, b = b, c = c, d = d, e = e)
  
  return(dat)
}



modelquantities_wrappedcauchy_share_charwriters = function(bucketData, measData, a, b, c, d, e)
{
  dat = list(Y = bucketData[,-c(1,2)],         # multinomial data
             G = ncol(bucketData[,-c(1,2)]),   # number of clusters (40)
             D = nrow(bucketData[,-c(1,2)]),   # total number of documents
             W = length(unique(bucketData$writer)), # number of unique writers
             #docwise
             docN = apply(bucketData[,-c(1,2)], FUN = sum, MARGIN =  1),  # number of letters in each doc, e.g. N[1] = 354
             docwriter = as.numeric(as.factor(bucketData$writer)),  # vector of writers for each document
             #letterwise
             zero_vec = rep(0, times = length(measData$pc_wrapped)),
             ones = rep(1, times = length(measData$pc_wrapped)),
             Gsmall = length(unique(measData$cluster)), # number of clusters (20)
             numletters = length(measData$pc_wrapped), # total number of letters 
             pc_wrapped = measData$pc_wrapped, #principal component rotation observations
             letterwriter = as.numeric(as.factor(measData$writer)), #vector of writers for each letter
             lettercluster = as.numeric(measData$cluster), #vector of cluster assignments, one for each letter
             zero_mat = matrix(0, nrow = length(unique(bucketData$writer)), ncol = length(unique(measData$cluster))),
             a = a, b = b, c = c, d = d, e = e)
  
  return(dat)
}




modelquantities_wrappedcauchy_gammaloops = function(bucketData, measData, clustersinmodel_names)
{
  loopratiodata = measData %>% 
    mutate(loop1_ratio = loop1_longdist/loop1_shortdist, 
           loop2_ratio = loop2_longdist/loop2_shortdist, 
           loop3_ratio = loop3_longdist/loop3_shortdist) %>% 
    pivot_longer(cols = c(loop1_ratio, loop2_ratio, loop3_ratio), names_to = "loop_ratio_num", values_to = "loop_ratio") %>%
    filter(!is.na(loop_ratio)) %>% 
    filter(!is.infinite(loop_ratio)) %>%
    filter(loop_ratio >= 1) %>%
    select(writer, cluster, doc, loopcount, loop_ratio_num, loop_ratio)
  
  loopratiodata$loop_ratio[loopratiodata$loop_ratio == 1] = loopratiodata$loop_ratio[loopratiodata$loop_ratio == 1] + 0.01
  
  measData = measData %>% filter(cluster %in% clustersinmodel_names) %>% mutate(cluster = factor(cluster, levels = levels(clustersinmodel_names)))
  
  dat = list(Y = bucketData[,-c(1,2)],         # multinomial data
             G = ncol(bucketData[,-c(1,2)]),   # number of clusters (40)
             D = nrow(bucketData[,-c(1,2)]),   # total number of documents
             W = length(unique(bucketData$writer)), # number of unique writers
             #docwise
             docN = apply(bucketData[,-c(1,2)], FUN = sum, MARGIN =  1),  # number of letters in each doc, e.g. N[1] = 354
             docwriter = as.numeric(as.factor(bucketData$writer)),  # vector of writers for each document
             #letterwise
             zeros = rep(0, times = length(measData$pc_wrapped)),
             Gsmall = length(unique(measData$cluster)), # number of clusters (20)
             numletters = length(measData$pc_wrapped), # total number of letters 
             pc_wrapped = measData$pc_wrapped, #principal component rotation observations
             letterwriter = as.numeric(measData$writer), #vector of writers for each letter
             lettercluster = as.numeric(measData$cluster), #vector of cluster assignments, one for each letter
             a = 2, b = 0.25,
             LR = log(loopratiodata$loop_ratio),
             loopwriter = as.numeric(as.factor(loopratiodata$writer)),
             numloops = length(loopratiodata$loop_ratio))
  
  return(dat)
}



#' Prepare test data for posterior analysis
#'
#' @param bucketData test bucket data from same \code{getdata_mb} call as train data
#' @param writers a vector of writers, if \code{writerIndices = TRUE} then this is a vector of indices, otherwise it is writer IDs.
#' @param writerIndices see \code{writers} above
#'
#' @export
#'
qdocs_mixture = function(bucketData, writers = 1:27, writerIndices = TRUE){
  if(writerIndices){
    newdocs1 = bucketData[bucketData$writer %in% unique(bucketData$writer)[writers],]
  }
  
  if(!writerIndices){
    newdocs1 = bucketData[bucketData$writer %in% writers,]
  }
  
  newdocs = as.data.frame(newdocs1[,-c(1,2)])
  rownames(newdocs) = newdocs1$writer
  return(newdocs)
}

#' Prepare test data for posterior analysis
#'
#' @param bucketData test bucket data from same \code{getdata_mb} call as train data
#' @param writers a vector of writers, if \code{writerIndices = TRUE} then this is a vector of indices, otherwise it is writer IDs.
#' @param writerIndices see \code{writers} above
#'
#' @export
#'
qdocs_normslope = function(bucketData, measData, writers = 1:27, writerIndices = TRUE){
  if(writerIndices){
    bd = bucketData %>% filter(writer %in% unique(bucketData$writer)[writers])
    md = measData %>% filter(writer %in% unique(measData$writer)[writers])
  }
  
  if(!writerIndices){
    bd = bucketData %>% filter(writer %in% writers)
    md = measData %>% filter(writer %in% writers)
  }
  
  rownames(bd) = bd$writer
  
  newdocs = list()
  newdocs[["bucketData"]] = bd
  newdocs[["measData"]] = md
  return(newdocs)
}


#' Prepare test data for posterior analysis
#'
#' @param bucketData test bucket data from same \code{getdata_mb} call as train data
#' @param writers a vector of writers, if \code{writerIndices = TRUE} then this is a vector of indices, otherwise it is writer IDs.
#' @param writerIndices see \code{writers} above
#'
#' @export
#'
qdocs_normslope_mean = function(bucketData, measData, writers = 1:27, writerIndices = TRUE){
  meanslope_dat = measData %>% group_by(writer, doc, cluster) %>% summarise(meanslope = mean(slope)) %>% as.data.frame()
  if(writerIndices){
    bd = bucketData %>% filter(writer %in% unique(bucketData$writer)[writers])
    md = meanslope_dat %>% filter(writer %in% unique(measData$writer)[writers])
  }
  
  if(!writerIndices){
    bd = bucketData %>% filter(writer %in% writers)
    md = meanslope_dat %>% filter(writer %in% writers)
  }
  
  md = md %>% spread(key = cluster, value = meanslope)
  
  rownames(bd) = bd$writer
  
  newdocs = list()
  newdocs[["bucketData"]] = bd
  newdocs[["measData"]] = md
  return(newdocs)
}


qdocs_pcrot = function(bucketData, measData, writers, writerIndices = TRUE, clustersinmodel_names){
  #measData = data$test$measData
  measData = measData %>% filter(cluster %in% clustersinmodel_names) %>% filter(pc_rotation != 0)
  if(writerIndices){
    bd = bucketData %>% filter(writer %in% unique(bucketData$writer)[writers])
    md = mean_dat %>% filter(writer %in% unique(measData$writer)[writers])
  }
  
  if(!writerIndices){
    bd = bucketData %>% filter(writer %in% writers)
    md = measData %>% filter(writer %in% writers)
  }
  
  rownames(bd) = bd$writer
  
  newdocs = list()
  newdocs[["bucketData"]] = bd
  newdocs[["measData"]] = md
  return(newdocs)
}



qdocs_wrappedcauchy = function(bucketData, measData, writers, writerIndices = TRUE, clustersinmodel_names = NULL){ # if NULL all clusters will be included
  
  if(!is.null(clustersinmodel_names)) measData = measData %>% filter(cluster %in% clustersinmodel_names) %>% mutate(cluster = factor(cluster, levels = levels(clustersinmodel_names)))
  
  measData$cluster = as.numeric(measData$cluster)
  if(writerIndices){bd = bucketData %>% filter(writer %in% unique(bucketData$writer)[writers]); md = measData %>% filter(writer %in% unique(measData$writer)[writers])}
  if(!writerIndices){bd = bucketData %>% filter(writer %in% writers); md = measData %>% filter(writer %in% writers)}
  
  rownames(bd) = bd$writer
  return(list(bucketData = bd, measData = md))
}


qdocs_wrappedcauchy_gammaloops = function(bucketData, measData, writers, writerIndices = TRUE, clustersinmodel_names = NULL){ # if NULL all clusters will be included
  
  loopratiodata = measData %>% 
    mutate(loop1_ratio = loop1_longdist/loop1_shortdist, 
           loop2_ratio = loop2_longdist/loop2_shortdist, 
           loop3_ratio = loop3_longdist/loop3_shortdist) %>% 
    pivot_longer(cols = c(loop1_ratio, loop2_ratio, loop3_ratio), names_to = "loop_ratio_num", values_to = "loop_ratio") %>%
    filter(!is.na(loop_ratio)) %>% 
    filter(!is.infinite(loop_ratio)) %>%
    filter(loop_ratio >= 1) %>%
    select(writer, cluster, doc, loopcount, loop_ratio_num, loop_ratio)
  
  loopratiodata$loop_ratio[loopratiodata$loop_ratio == 1] = loopratiodata$loop_ratio[loopratiodata$loop_ratio == 1] + 0.01
  
  if(!is.null(clustersinmodel_names)) measData = measData %>% filter(cluster %in% clustersinmodel_names) %>% mutate(cluster = factor(cluster, levels = levels(clustersinmodel_names)))
  
  measData$cluster = as.numeric(measData$cluster)
  if(writerIndices){
    bd = bucketData %>% filter(writer %in% unique(bucketData$writer)[writers])
    md = measData %>% filter(writer %in% unique(measData$writer)[writers])
    ld = loopratiodata %>% filter(writer %in% unique(loopratiodata$writer)[writers])
  }
  if(!writerIndices){
    bd = bucketData %>% filter(writer %in% writers)
    md = measData %>% filter(writer %in% writers)
    ld = loopratiodata %>% filter(writer %in% writers)
  }
  
  #rownames(bd) = bd$writer
  return(list(bucketData = bd, measData = md, loopData = ld))
}





angle <- function(N,M){                                # gives a value in (-pi,pi), where negative values come from unit vectors below the x axis (kinda weird/not traditional)
  theta = atan2(N[2],N[1]) - atan2(M[2],M[1])          # https://stackoverflow.com/questions/1897704/angle-between-two-vectors-in-r
  ifelse(theta>0,as.numeric(theta),theta+pi)
}

getloopvals = function(charfeat){
  loopvals = rep(NA,6)
  if(charfeat$loopInfo$loopCount > 0){
    for(i in 1:min(charfeat$loopInfo$loopCount,3)){
      x1 = charfeat$loopInfo$loopPlottingInfo[[i]]$shortestLine$P1[1]
      y1 = charfeat$loopInfo$loopPlottingInfo[[i]]$shortestLine$P1[2]
      x2 = charfeat$loopInfo$loopPlottingInfo[[i]]$shortestLine$P2[1]
      y2 = charfeat$loopInfo$loopPlottingInfo[[i]]$shortestLine$P2[2]
      shortlength = sqrt((x2-x1)^2+(y2-y1)^2)
      
      x1 = charfeat$loopInfo$loopPlottingInfo[[i]]$longestLine$P1[1]
      y1 = charfeat$loopInfo$loopPlottingInfo[[i]]$longestLine$P1[2]
      x2 = charfeat$loopInfo$loopPlottingInfo[[i]]$longestLine$P2[1]
      y2 = charfeat$loopInfo$loopPlottingInfo[[i]]$longestLine$P2[2]
      longlength = sqrt((x2-x1)^2+(y2-y1)^2)
      
      loopvals[i*2-1] = shortlength
      loopvals[i*2] = longlength
    }
  }
  return(loopvals)
}

proclistToData_csafe = function(proclist, testdoc, traindocs, backuptest, oldtrain)
{
  docnames = sapply(proclist, function(x){substr(x$docname, start = 7, stop = 18) %in% c(testdoc, traindocs, backuptest)})
  proclist = proclist[docnames]
  
  writers = as.numeric(unlist(lapply(proclist, function(x){substr(x$docname, start = 2, stop = 5)})))
  doc = unlist(lapply(proclist, function(x){substr(x$docname, start = 7, stop = 18)}))
  session = as.numeric(unlist(lapply(proclist, function(x){substr(x$docname, start = 8, stop = 9)})))
  prompt = unlist(lapply(proclist, function(x){substr(x$docname, start = 12, stop = 14)}))
  rep = as.numeric(unlist(lapply(proclist, function(x){substr(x$docname, start = 17, stop = 18)})))
  
  for(j in 1:length(proclist)){
    featuresPerLetter = c("writer", "doc", "cluster", "letterbucket")
    letter_measurements = data.frame(matrix(ncol = length(featuresPerLetter), nrow = length(proclist[[j]]$process$letterList)))
    names(letter_measurements) = featuresPerLetter
    letter_measurements$writer = writers[j]
    letter_measurements$doc = doc[j]
    letter_measurements$letterbucket = unlist(lapply(proclist[[j]]$process$letterList, function(x){paste0(length(x$nodes),"_",x$letterCode)}))
    letter_measurements$cluster = sapply(proclist[[j]]$process$letterList, function(x){x$cluster})
    letter_measurements$slope = sapply(proclist[[j]]$process$letterList, function(x){x$characterFeatures$slope})
    letter_measurements$pc_rotation = sapply(proclist[[j]]$process$letterList, function(x){xv = x$characterFeatures$xvar; yv = x$characterFeatures$yvar; cv = x$characterFeatures$covar; eig = eigen(cbind(c(xv, cv), c(cv, yv)), symmetric = TRUE); return(angle(t(as.matrix(eig$vectors[,1])), as.matrix(c(1,0))))});
    letter_measurements$pc_wrapped = letter_measurements$pc_rotation*2
    
    if(j == 1){
      data = letter_measurements
    } else {
      data = rbind(data, letter_measurements)
    }
    # print(j)  # SR commented out
  }
  
  ## Y and Ytest
  if(length(testdoc) == 1){
    Y = data[data$doc %in% traindocs,]
    for(i in 1:length(unique(writers))){
      temp = data %>% filter(writer == unique(writer)[i], doc == testdoc)
      if(nrow(temp) == 0){temp = data %>% filter(writer == unique(writer)[i], doc == backuptest)}
      if(i==1){Ytest = temp}else{Ytest = rbind(Ytest,temp)}
    }
  }
  
  Y1 <- Y %>% group_by(writer, doc, cluster) %>%
    summarise(n = n()) %>%
    tidyr::spread(key = cluster, value = n, fill = 0)
  
  Ybuckets = as.data.frame(Y1[,names(oldtrain$bucketData)])
  
  Ytest_1 <- Ytest %>% group_by(writer, doc, cluster) %>%
    summarise(n = n()) %>%
    tidyr::spread(key = cluster, value = n, fill = 0)
  
  Ybuckets_test <- as.data.frame(Ytest_1[,names(oldtrain$bucketData)])
  
  # Measurement Data
  # Train  
  M1 = Y %>% select(writer, doc, cluster, slope, pc_rotation, pc_wrapped) %>% mutate(writer = factor(writer, levels = unique(writer)), cluster = factor(cluster, levels = names(Ybuckets[,-c(1,2)])))
  M = M1[with(M1, order(writer, doc, cluster)),]
  
  # Test
  M1test = Ytest %>% select(writer, doc, cluster, slope, pc_rotation, pc_wrapped) %>% mutate(writer = factor(writer, levels = unique(writer)), cluster = factor(cluster, levels = names(Ybuckets[,-c(1,2)])))
  Mtest = M1test[with(M1test, order(writer, doc, cluster)),]
  
  train = list(bucketData = Ybuckets, measData = M); test <- list(bucketData = Ybuckets_test, measData = Mtest)
  
  data = list(train = train, test = test)
  return(data)
}


proclistToData_csafe_testonly = function(proclist, oldtrain, testdoc, backuptest)
{
  docnames <- sapply(proclist, function(x){substr(x$docname, start = 7, stop = 18) %in% c(testdoc, backuptest)})
  proclist = proclist[docnames]
  
  writers = as.numeric(unlist(lapply(proclist, function(x){substr(x$docname, start = 2, stop = 5)})))
  doc = unlist(lapply(proclist, function(x){substr(x$docname, start = 7, stop = 18)}))
  session = as.numeric(unlist(lapply(proclist, function(x){substr(x$docname, start = 8, stop = 9)})))
  prompt = unlist(lapply(proclist, function(x){substr(x$docname, start = 12, stop = 14)}))
  rep = as.numeric(unlist(lapply(proclist, function(x){substr(x$docname, start = 17, stop = 18)})))
  
  for(j in 1:length(proclist)){
    featuresPerLetter = c("writer", "doc", "cluster", "letterbucket")
    letter_measurements = data.frame(matrix(ncol = length(featuresPerLetter), nrow = length(proclist[[j]]$process$letterList)))
    names(letter_measurements) = featuresPerLetter
    letter_measurements$writer = writers[j]
    letter_measurements$doc = doc[j]
    letter_measurements$letterbucket = unlist(lapply(proclist[[j]]$process$letterList, function(x){paste0(length(x$nodes),"_",x$letterCode)}))
    letter_measurements$cluster = sapply(proclist[[j]]$process$letterList, function(x){x$cluster})
    letter_measurements$slope = sapply(proclist[[j]]$process$letterList, function(x){x$characterFeatures$slope})
    letter_measurements$pc_rotation = sapply(proclist[[j]]$process$letterList, function(x){xv = x$characterFeatures$xvar; yv = x$characterFeatures$yvar; cv = x$characterFeatures$covar; eig = eigen(cbind(c(xv, cv), c(cv, yv)), symmetric = TRUE); return(angle(t(as.matrix(eig$vectors[,1])), as.matrix(c(1,0))))});
    letter_measurements$pc_wrapped = letter_measurements$pc_rotation*2
    
    if(j == 1){
      data = letter_measurements
    } else {
      data = rbind(data, letter_measurements)
    }
    print(j)
  }
  
  ## Y and Ytest
  if(length(testdoc) == 1){
    for(i in 1:length(unique(writers))){
      temp = data %>% filter(writer == unique(writer)[i], doc == testdoc)
      if(nrow(temp) == 0){temp = data %>% filter(writer == unique(writer)[i], doc == backuptest)}
      if(i==1){Ytest = temp}else{Ytest = rbind(Ytest,temp)}
    }
  }
  
  Ytest_1 <- Ytest %>% group_by(writer, doc, cluster) %>%
    summarise(n = n()) %>%
    tidyr::spread(key = cluster, value = n, fill = 0)
  
  Ybuckets_test <- as.data.frame(Ytest_1[,names(oldtrain$bucketData)])
  
  # Measurement Data
  M1test = Ytest %>% select(writer, doc, cluster, slope, pc_rotation, pc_wrapped) %>% mutate(writer = factor(writer, levels = unique(writer)), cluster = factor(cluster, levels = names(oldtrain$bucketData[,-c(1,2)])))
  Mtest = M1test[with(M1test, order(writer, doc, cluster)),]
  
  test <- list(bucketData = Ybuckets_test, measData = Mtest)
  return(test)
}



proclistToData_cvl_testonly = function(proclist, testdoc, backuptest, oldtrain)
{
  docnames = sapply(proclist, function(x){substr(x$docname, start = 6, stop = 6) %in% c(testdoc, traindocs, backuptest)})
  proclist = proclist[docnames]
  
  writers = as.numeric(sapply(proclist, function(x){substr(x$docname, start = 0, stop = 4)}))
  doc = sapply(proclist, function(x){substr(x$docname, start = 6, stop = 6)})
  
  for(j in 1:length(proclist)){
    featuresPerLetter = c("writer", "doc", "cluster", "letterbucket")
    letter_measurements = data.frame(matrix(ncol = length(featuresPerLetter), nrow = length(proclist[[j]]$process$letterList)))
    names(letter_measurements) = featuresPerLetter
    letter_measurements$writer = writers[j]
    letter_measurements$doc = doc[j]
    letter_measurements$letterbucket = unlist(lapply(proclist[[j]]$process$letterList, function(x){paste0(length(x$nodes),"_",x$letterCode)}))
    letter_measurements$cluster = sapply(proclist[[j]]$process$letterList, function(x){x$cluster})
    letter_measurements$slope = sapply(proclist[[j]]$process$letterList, function(x){x$characterFeatures$slope})
    letter_measurements$pc_rotation = sapply(proclist[[j]]$process$letterList, function(x){xv = x$characterFeatures$xvar; yv = x$characterFeatures$yvar; cv = x$characterFeatures$covar; eig = eigen(cbind(c(xv, cv), c(cv, yv)), symmetric = TRUE); return(angle(t(as.matrix(eig$vectors[,1])), as.matrix(c(1,0))))});
    letter_measurements$pc_wrapped = letter_measurements$pc_rotation*2
    
    if(j == 1){
      data = letter_measurements
    } else {
      data = rbind(data, letter_measurements)
    }
    print(j)
  }
  
  ## Ytest
  if(length(testdoc) == 1){
    for(i in 1:length(unique(writers))){
      temp = data %>% filter(writer == unique(writer)[i], doc == testdoc)
      if(nrow(temp) == 0){temp = data %>% filter(writer == unique(writer)[i], doc == backuptest)}
      if(i==1){Ytest = temp}else{Ytest = rbind(Ytest,temp)}
    }
  }
  
  Ytest_1 <- Ytest %>% group_by(writer, doc, cluster) %>%
    summarise(n = n()) %>%
    tidyr::spread(key = cluster, value = n, fill = 0)
  
  Ybuckets_test <- as.data.frame(Ytest_1[,names(oldtrain$bucketData)])
  
  # Measurement Data
  M1test = Ytest %>% select(writer, doc, cluster, slope, pc_rotation, pc_wrapped) %>% mutate(writer = factor(writer, levels = unique(writer)), cluster = factor(cluster, levels = names(oldtrain$bucketData[,-c(1,2)])))
  Mtest = M1test[with(M1test, order(writer, doc, cluster)),]
  
  test <- list(bucketData = Ybuckets_test, measData = Mtest)
  
  return(test)
}





proclistToData_cvl = function(proclist, testdoc, traindocs, backuptest, oldtrain)
{
  docnames = sapply(proclist, function(x){substr(x$docname, start = 6, stop = 6) %in% c(testdoc, traindocs, backuptest)})
  proclist = proclist[docnames]
  
  writers = as.numeric(sapply(proclist, function(x){substr(x$docname, start = 0, stop = 4)}))
  doc = sapply(proclist, function(x){substr(x$docname, start = 6, stop = 6)})
  
  for(j in 1:length(proclist)){
    featuresPerLetter = c("writer", "doc", "cluster", "letterbucket")
    letter_measurements = data.frame(matrix(ncol = length(featuresPerLetter), nrow = length(proclist[[j]]$process$letterList)))
    names(letter_measurements) = featuresPerLetter
    letter_measurements$writer = writers[j]
    letter_measurements$doc = doc[j]
    letter_measurements$letterbucket = unlist(lapply(proclist[[j]]$process$letterList, function(x){paste0(length(x$nodes),"_",x$letterCode)}))
    letter_measurements$cluster = sapply(proclist[[j]]$process$letterList, function(x){x$cluster})
    letter_measurements$slope = sapply(proclist[[j]]$process$letterList, function(x){x$characterFeatures$slope})
    letter_measurements$pc_rotation = sapply(proclist[[j]]$process$letterList, function(x){xv = x$characterFeatures$xvar; yv = x$characterFeatures$yvar; cv = x$characterFeatures$covar; eig = eigen(cbind(c(xv, cv), c(cv, yv)), symmetric = TRUE); return(angle(t(as.matrix(eig$vectors[,1])), as.matrix(c(1,0))))});
    letter_measurements$pc_wrapped = letter_measurements$pc_rotation*2
    
    if(j == 1){
      data = letter_measurements
    } else {
      data = rbind(data, letter_measurements)
    }
    print(j)
  }
  
  ## Y and Ytest
  if(length(testdoc) == 1){
    Y = data[data$doc %in% traindocs,]
    for(i in 1:length(unique(writers))){
      temp = data %>% filter(writer == unique(writer)[i], doc == testdoc)
      if(nrow(temp) == 0){temp = data %>% filter(writer == unique(writer)[i], doc == backuptest)}
      if(i==1){Ytest = temp}else{Ytest = rbind(Ytest,temp)}
    }
  }
  
  Y1 <- Y %>% group_by(writer, doc, cluster) %>%
    summarise(n = n()) %>%
    tidyr::spread(key = cluster, value = n, fill = 0)
  
  Ybuckets = as.data.frame(Y1[,names(oldtrain$bucketData)])
  
  Ytest_1 <- Ytest %>% group_by(writer, doc, cluster) %>%
    summarise(n = n()) %>%
    tidyr::spread(key = cluster, value = n, fill = 0)
  
  Ybuckets_test <- as.data.frame(Ytest_1[,names(oldtrain$bucketData)])
  
  # Measurement Data
  # Train  
  M1 = Y %>% select(writer, doc, cluster, slope, pc_rotation, pc_wrapped) %>% mutate(writer = factor(writer, levels = unique(writer)), cluster = factor(cluster, levels = names(Ybuckets[,-c(1,2)])))
  M = M1[with(M1, order(writer, doc, cluster)),]
  
  # Test
  M1test = Ytest %>% select(writer, doc, cluster, slope, pc_rotation, pc_wrapped) %>% mutate(writer = factor(writer, levels = unique(writer)), cluster = factor(cluster, levels = names(Ybuckets[,-c(1,2)])))
  Mtest = M1test[with(M1test, order(writer, doc, cluster)),]
  
  train = list(bucketData = Ybuckets, measData = M); test <- list(bucketData = Ybuckets_test, measData = Mtest)
  
  data = list(train = train, test = test)
  return(data)
}




proclistToData_iam = function(proclist, numtest, numtrain, oldtrain, seed){
  s_parse = sapply(proclist, function(x) strsplit(x$docname, split = "-"))
  form_id = s_w_form = line_id = writer_id = c()
  for(i in 1:length(s_parse)){
    form_id[i] = paste(s_parse[[i]][1], s_parse[[i]][2], sep = "-")
    s_w_form[i] = s_parse[[i]][3]
    line_id[i] = substr(s_parse[[i]][4], start = 1, stop = 2)
    writer_id[i] = substr(s_parse[[i]][4], start = 5, stop = 8)
  }
  num_graphs = sapply(proclist, function(x) length(x$process$letterList))
  df = data.frame(form_id, s_w_form, line_id, writer_id, sentence_num = as.numeric(as.factor(paste(form_id, s_w_form, writer_id, sep = "-"))), num_graphs)
  df = df %>% 
    group_by(writer_id) %>% 
    mutate(s_w_writer = as.numeric(as.factor(sentence_num)), number_sent_bywriter = length(unique(sentence_num))) %>% 
    ungroup() %>% 
    group_by(writer_id, s_w_writer, sentence_num) %>% 
    mutate(numgraphsinsentence = sum(num_graphs)) %>% 
    ungroup() %>% as.data.frame()
  
  set.seed(seed)
  
  train_test_split = matrix(nrow = length(unique(df$writer_id)), ncol = max(df$s_w_writer), byrow = TRUE)
  for(i in 1:length(unique(df$writer_id))){
    train_test_split[i,] = sample(1:max(df$s_w_writer), max(df$s_w_writer))
  }
  
  split = c()
  for(k in 1:length(proclist)){
    if(df$s_w_writer[k] %in% train_test_split[as.numeric(as.factor(df$writer_id[k])),1:numtrain]){
      split[k] = "train"
    } else if(df$s_w_writer[k] %in% train_test_split[as.numeric(as.factor(df$writer_id[k])),(numtrain+1):(numtest+numtrain)]){
      split[k] = "test"
    } else{
      split[k] = NA
    }
    #split[k] = ifelse(df$s_w_writer[k] %in% train_test_split[as.numeric(as.factor(df$writer_id[k])),1:numtrain], "train", ifelse(df$s_w_writer[k] %in% train_test_split[as.numeric(as.factor(df$writer_id[k])), numtrain+1:numtest], "test", "none"))
  }
  
  for(j in 1:length(proclist)){
    featuresPerLetter = c("writer")
    letter_measurements = data.frame(matrix(ncol = length(featuresPerLetter), nrow = length(proclist[[j]]$process$letterList)))
    names(letter_measurements) = featuresPerLetter
    letter_measurements$writer = df$writer_id[j]
    letter_measurements$split = split[j]
    letter_measurements$sentence_num = df$sentence_num[j]
    letter_measurements$form_id = df$form_id[j]
    letter_measurements$sent_w_form = df$s_w_form[j]
    letter_measurements$cluster = sapply(proclist[[j]]$process$letterList, function(x){x$cluster})
    letter_measurements$pc_rotation = sapply(proclist[[j]]$process$letterList, function(x){xv = x$characterFeatures$xvar; yv = x$characterFeatures$yvar; cv = x$characterFeatures$covar; eig = eigen(cbind(c(xv, cv), c(cv, yv)), symmetric = TRUE); return(angle(t(as.matrix(eig$vectors[,1])), as.matrix(c(1,0))))});
    letter_measurements$pc_wrapped = letter_measurements$pc_rotation*2
    
    if(j == 1){
      data = letter_measurements
    } else {
      data = rbind(data, letter_measurements)
    }
    if(j%%100==0) print(j)
  }
  
  ## 
  Y = data %>% filter(split == "train")
  Ytest = data %>% filter(split == "test")
  
  Ygrouped = Y %>% 
    group_by(writer) %>% 
    mutate(sentence_in_docgroup = as.numeric(as.factor(sentence_num))) %>% 
    ungroup() %>% 
    mutate(docgroups = ifelse(sentence_in_docgroup %in% 1:4, 1, ifelse(sentence_in_docgroup %in% 5:8, 2, ifelse(sentence_in_docgroup %in% 9:12, 3, ifelse(sentence_in_docgroup %in% 13:16, 4, NA))) )) %>%
    as.data.frame()
  
  #temp = Ygrouped %>% group_by(writer, sentence_num) %>% summarize(sentence_in_docgroup = mean(sentence_in_docgroup)) %>% as.data.frame()
  
  Y1 <- Ygrouped %>% group_by(writer, docgroups, cluster) %>%
    summarise(n = n()) %>%
    tidyr::spread(key = cluster, value = n, fill = 0)
  
  Ybuckets = as.data.frame(Y1[,c("writer", "docgroups", names(oldtrain$bucketData)[-c(1,2)])])
  
  Ytest_1 <- Ytest %>% group_by(writer, cluster) %>%
    summarise(n = n()) %>%
    tidyr::spread(key = cluster, value = n, fill = 0)
  
  Ybuckets_test <- cbind(placeholder = 1, as.data.frame(Ytest_1[,c("writer", names(oldtrain$bucketData)[-c(1,2)])]))
  
  # Measurement Data
  # Train  
  M1 = Y %>% select(writer, form_id, sent_w_form, cluster, pc_rotation, pc_wrapped) %>% mutate(writer = factor(writer, levels = unique(writer)), cluster = factor(cluster, levels =  names(oldtrain$bucketData)[-c(1,2)]))
  M = M1[with(M1, order(writer, cluster)),]
  
  # Test
  M1test = Ytest %>% select(writer,  form_id, sent_w_form, cluster, pc_rotation, pc_wrapped) %>% mutate(writer = factor(writer, levels = unique(writer)), cluster = factor(cluster, levels =  names(oldtrain$bucketData)[-c(1,2)]))
  Mtest = M1test[with(M1test, order(writer, cluster)),]
  
  train = list(bucketData = Ybuckets, measData = M); test <- list(bucketData = Ybuckets_test, measData = Mtest)
  
  split_info = data.frame(df$writer_id, df$form_id, df$s_w_form, df$s_w_writer, split)
  
  data = list(train = train, test = test, split_info = split_info)
  return(data)
}




proclistToData_iam_norandom = function(proclist, numtest, numtrain = 16, oldtrain){
  s_parse = sapply(proclist, function(x) strsplit(x$docname, split = "-"))
  form_id = s_w_form = line_id = writer_id = c()
  for(i in 1:length(s_parse)){
    form_id[i] = paste(s_parse[[i]][1], s_parse[[i]][2], sep = "-")
    s_w_form[i] = s_parse[[i]][3]
    line_id[i] = substr(s_parse[[i]][4], start = 1, stop = 2)
    writer_id[i] = substr(s_parse[[i]][4], start = 5, stop = 8)
  }
  num_graphs = sapply(proclist, function(x) length(x$process$letterList))
  df = data.frame(form_id, s_w_form, line_id, writer_id, sentence_num = as.numeric(as.factor(paste(form_id, s_w_form, writer_id, sep = "-"))), num_graphs)
  
  df = df %>%
    group_by(writer_id, sentence_num) %>%
    mutate(numgraphs_sentence = sum(num_graphs)) %>%
    ungroup() %>%
    mutate(s_length_order = rank(-1*numgraphs_sentence, ties.method = "first")) %>%
    group_by(writer_id, sentence_num) %>%
    mutate(sent_order = mean(s_length_order)) %>%
    ungroup() %>%
    group_by(writer_id) %>%
    mutate(sent_order_w_writer = as.numeric(as.factor(sent_order))) %>%
    mutate(s_w_writer = as.numeric(as.factor(sentence_num)), number_sent_bywriter = length(unique(sentence_num))) %>% 
    ungroup() %>%
    group_by(writer_id, sent_order_w_writer) %>%
    as.data.frame()
  
  df$split = ifelse(df$sent_order_w_writer %in% 1:numtrain, "train", ifelse(df$sent_order_w_writer %in% (max(df$sent_order_w_writer)-numtest + 1):max(df$sent_order_w_writer), "test", NA))
  
  # investigate = df %>% group_by(writer_id, numgraphs_sentence, sent_order_w_writer, s_w_writer) %>% summarise(split = unique(split), docg = unique(docgroup)) %>% as.data.frame()
  
  # df = df %>% 
  #   group_by(writer_id) %>% 
  #   mutate(s_w_writer = as.numeric(as.factor(sentence_num)), number_sent_bywriter = length(unique(sentence_num))) %>% 
  #   ungroup() %>% 
  #   group_by(writer_id, s_w_writer, sentence_num) %>% 
  #   mutate(numgraphsinsentence = sum(num_graphs)) %>% 
  #   ungroup() %>% as.data.frame()
  #   
  #df$split = ifelse(df$s_w_writer %in% 1:numtrain, "train", ifelse(df$s_w_writer %in% (numtrain+1):(numtrain+numtest), "test", NA))
  
  for(j in 1:length(proclist)){
    featuresPerLetter = c("writer")
    letter_measurements = data.frame(matrix(ncol = length(featuresPerLetter), nrow = length(proclist[[j]]$process$letterList)))
    names(letter_measurements) = featuresPerLetter
    letter_measurements$writer = df$writer_id[j]
    letter_measurements$split = df$split[j]
    letter_measurements$sentence_num = df$sentence_num[j]
    letter_measurements$form_id = df$form_id[j]
    letter_measurements$sent_w_form = df$s_w_form[j]
    letter_measurements$sent_order_w_writer = df$sent_order_w_writer[j]
    letter_measurements$cluster = sapply(proclist[[j]]$process$letterList, function(x){x$cluster})
    letter_measurements$pc_rotation = sapply(proclist[[j]]$process$letterList, function(x){xv = x$characterFeatures$xvar; yv = x$characterFeatures$yvar; cv = x$characterFeatures$covar; eig = eigen(cbind(c(xv, cv), c(cv, yv)), symmetric = TRUE); return(angle(t(as.matrix(eig$vectors[,1])), as.matrix(c(1,0))))});
    letter_measurements$pc_wrapped = letter_measurements$pc_rotation*2
    
    if(j == 1){
      data = letter_measurements
    } else {
      data = rbind(data, letter_measurements)
    }
    if(j%%100==0) print(j)
  }
  
  ## 
  Y = data %>% filter(split == "train") %>% mutate(docgroups = (sent_order_w_writer+3)%%4 +1)
  Ytest = data %>% filter(split == "test")
  
  # Ygrouped = Y %>%
  #   mutate(docgroups = ifelse(sent_w_writer %in% 1:4, 1, ifelse(sent_w_writer %in% 5:8, 2, ifelse(sent_w_writer %in% 9:12, 3, ifelse(sent_w_writer %in% 13:16, 4, NA))) )) %>%
  #   as.data.frame()
  
  Y1 <- Y %>% group_by(writer, docgroups, cluster) %>%
    summarise(n = n()) %>%
    tidyr::spread(key = cluster, value = n, fill = 0)
  
  Ybuckets = as.data.frame(Y1[,c("writer", "docgroups", names(oldtrain$bucketData)[-c(1,2)])])
  
  Ytest_1 <- Ytest %>% group_by(writer, cluster) %>%
    summarise(n = n()) %>%
    tidyr::spread(key = cluster, value = n, fill = 0)
  
  Ybuckets_test <- cbind(placeholder = 1, as.data.frame(Ytest_1[,c("writer", names(oldtrain$bucketData)[-c(1,2)])]))
  
  # Measurement Data
  # Train  
  M1 = Y %>% select(writer, form_id, sent_w_form, cluster, pc_rotation, pc_wrapped) %>% mutate(writer = factor(writer, levels = levels(Ybuckets$writer)), cluster = factor(cluster, levels =  names(oldtrain$bucketData)[-c(1,2)]))
  M = M1[with(M1, order(writer, cluster)),]
  
  # Test
  M1test = Ytest %>% select(writer,  form_id, sent_w_form, cluster, pc_rotation, pc_wrapped) %>% mutate(writer = factor(writer, levels = levels(Ybuckets$writer)), cluster = factor(cluster, levels =  names(oldtrain$bucketData)[-c(1,2)]))
  Mtest = M1test[with(M1test, order(writer, cluster)),]
  
  train = list(bucketData = Ybuckets, measData = M); test = list(bucketData = Ybuckets_test, measData = Mtest)
  
  data = list(train = train, test = test)
  return(data)
}





proclistToData_iam_norandom_nogrouping = function(proclist, numtest, numtrain = 16, oldtrain){
  s_parse = sapply(proclist, function(x) strsplit(x$docname, split = "-"))
  form_id = s_w_form = line_id = writer_id = c()
  for(i in 1:length(s_parse)){
    form_id[i] = paste(s_parse[[i]][1], s_parse[[i]][2], sep = "-")
    s_w_form[i] = s_parse[[i]][3]
    line_id[i] = substr(s_parse[[i]][4], start = 1, stop = 2)
    writer_id[i] = substr(s_parse[[i]][4], start = 5, stop = 8)
  }
  num_graphs = sapply(proclist, function(x) length(x$process$letterList))
  df = data.frame(form_id, s_w_form, line_id, writer_id, sentence_num = as.numeric(as.factor(paste(form_id, s_w_form, writer_id, sep = "-"))), num_graphs)
  df = df %>% 
    group_by(writer_id) %>% 
    mutate(s_w_writer = as.numeric(as.factor(sentence_num)), number_sent_bywriter = length(unique(sentence_num))) %>% 
    ungroup() %>% 
    group_by(writer_id, s_w_writer, sentence_num) %>% 
    mutate(numgraphsinsentence = sum(num_graphs)) %>% 
    ungroup() %>% as.data.frame()
  
  df$split = ifelse(df$s_w_writer %in% 1:16, "train", ifelse(df$s_w_writer %in% 17:(16+numtest), "test", NA))
  
  for(j in 1:length(proclist)){
    featuresPerLetter = c("writer")
    letter_measurements = data.frame(matrix(ncol = length(featuresPerLetter), nrow = length(proclist[[j]]$process$letterList)))
    names(letter_measurements) = featuresPerLetter
    letter_measurements$writer = df$writer_id[j]
    letter_measurements$split = df$split[j]
    letter_measurements$sentence_num = df$sentence_num[j]
    letter_measurements$form_id = df$form_id[j]
    letter_measurements$sent_w_form = df$s_w_form[j]
    letter_measurements$sent_w_writer = df$s_w_writer[j]
    letter_measurements$cluster = sapply(proclist[[j]]$process$letterList, function(x){x$cluster})
    letter_measurements$pc_rotation = sapply(proclist[[j]]$process$letterList, function(x){xv = x$characterFeatures$xvar; yv = x$characterFeatures$yvar; cv = x$characterFeatures$covar; eig = eigen(cbind(c(xv, cv), c(cv, yv)), symmetric = TRUE); return(angle(t(as.matrix(eig$vectors[,1])), as.matrix(c(1,0))))});
    letter_measurements$pc_wrapped = letter_measurements$pc_rotation*2
    
    if(j == 1){
      data = letter_measurements
    } else {
      data = rbind(data, letter_measurements)
    }
    if(j%%100==0) print(j)
  }
  
  ## 
  Y = data %>% filter(split == "train") #%>% mutate(docgroups = ifelse(sent_w_writer %in% 1:4, 1, ifelse(sent_w_writer %in% 5:8, 2, ifelse(sent_w_writer %in% 9:12, 3, ifelse(sent_w_writer %in% 13:16, 4, NA))) ))
  Ytest = data %>% filter(split == "test")
  
  # Ygrouped = Y %>%
  #   mutate(docgroups = ifelse(sent_w_writer %in% 1:4, 1, ifelse(sent_w_writer %in% 5:8, 2, ifelse(sent_w_writer %in% 9:12, 3, ifelse(sent_w_writer %in% 13:16, 4, NA))) )) %>%
  #   as.data.frame()
  
  Y1 <- Y %>% group_by(writer, sent_w_writer, cluster) %>%
    summarise(n = n()) %>%
    tidyr::spread(key = cluster, value = n, fill = 0)
  
  Ybuckets = as.data.frame(Y1[,c("writer", "sent_w_writer", names(oldtrain$bucketData)[-c(1,2)])])
  
  Ytest_1 <- Ytest %>% group_by(writer, cluster) %>%
    summarise(n = n()) %>%
    tidyr::spread(key = cluster, value = n, fill = 0)
  
  Ybuckets_test <- cbind(placeholder = 1, as.data.frame(Ytest_1[,c("writer", names(oldtrain$bucketData)[-c(1,2)])]))
  
  # Measurement Data
  # Train  
  M1 = Y %>% select(writer, form_id, sent_w_form, cluster, pc_rotation, pc_wrapped) %>% mutate(writer = factor(writer, levels = unique(writer)), cluster = factor(cluster, levels =  names(oldtrain$bucketData)[-c(1,2)]))
  M = M1[with(M1, order(writer, cluster)),]
  
  # Test
  M1test = Ytest %>% select(writer,  form_id, sent_w_form, cluster, pc_rotation, pc_wrapped) %>% mutate(writer = factor(writer, levels = unique(writer)), cluster = factor(cluster, levels =  names(oldtrain$bucketData)[-c(1,2)]))
  Mtest = M1test[with(M1test, order(writer, cluster)),]
  
  train = list(bucketData = Ybuckets, measData = M); test = list(bucketData = Ybuckets_test, measData = Mtest)
  
  data = list(train = train, test = test)
  return(data)
}




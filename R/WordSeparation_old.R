#' add_word_info
#'
#' Associates characters to their respective word numbers by ML on labeled data
#' 
#' @param letterList List containing characters
#' @param dims Dimensions of binary image
#' @return Appends line information to character features
#' 
#' @keywords character features line number
#'
#' @importFrom stats predict
add_word_info = function(letterList, dims){
  
  #Remove any null objects
  j = 1
  while (j < length(letterList) + 1){
    if(is.null(letterList[[j]][[1]])){
      letterList = letterList[-j]
      j = j + 1
    }
    j = j + 1
  }
  
  #Compute the approximate width and height of each line
  dimsList <- list()
  currentLine = 1
  
  left_most = Inf
  right_most = 0
  tallest = 0
  
  for (i in 1:length(letterList)){
    
    left = letterList[[i]]$characterFeatures$leftmost_col
    right = letterList[[i]]$characterFeatures$rightmost_col
    height = letterList[[i]]$characterFeatures$height
    
    
    if(!is.null(left) && left < left_most){
      left_most = left
    }
    
    if(!is.null(right) &&  right > right_most){
      right_most = letterList[[i]]$characterFeatures$rightmost_col
    }
    
    if(!is.null(height) && height > tallest){
      tallest = height
    }
    
    if(letterList[[i]]$characterFeatures$line_number != currentLine | i == length(letterList)) {
      
      dimsList[[currentLine]] <- list(right_most-left_most, tallest)
      
      currentLine = currentLine + 1
      left_most = Inf
      right_most = 0
      tallest = 0
    }
  }
  
  #Case if a single letter is by itself on its own line
  if(length(dimsList) < letterList[[length(letterList)]]$characterFeatures$line_number){
    left_most = letterList[[length(letterList)]]$characterFeatures$leftmost_col
    right_most = letterList[[length(letterList)]]$characterFeatures$rightmost_col
    tallest = letterList[[length(letterList)]]$characterFeatures$height
    dimsList[[currentLine]] <- list(right_most-left_most, tallest)
  }
  
  #Create a new DF and fill it up from each character entry
  dataDF <- data.frame(line=numeric(0),line_height=numeric(0),line_width=numeric(0),height=numeric(0),width=numeric(0),x=numeric(0),label=character(0),stringsAsFactors = FALSE)
  for (i in 1:length(letterList)){
    line = letterList[[i]]$characterFeatures$line_number
    dataDF[nrow(dataDF) + 1,] =
      list(line,
           dimsList[[line]][[2]], dimsList[[line]][[1]],
           letterList[[i]]$characterFeatures$height, letterList[[i]]$characterFeatures$width,
           letterList[[i]]$characterFeatures$leftmost_col)
  }
  
  #Add in proportional info
  for(r in 1:nrow(dataDF)){
    row = dataDF[r,]
    prev_row = dataDF[r-1,]
    next_row = dataDF[r+1,]
    
    dataDF[r, 'height_prop'] = row$height/row$line_height
    dataDF[r, 'width_prop'] = row$width/row$line_width
    
    to_right = next_row$x - (row$x + row$width)
    dataDF[r, 'to_right_prop'] = to_right/row$line_width
    
    if(r==1){next}
    to_left = row$x - (prev_row$x + prev_row$width)
    dataDF[r, 'to_left_prop'] = to_left/row$line_width
  } 
  
  
  #just take the proportional data since that is what our model is based off of
  testDF = dataDF[c("height_prop", "width_prop", "to_right_prop", "to_left_prop")]
  testDF[testDF < 0] = 0
  #testDF[is.infinite(testDF)] <- NA  
  
  testDF=do.call(data.frame, lapply (testDF, function(value) replace(value, is.infinite(value),NA)))
  
  # Make prediction and add to other
  loadNamespace("randomForest")
  wordPredictions <- cbind(testDF, predict(wordModelNew, testDF, type = "class"))
  names(wordPredictions)[names(wordPredictions) == "predict(wordModelNew, testDF, type = \"class\")"] <- "prediction"
  wordPredictions[1, 'prediction']="beginning"
  wordPredictions[nrow(wordPredictions), 'prediction']="end"
  wordPredictions$prediction[is.na(wordPredictions$prediction)] ="end"
  
  #Some manual interventions
  aggregateWordPredictions = aggregate(. ~ prediction, wordPredictions, mean)
  
  beginning_to_left_mean = aggregateWordPredictions[1,'to_left_prop']
  ending_to_right_mean = aggregateWordPredictions[2,'to_right_prop']
  if(is.na(ending_to_right_mean)) {ending_to_right_mean = 0}
  
  wordPredictions$prediction[which(wordPredictions$to_left_prop > beginning_to_left_mean)] = 'beginning'
  wordPredictions$prediction[which(wordPredictions$to_right_prop > ending_to_right_mean)] = 'end'
  
  
  #Now use the predictions to figure out the word boundaries
  wordCount = 1
  lineCount = 1
  holding = NULL
  prediction = NA
  for(i in 1:length(letterList)){
    if(length(letterList) < 10) {wordCount = 1}
    letterList[[i]]$characterFeatures = c(letterList[[i]]$characterFeatures, list(wordIndex = wordCount))
    holding = prediction
    prediction = wordPredictions[i, 'prediction']
    
    #If we are on the last letter don't do any of this (out of bounds)
    if(i == length(letterList)){break}
    
    #keep track of next prediction
    nextPrediction = wordPredictions[i+1, 'prediction']
    
    if(prediction == "middle" & nextPrediction == "beginning"){
      if(wordPredictions[i,'to_right_prop'] > ending_to_right_mean *.5){
        wordPredictions[i, 'prediction'] = 'end'
      }
    }
    else {
      wordPredictions[i+1, 'prediction'] = 'middle'
    }
    
    
    if(prediction == "end" & nextPrediction == "end"){
      wordPredictions[i, 'prediction'] = 'middle'
    }
    
    #Update the prediction to use
    # prediction = wordPredictions[i, 'prediction'] 
    
    #if next char is on a new line, index the word and go to next iteration
    if(letterList[[i+1]]$characterFeatures$line_number > letterList[[i]]$characterFeatures$line_number){
      wordCount = wordCount + 1
      wordPredictions[i, 'prediction'] = 'end'
      wordPredictions[i+1, 'prediction'] = 'beginning'
      next
    }
    prediction = wordPredictions[i, 'prediction'] 
    
    #if we see an end and the next is NOT an end, move on
    if(prediction == "end" & nextPrediction != "end"){
      wordCount = wordCount + 1
      next
    }
  }
  
  return(letterList)
}


# #' add_word_info_old
# #' THIS IS THE OLD VERSION ---- CURRENT VERSION ABOVE
# #' Associates characters to their respective word numbers by distance between right edge of char and left edge of next
# #' Needs improvement if runtime becomes a problem
# #' @param character_features All extracted features
# #' @param dims Dimensions of binary image
# #' @keywords character, features, line number
# #' @return Appends line information to character features

# add_word_info_old = function(letterList, dims){#character_features){
#   
#   #loop that goes through and records distances between rightmost_col and leftmost_col of next
#   dist_between_list = list()
#   right_col = letterList[[1]]$characterFeatures$rightmost_col
#   for(i in 2:length(letterList)){
#     left_col = letterList[[i]]$characterFeatures$leftmost_col
#     dist_between = left_col - right_col
#     dist_between_list <- append(dist_between_list, dist_between)
#     right_col = letterList[[i]]$characterFeatures$rightmost_col                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#   }    
#   dist_between_vec <- unlist(dist_between_list)
#   new_line_threshold = -(dims[2]/2)
#   
#   
#   #find the average of these measurements - here a few ideas on how to calcualte it
#   dist_between_vec <- append(dist_between_vec, c(0))
#   
#   #1: ZERO OUT, TAKE MEAN * 1.5
#   dist_vec_zeroed <- dist_between_vec #save off a zeroed one to find our threshold, keep the real one for processing
#   dist_vec_zeroed[dist_vec_zeroed < 0] <- 0
#   dist_between_mean = mean(dist_vec_zeroed)
#   splitThreshold = dist_between_mean * 1.5
#   
#   #split up the words according to this measurement
#  wordCount = 1
#   for(i in 1:(length(letterList))){
#     letterList[[i]]$characterFeatures = c(letterList[[i]]$characterFeatures, list(wordIndex = wordCount))
#    
#     if(dist_between_vec[[i]] < new_line_threshold){ #CONSULT THE MODEL HERE
#       wordCount = wordCount + 1
#     }
#     
#     if(dist_between_vec[[i]] >= splitThreshold){
#       wordCount = wordCount + 1
#     }
#   }
#   
#   return(letterList)
# }

#' create_words
#'
#' creates word objects based on splits found in processHandwriting
#' 
#' @param processList Output from processHandwriting - contains all glyph information
#' @return list of word objects
#' 
#' @examples 
#' twoSent_document = list()
#' twoSent_document$image = twoSent
#' twoSent_document$thin = thinImage(twoSent_document$image)
#' twoSent_processList = processHandwriting(twoSent_document$thin, dim(twoSent_document$image))
#' 
#' dims = dim(twoSent_document$image)
#' words = create_words(twoSent_processList) 
#' words_after_processing = process_words(words, dim(twoSent_document$image), TRUE)
#' @export
create_words = function(processList){
  message("Creating words..")
  words = list()
  letterList = processList$letterList
  
  allNodes = c()
  wordPath = c()
  terminalNodes = c()
  connectingNodes = c()
  
  wordIndex = 1
  for(i in letterList){
    #if about to go to next word, create all word info and then index, make new empty lists
    if(wordIndex != i$characterFeatures$wordIndex){
      wordsInfo <- list(wordIndex = wordIndex, wordPath = wordPath, allNodes = allNodes, terminalNodes = terminalNodes, connectingNodes = connectingNodes)
      words[[wordIndex]] <- wordsInfo
      
      wordIndex = wordIndex + 1
      allNodes = c()
      wordPath = c()
      terminalNodes = c()
      connectingNodes = c()
    }
    
    #As long as in word, keep apending next graphs info.
    if(wordIndex == i$characterFeatures$wordIndex){
      allNodes <- append(allNodes, i$nodes)
      wordPath <- append(wordPath, i$path)
      terminalNodes <- append(terminalNodes, i$terminalNodes)
      connectingNodes <- append(connectingNodes, i$connectingNodes)
    }
  }
  
  wordsInfo <- list(wordIndex = wordIndex, wordPath = wordPath, allNodes = allNodes, terminalNodes = terminalNodes, connectingNodes = connectingNodes)
  words[[wordIndex]] <- wordsInfo
  
  nameList = list()
  
  #Naming objects
  for (i in 1:length(words)){
    name = paste("wordIndex:",i,sep=" ")
    nameList <- append(nameList, name)
  }
  names(words) <- nameList
  
  return(words)
}

#' make_single_word
#' 
#' @param letterList A list of graphs
#' 
#' @export
make_single_word = function(letterList){
  
  for(i in 1:length(letterList)){
    letterList[[i]]$characterFeatures$wordIndex = 1
  }
  
  return(letterList)
}

#' plotWord
#'
#' This function returns a plot of a single Word extracted from a document. It uses the letterList parameter from the processHandwriting function and accepts a single value as whichLetter. Dims requires the dimensions of the entire document, since this isn't contained in processHandwriting.
#' 
#' @param letterList Letter list from processHandwriting function
#' @param whichWord Single word value denoting which line to plot - checked if too big inside function.
#' @param dims Dimensions of the original document
#' @return Plot of single word.
#' 
#' @examples
#' \dontrun{
#' twoSent_document = list()
#' twoSent_document$image = twoSent
#' twoSent_document$thin = thinImage(twoSent_document$image)
#' twoSent_processList = processHandwriting(twoSent_document$thin, dim(twoSent_document$image))
#' 
#' dims = dim(twoSent_document$image)
#' words = create_words(twoSent_processList) 
#' words_after_processing = process_words(words, dim(twoSent_document$image), TRUE)
#' plotWord(twoSent_processList$letterList, 1, dims)
#' }
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
  #nodeSize = 4
  #nodeColor = "red"
  #pointSet = data.frame(X = ((nodeList - 1) %/% dim(img)[1]) + 1, Y = dim(img)[1] - ((nodeList - 1) %% dim(img)[1]))
  #p = p + geom_point(data = pointSet, aes(X, Y), size = nodeSize, shape = I(16), color = I(nodeColor), alpha = I(.6))
  
  return(p)
}

#' process_words
#'
#' Gets information on a word level
#' 
#' @param words List of words and some glyph level information
#' @param dims The dimensions of the image (important for r/c features)
#' @param triangulate Logical value that begins the triangulation process when set to TRUE. 
#' @return A new list with word level information for each word.
#' 
#' @examples
#' twoSent_document = list()
#' twoSent_document$image = twoSent
#' twoSent_document$thin = thinImage(twoSent_document$image)
#' twoSent_processList = processHandwriting(twoSent_document$thin, dim(twoSent_document$image))
#' 
#' dims = dim(twoSent_document$image)
#' words = create_words(twoSent_processList) 
#' words_after_processing = process_words(words, dim(twoSent_document$image), TRUE)
#' 
#' @export
process_words = function(words, dims, triangulate = FALSE){
  #Will do different things depending on what is passed in, pass in TRUE to start the triangulation process
  if(triangulate){ 
    colorpoints_df <- lapply(words, find_colorpoints, dims=dims);
    #triangulate(colorpoints_df, dims)
  }
  
  #add word information to the word list that was passed in
  for(i in 1:length(words)){
    words[[i]] <- append(words[[i]], list(colorpoints_df = colorpoints_df[[i]]))
  }
  
  message("   ... words processed")
  
  return(words);
}

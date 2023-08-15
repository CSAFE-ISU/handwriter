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
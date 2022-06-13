#' make_single_word
#' 
#' @export
make_single_word = function(letterList){
  
  for(i in 1:length(letterList)){
    letterList[[i]]$characterFeatures$wordIndex = 1
  }
  
  return(letterList)
}


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

#
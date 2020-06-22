#' create_words
#'
#' creates word objects based on splits found in processHandwriting
#' @param processList Output from processHandwriting - contains all glyph information
#' @return list of word objects
#' @export
create_words = function(processList){
  words = list()
  letterList = processList$letterList
  
  allNodes = c()
  wordPath = c()
  terminalNodes = c()
  connectingNodes = c()
  
  wordIndex = 1
  for(i in letterList){
    
    if(wordIndex != i$characterFeatures$wordIndex){
      wordsInfo <- list(wordIndex = wordIndex, wordPath = wordPath, allNodes = allNodes, terminalNodes = terminalNodes, connectingNodes = connectingNodes)
      words[[wordIndex]] <- wordsInfo
      
      wordIndex = wordIndex + 1
      allNodes = c()
      wordPath = c()
      terminalNodes = c()
      connectingNodes = c()
    }
    
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
  
  #Just naming them here
  for (i in 1:length(words)){
    name = paste("wordIndex:",i,sep=" ")
    nameList <- append(nameList, name)
  }
  names(words) <- nameList
  
  return(words)
}

#
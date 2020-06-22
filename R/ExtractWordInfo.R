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
  wordIndex = 1
  for(i in letterList){
    
    if(wordIndex != i$characterFeatures$wordIndex){
      wordsInfo <- list(wordIndex = wordIndex, allNodes = allNodes, wordPath = wordPath)
      words[[wordIndex]] <- wordsInfo
      
      wordIndex = wordIndex + 1
      allNodes = c()
      wordPath = c()
    }
    
    if(wordIndex == i$characterFeatures$wordIndex){
      allNodes <- append(allNodes, i$nodes)
      wordPath <- append(wordPath, i$path)
    }
  }
  
  return(words)
}
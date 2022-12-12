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

#' find_colorpoints
#'
#' Finds and assigns points for Kneser Triangulation
#' 
#' @param words List of words and some glyph level information
#' @param dims The dimensions of the image (important for r/c features)
#' @return A new list with word level information for each word.
find_colorpoints = function(words, dims){
  
  #Add connecting PINK nodes
  colordf <- data.frame(words$connectingNodes, i_to_rc(words$connectingNodes, dims), rep(3, length(words$connectingNodes)), rep("pink", length(words$connectingNodes)))
  colnames(colordf) <- c("point", "row", "col", "colorIndex", "color")
  
  #Add terminal YELLOW nodes
  yellow_point <- data.frame(words$terminalNodes, i_to_rc(words$terminalNodes, dims), rep(6, length(words$terminalNodes)), rep("yellow", length(words$terminalNodes)))
  colnames(yellow_point) <- c("point", "row", "col", "colorIndex", "color")
  colordf <- rbind(colordf, (yellow_point))
  
  #Add Starting BLUE node
  if (length(words$terminalNodes) != 0){
    sorted <- sort(words$terminalNodes)
    beginning_node = sorted[[1]]
    blue_point <- data.frame(beginning_node, i_to_rc(beginning_node, dims), 1, "blue")
    colnames(blue_point) <- c("point", "row", "col", "colorIndex", "color")
    colordf <- rbind(colordf, (blue_point))

    #Add Ending ORANGE node
    ending_node = sorted[[length(sorted)]]
    orange_point <- data.frame(ending_node, i_to_rc(ending_node, dims), 2, "orange")
    colnames(orange_point) <- c("point", "row", "col", "colorIndex", "color")
    colordf <- rbind(colordf, orange_point)
  }

  #Add Node at highest GREEN point in total path
  rcs = i_to_rci(words$wordPath, dims)
  highest_point_index = rcs[[which.min(rcs[ ,1]), 3]]
  green_point <- data.frame(highest_point_index, i_to_rc(highest_point_index, dims), 4, "green")
  colnames(green_point) <- c("point", "row", "col", "colorIndex", "color")
  colordf <- rbind(colordf, green_point)


  #Similarly, add node at lowest PURPLE point in total path
  lowest_point_index = rcs[[which.max(rcs[ ,1]), 3]]
  purple_point <- data.frame(lowest_point_index, i_to_rc(lowest_point_index, dims), 5, "purple")
  colnames(purple_point) <- c("point", "row", "col", "colorIndex", "color")
  colordf <- rbind(colordf, purple_point)
       
  # #Add the YELLOW points for dots on i or j
  # #JAMES NOTES - For some reason these dont appear when plotting word normally - more exploration needed
  # has_i_or_j = FALSE
  # 
  # if(has_i_or_j){
  #   yellowrow = mean(colordf[,2])
  #   yellowcol = mean(colordf[,3])
  #   yellowindex = rc_to_i(yellowrow-15, yellowcol, dims)
  #   yellow_point <- data.frame(yellowindex, i_to_rc(yellowindex, dims), 6, "yellow")
  #   colnames(yellow_point) <- c("point", "row", "col", "colorIndex", "color")
  #   colordf <- rbind(colordf, yellow_point)
  # }
  
  return(colordf);
}

# triangulate = function(colorpoints_df, dims){
#   for(i in 1:length(colorpoints_df)){
#     word = colorpoints_df[[i]]
#     bluepoint = word[word$color=="blue"]
#   }
    
# }
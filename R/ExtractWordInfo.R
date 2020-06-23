#' process_words
#'
#' gets information on a word level
#' @param words List of words and some glyph level information
#' @param dims The dimensions of the image (important for r/c features)
#' @return A new list with word level information for each word.
#' @export
process_words = function(words, dims, triangulate = FALSE){
  #Will do different things depending on what is passed in, pass in TRUE to start the triangulation process
  if(triangulate){ 
    colorpoints_df <- lapply(words, find_colorpoints, dims=dims);
    #tri = triangulation(colorpoints_df, dims)
  }
  
  #add word information to the word list that was passed in
  for(i in 1:length(words)){
    words[[i]] <- append(words[[i]], list(colorpoints_df = colorpoints_df[[i]]))
  }
  return(words);
}

#' find_colorpoints
#'
#' finds and assigns points for Kneser Triangulation
#' @param words List of words and some glyph level information
#' @param dims The dimensions of the image (important for r/c features)
#' @return A new list with word level information for each word.
#' @export
find_colorpoints = function(words, dims){
  
  len = length(words$connectingNodes)
  
  #Add connecting PINK nodes
  colordf <- data.frame(words$connectingNodes, i_to_rc(words$connectingNodes, dims), rep(3, len), rep("pink", len))
  colnames(colordf) <- c("point", "row", "col", "colorIndex", "color")
  
  
  #Add Starting BLUE node
  beginning_node = words$terminalNodes[[1]]
  blue_point <- data.frame(beginning_node, i_to_rc(beginning_node, dims), 1, "blue")
  colnames(blue_point) <- c("point", "row", "col", "colorIndex", "color")
  colordf <- rbind(colordf, (blue_point))


  #Add Ending ORANGE node
  ending_node = words$terminalNodes[[length(words$terminalNodes)]]
  orange_point <- data.frame(ending_node, i_to_rc(ending_node, dims), 2, "orange")
  colnames(orange_point) <- c("point", "row", "col", "colorIndex", "color")
  colordf <- rbind(colordf, orange_point)


  #Add Node at highest GREEN Point in total path
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
       
  #Compute and add the dots for i or j
  #JAMES NOTES - For some reason these dont appear when plotting word normally - more exploration needed
  #Not in plot letter either (which I didnt write)
  #So, here is a fake one for now - currenty says there is a . in half of the words
  has_i_or_j = FALSE
  flip <- sample(1:2, 1)
  if(flip == 1) has_i_or_j = TRUE
  
  if(has_i_or_j){
    yellowrow = mean(colordf[,2])
    yellowcol = mean(colordf[,3])
    yellowindex = rc_to_i(yellowrow, yellowcol-10, dims)
    yellow_point <- data.frame(yellowindex, i_to_rc(yellowindex, dims), 6, "yellow")
    colnames(yellow_point) <- c("point", "row", "col", "colorIndex", "color")
    colordf <- rbind(colordf, yellow_point)
  }

  return(colordf);
}


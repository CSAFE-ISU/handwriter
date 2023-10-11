#' extract_character_features
#'
#' Primary driver of feature extraction. Parses all characters from a processed image.
#' 
#' @param img The thinned image bitmap
#' @param character_lists Output from processHandwriting$letterLists
#' @param dims Dimensions of binary image
#' @return nested lists associating features to respective characters.
#' 
extract_character_features = function(img, character_lists,dims){
  
  character_features = list()
  
  for(i in 1:length(character_lists)){
    cur_features = char_to_feature(character_lists[[i]],dims, i)
    character_features = append(character_features,list(cur_features))
  }
 
  character_features = add_updown_neighboring_char_dist(character_features, character_lists, img, dims)
  character_features = add_line_info(character_features,dims)
  character_features = nov_neighboring_char_dist(character_features)
  character_features = add_covariance_matrix(character_lists, character_features, dims)
  
  return(character_features)
}

#' char_to_feature
#'
#' Secondary driver of feature extraction
#' Extracts features from a single character
#' 
#' @param character character to extract information from
#' @param dims Dimensions of binary image
#' @param uniqueid Unique numerical reference to character
#' @return List containing features of character
#' 
char_to_feature = function(character, dims, uniqueid){
  aspect_info = get_aspect_info(character$path,dims)
  centroid_info = get_centroid_info(character$path,dims)
  features = c(aspect_info,centroid_info)
  
  #persistent index for sorting/rearranging the features list
  features$uniqueid = uniqueid
  
  return(features)
}


#' plotNodesLine
#'
#' Internal function for drawing a line from two given nodes.
#'  
#' @param doc A document processed with [handwriter::processHandwriter()]
#' @param nodeSize size of node; default set to 3
#' @param nodeColor color of node; default set to red
#' @return a line in between the two nodes
plotNodesLine = function(doc, nodeSize = 3, nodeColor = "red")
{
  X <- Y <- NULL
  p = plotImageThinned(doc)
  pointSet = data.frame(X = ((doc$process$nodes - 1) %/% dim(doc$image)[1]) + 1, Y = dim(doc$image)[1] - ((doc$process$nodes - 1) %% dim(doc$image)[1]))
  sx = pointSet[[1]][[1]]
  sy = pointSet[[2]][[1]]
  ex = pointSet[[1]][[2]]
  ey = pointSet[[2]][[2]]
  p = p + geom_point(data = pointSet, aes(X, Y), size = nodeSize, shape = I(16), color = I(nodeColor), alpha = I(.4)) + geom_segment(x = sx, y = sy, xend = ex, yend = ey)
  
  return(p)
}

plotNodesLine1 = function(doc, nodeSize = 3, nodeColor = "red")
{
  X <- Y <- NULL
  p = plotImageThinned(doc)
  pointSet = data.frame(X = ((doc$process$nodes - 1) %/% dim(doc$image)[1]) + 1, Y = dim(doc$image)[1] - ((doc$process$nodes - 1) %% dim(doc$image)[1]))
  sx = pointSet[[1]][[1]]
  sy = pointSet[[2]][[1]]
  ex = pointSet[[1]][[2]]
  ey = pointSet[[2]][[2]]
  p = p + geom_point(data = pointSet, aes(X, Y), size = nodeSize, shape = I(16), color = I(nodeColor), alpha = I(.4)) + geom_curve(x = sx, y = sy, xend = ex, yend = ey, curvature = 0, angle = 180)
  return(p)
}

#' get_aspect_info
#'
#' Extracts aspect ratio & supporting information from a character
#' Relevant Features:
#' Aspect Ratio: Row (Height) over (Column Width) 
#' Height, Width (Each measure of pixels)
#' The rest are supporting features that are minor independently.
#'  
#' @param character character to extract information from
#' @param dims Dimensions of binary image
#' @return List containing aspect_ratio, 
#' 
get_aspect_info = function(character, dims)
{
  rowcol = i_to_rci(character,dims)
  rows_y = rowcol[,'y'] 
  cols_x = rowcol[,'x']
  row_dist = max(rows_y) - min(rows_y) + 1 #vertical distance
  col_dist = max(cols_x) - min(cols_x) + 1 #horizontal distance
  aspect_info = list(aspect_ratio = row_dist/col_dist,height = row_dist, width = col_dist,topmost_row = min(rows_y),bottom_row = max(rows_y),leftmost_col=min(cols_x),rightmost_col=max(cols_x))
  return(aspect_info)
}

#' get_centroid_info
#'
#' Extracts centroid & supporting information from a character
#' Relevant Features:
#' Centroid Index: R Index representation of centroid location
#' Centroid x,y: X,Y representations of the centroid, see ?i_to_rci 
#' Centroid Horiz Location: How far along horizontally (Represented as a number between 0 and 1) the centroid is in its respective character.
#' Centroid Vertical Location: How far along vertically (Represented as a number between 0 and 1) the centroid is in its respective character.
#' Slope: 'Letter Lean', slope found between the centroids of each disjoint half in a single character.
#' The letter is split in half, each halve's centroid is calculated independently, the slope is taken between the two. 
#' Box Density: (Dimensions of box around letter width height) / (how much of the document it covers) //Might be a more document as opposed to letter based feature
#' Pixel Density: Ratio of black to white pixels found in box drawn around the letter.
#' 
#' @param character character to extract information from
#' @param dims Dimensions of binary image
#' @return List containing centroid, pixel density,letter 'lean', and all supporting information
#' 
get_centroid_info = function(character, dims)
{
  rowcol = i_to_rci(character,dims)
  rows_y = rowcol[,'y'] 
  cols_x = rowcol[,'x']
  centroid_row = mean(rows_y)
  centroid_col = mean(cols_x)
  row_dist = max(rows_y) - min(rows_y) + 1 #vertical distance
  col_dist = max(cols_x) - min(cols_x) + 1 #horizontal distance
  centroid_index = rc_to_i(centroid_row,centroid_col,dims)
  
  #relative density: draw a box around the letter, ratio of black to white pixels in the box
  r_density = length(character)/(row_dist*col_dist)
  #box density: dimensions of box around letter / how much of the document it covers
  box_density = (row_dist*col_dist) / (dims[1]*dims[2])

  centroid_horiz_location = (centroid_col-min(cols_x) + 1) / col_dist
  centroid_vert_location = (centroid_row-min(rows_y) + 1) / row_dist
  #used for getting skew, assuming centroid is more middle than the median col_x
  #probably can be removed, I just want nic to be able to plot them to determine if its an appropriate 'split' in the letter
  lHalf = list(rows_y = rows_y[which(cols_x<centroid_col)],cols_x = cols_x[which(cols_x<centroid_col)])
  rHalf = list(rows_y = rows_y[which(cols_x>centroid_col)],cols_x = cols_x[which(cols_x>centroid_col)])
  lHalfCentroidrc = list(y=mean(lHalf$rows_y),x=mean(lHalf$cols_x))
  rHalfCentroidrc = list(y=mean(rHalf$rows_y),x=mean(rHalf$cols_x))
  lHalfCentroidi = rc_to_i(mean(lHalf$rows_y),mean(lHalf$cols_x),dims)
  rHalfCentroidi = rc_to_i(mean(rHalf$rows_y),mean(rHalf$cols_x),dims)
  #indices of each half
  lHi = rc_to_i(lHalf$rows_y,lHalf$cols_x,dims)
  rHi = rc_to_i(rHalf$rows_y,rHalf$cols_x,dims)
  #finding slope, in case of long letters like e in csafe maybe account length?
  #errrrr does the y need a +1
  slope = ((dims[1] - rHalfCentroidrc$y)-(dims[1] - lHalfCentroidrc$y))/(rHalfCentroidrc$x-lHalfCentroidrc$x+1)
  if(length(lHalf[[1]]) == 0 & length(rHalf[[1]]) == 0)
  {
    slope = 0
  }
  lHalfCentroid = rc_to_i(mean(lHalf$rows_y),mean(lHalf$cols_x),dims)
  centroid_info = list(centroid_index = centroid_index, centroid_y = centroid_row, centroid_x = centroid_col, centroid_horiz_location = centroid_horiz_location,centroid_vert_location = centroid_vert_location,lHalf = lHi,rHalf=rHi,disjoint_centroids = list(left = lHalfCentroidi,right = rHalfCentroidi),slope = slope, pixel_density = r_density,box_density = box_density)
  return(centroid_info)
}

#' add_covariance_matrix
#'
#' @param character_lists Output from processHandwriting$letterLists
#' @param character_features Nested lists associating features to respective characters.
#' @param dims Dimensions of binary image
#' @return nested lists associating features to respective characters.
#' 
add_covariance_matrix = function(character_lists, character_features, dims){
  for(i in 1:length(character_lists)){
    matrix = i_to_rc(character_lists[[i]]$path, dims)
    x = matrix[,2]
    y = matrix[,1]
    y = dims[1] - y #FLIPS Y VALUE SO IT REPS A REAL COORD PLANE
    variance_of_x = stats::var(x)
    variance_of_y = stats::var(y)
    covariance_of_xy = stats::cov(x,y)
    
    #Add Covariance to the character features
    character_features[[i]]$xvar = variance_of_x
    character_features[[i]]$yvar = variance_of_y
    character_features[[i]]$covar = covariance_of_xy
  }

  return(character_features)
}

#' add_line_info
#'
#' Associates characters to their respective line numbers
#' Needs improvement if runtime becomes a problem
#' 
#' @param character_features All extracted features 
#' @param dims Dimensions of binary image
#' @return Appends line information to character features
#' 
add_line_info = function(character_features,dims){
  line_info = line_number_extract(all_down_dists(character_features), all_centroids(character_features), dims)
  line_order = lapply(line_info, sort)
  for(i in 1:length(character_features)){
    cur_letter_index = character_features[[i]]$centroid_index
    for(j in 1:length(line_info)){
      if(cur_letter_index %in% line_info[[j]]){
        character_features[[i]] = c(character_features[[i]],list(line_number = j, order_within_line = which(line_order[[j]] == cur_letter_index)))
      }
    }
  }
  return(character_features)
}


#Return a list of the distances from the top of a character to the first thing above it
add_updown_neighboring_char_dist = function(character_features, character_lists, img, dims){

  #For each character
  for(i in 1:length(character_lists)){
    down_distance = Inf
    #Get the lowest point as an index point
    lowest_point = character_lists[[i]]$path[[1]]
    rci = i_to_rci(character_lists[[i]]$path, dims)
    min_y_sorted = rci[order(rci[,1],decreasing=TRUE),]
    lowest_index = min_y_sorted[[1,3]]
    row = min_y_sorted[[1,1]]
    col = min_y_sorted[[1,2]]
    
    #go down until hit another index (don't go past the bottom)
    cur_row = row + 1
    while(cur_row <= dims[1]){
      index_to_check = rc_to_i(cur_row, col, dims)
      
      if(img[[cur_row, col]] == 0){
        down_distance = cur_row-row
        break;
      }
      
      cur_row = cur_row + 1
    }
    #Do the math on the difference
    character_features[[i]]$down_dist = down_distance

  }
  
  return(character_features)
}


#' get_loop_info
#'
#' Associator of loop to character association
#' Relevant Features:
#' Loop Count, how many loops are found in the letter
#' Loop Major, length of farthest line that can be drawn inside of a loop
#' Loop Minor, length of the perpendicular bisector of the loop major.
#' 
#' @param character Target for loop association
#' @param dims Dimensions of binary image
#' @return Loop information to respective character
#' 
get_loop_info = function(character,dims){
  
  #loops = loop_extract(character$allPaths)
  #loop_info = list(loop_count = length(loops),loops = loops)
  loop_info = list(loop_count = length(character$loops), loops = character$loops)
  return(loop_info)
}


# Principle: Appending inside of a nested loop
# NOTE: Uses distances between centroids, NOT right edge to left edge
nov_neighboring_char_dist = function(character_features){
  by_line = character_features_by_line(character_features)
  for(line in 1:length(by_line)){
    for(i in 1:length(by_line[[line]])){
      cur_char = by_line[[line]][[i]]
      l_neighbor_centroid_dist = NULL
      r_neighbor_centroid_dist = NULL
      
      if(i != 1){
        prev_char = by_line[[line]][[i-1]]
        l_neighbor_centroid_dist = cur_char$centroid_x - prev_char$centroid_x
      }
      if(i != length(by_line[[line]])){
        next_char = by_line[[line]][[i+1]]
        r_neighbor_centroid_dist = next_char$centroid_x - cur_char$centroid_x
      }
      character_features[[cur_char$uniqueid]] = c(character_features[[cur_char$uniqueid]],list(l_neighbor_dist = l_neighbor_centroid_dist, r_neighbor_dist = r_neighbor_centroid_dist))
    }
  }
  
  return(character_features)
}

#sort character features indexed by their respective line
character_features_by_line = function(character_features){
  max_line = -Inf
  for(i in 1:length(character_features)){
    max_line = max(max_line, character_features[[i]]$line_number)
  }
  
  characters_by_line = rep(list(list()),max_line)
  
  for(j in 1:length(character_features)){
    
    characters_by_line[[character_features[[j]]$line_number]] = append(characters_by_line[[character_features[[j]]$line_number]],list(character_features[[j]]))
  }

  return(characters_by_line)
}

#' loop_extract
#'
#' Iterates through all available paths from processHandwriting()
#' Picks out loops for later character association.
#' 
#' @param allPaths All character (formerly letter) paths from processHandwriting()
#' 
#' @return List of all loops 
loop_extract = function(allPaths){
  loops = list()
  for(i in 1:length(allPaths)){
    if(length(allPaths)<1){
      next
    }
    if(allPaths[[i]][[1]]==allPaths[[i]][[length(allPaths[[i]])]]){
      loops = c(loops,list(allPaths[[i]]))
    }
  }
  return(loops)
}

#' all_centroids
#'
#' Iterates through extracted character features, extracting
#' all centroids found for later use in line numbering.
#' 
#' @param character_features Features extracted from any given document
#' @return All centroids concatenated with one another (unlisted)
#' 
all_centroids = function(character_features){
  centroids = list()
  for(i in 1:length(character_features)){
    centroids = c(centroids,character_features[[i]]$centroid_index)
  }
  return(unlist(centroids))
}

#' all_down_dists
#'
#' Iterates through extracted character features, extracting
#' all downward distances found for later use in line separating.
#' 
#' @param character_features Features extracted from any given document
#' @return All downdistance concatenated with one another (unlisted)
#' 
all_down_dists = function(character_features){
  down_dists = list()
  for(i in 1:length(character_features)){
    down_dists = c(down_dists,character_features[[i]]$down_dist)
  }
  return(unlist(down_dists))
}

#' line_number_extract
#'
#' Primary logic unit for line number to character association.
#' 
#' @param down_dists how far down to the next character from each character
#' @param all_centroids List of centroids extracted from cumulative character_features
#' @param dims Dimensions of binary image
#' @return List associating line numbers to characters
#' 
#' @importFrom stats median
#' @importFrom utils head
line_number_extract = function(down_dists, all_centroids, dims){
  centroid_rci = matrix(i_to_rci(all_centroids,dims), ncol = 3)
  #sorting list based on y
  centroid_rci = matrix(centroid_rci[order(centroid_rci[,1]),], ncol = 3)

  #Do some down_distance math
  sorted_down_dists = sort(down_dists)
  
  inf_removed = sorted_down_dists[!is.na(sorted_down_dists) & !is.infinite(sorted_down_dists)]
  
  length_of_vector = length(inf_removed)
  items_to_remove = length_of_vector/5 #Removing top 20% right now
  trimmed = head(inf_removed, -items_to_remove)
  
  threshold_num = as.numeric(median(trimmed)/2)

  
  lines = list()
  cur_line = vector(mode="double", length=0)
  threshold = vector(mode="double", length=0)
  i = 1
  while(i <= max(dim(centroid_rci)[1], 1)){
    tm = mean(threshold)
    cur_index = centroid_rci[i,3][[1]]
    cur_y = centroid_rci[i,1][[1]] #centroid of current
    if(length(threshold)==0){
      cur_line = c(cur_line,cur_index)
    }
    else if (is.na(threshold_num)){
      cur_line = c(cur_line,cur_index)
    }
    else if(abs(tm-cur_y) < threshold_num){
      cur_line = c(cur_line,cur_index)
    }
    else{
      lines = c(lines,list(cur_line))
      cur_line = vector(mode="double", length=0)
      i = i-1
      threshold = vector(mode="double", length=0)
    }
    if(i==dim(centroid_rci)[1]){
      lines = c(lines,list(cur_line))
    }
    threshold = c(threshold,cur_y)
    i = i + 1
  }
  return(lines)
}

#####################################################################
###---### CURRENTLY UNUSED FUNCTIONS -- FUTURE USE UNKNOWN ####---###
#####################################################################
# 
# loop_info = function(loop_list, dims){
#   major = loop_major(loop_list, dims)
#   slope = find_i_slope(major$major_p1,major$major_p2,dims)
#   print(slope)
#   minor = loop_minor(loop_list,slope,dims)
#   return(list(major = major,minor = minor))
# }
# loop_major = function(loop_list,dims){
#   rowcol = i_to_rci(loop_list,dims)
#   rows_y = rowcol[,'y'] 
#   cols_x = rowcol[,'x']
#   major_dist = -Inf
#   y1 = rowcol[[1]]
#   x1 = rowcol[[1]]
#   furthest_index = NULL
#   for(i in 1:length(loop_list)){
#     cur_dist = sqrt((cols_x[[i]]-x1)^2+(rows_y[[i]]-y1)^2)
#     if(cur_dist > major_dist ){
#       major_dist = cur_dist
#       furthest_index = loop_list[[i]]
#     }
#   }
#   return(list(major_p1 = loop_list[[1]], major_p2 = furthest_index, major_dist = major_dist))
# }
# vector_to_mid = function(targ)
# 
# loop_minor = function(loop_list, slope, dims){
#     i1 = NULL
#     i2 = NULL
#     neg_recip = -1/(slope)
#     cat("neg recip: ",neg_recip,"\n")
#     min_dif = Inf
#     for(i in 1:length(loop_list)/2){
#       for(j in length(loop_list)/2:1){
#         if(i == j) next 
#         else {
#           new_slope = find_i_slope(loop_list[[i]],loop_list[[j]],dims)
#           slope_dif = abs(new_slope-neg_recip)
#           if(!is.nan(new_slope) & new_slope< -.8 & new_slope > -1){
#           }
#         }
#         if(!is.nan(slope_dif) & slope_dif < min_dif){
#           cat(loop_list[[i]],loop_list[[j]],"new slope: ",new_slope, "\n")
#           i1 = loop_list[[i]]
#           i2 = loop_list[[j]]
#           min_dif = slope_dif
#         }
#       }
#     }
#     return(list(minor_p1 = i1, minor_p2 = i2))
#   }
# 
# find_i_slope = function(starti, endi, dims, dbug = FALSE)
# {
#   rci = i_to_rci(c(starti,endi),dims)
#   #print(rci)
#   #standard for actual coordinate eq's
#   rows_y = dims[[1]] - rci[,'y'] + 1
#   cols_x = rci[,'x']
#   x1 = cols_x[[1]]
#   y1 = rows_y[[1]]
#   x2 = cols_x[[2]]
#   y2 = rows_y[[2]]
#   if(dbug){
#     cat("x1: ",x1[[1]]," y1: ", y1[[1]] ,"\n")
#     cat("x2: ",x2[[1]]," y2: ", y2[[1]], "\n")
#   }
#   slope = (y2-y1)/(x2-x1)
#   return(slope)
# }
# 
# #driver for minor axis, rq'd feature by amy
# perp_bisector = function(x1,x2,y1,y2,slope,dims,dbug = FALSE){
#   midx = (x1+x2)/2
#   midy = (y1+y2)/2
#   neg_recip = -1/(slope)
# }
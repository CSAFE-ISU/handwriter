#!!!! <- this means nick please look at this


#eh not very useful doesn't work as well
loopMeasure = function(loopList, dims){
  #y coord
  #row modulo add 1
  #subtract 1 from the index, perform operation, add 1
  #col div add 1
  rowList = loopList %/% dims[1]
  #x coord
  colList = loopList %% dims[2]
  #whats the regular way to do this
  longestPath = -9999999
  targ_x1 = NULL
  targ_y1 = NULL
  targ_x2 = NULL
  targ_y2 = NULL
  i_index = NULL
  j_index = NULL
  for(i in 1:length(rowList)){
    y1 = dims[[1]] - rowList[i]
    x1 = colList[i]
    i_index = loopList[i]
    for(j in 1:length(rowList)){
      y2 = dims[[1]] - rowList[j]
      x2 = colList[j]
      euDist = sqrt((x2-x1)^2+(y2-y1)^2)
      if(euDist > longestPath){
        longestPath = euDist
        targ_x1 = x1
        targ_y1 = y1
        targ_x2 = x2
        targ_y2 = y2
        j_index = loopList[j]
      }
    }
  }
  #print indexes of longest point for testing purposes
  #heres an idea of returning stuff
  #return(data.frame(valNames = c("pathLen","x1","x2","y1","y2"),index = c("hmm",i_index,j_index,i_index,j_index) ,vals=c(longestPath,targ_x1,targ_x2,targ_y1,targ_y2)))
  #heres another one that seems more useful for returning usable elements
  return(list(list(i_index,j_index),longestPath))
}

loopMeasures = function(loopListAll, dims){
  loopMeasure_results = list(1:length(loopListAll))
  loopMeasure_points = list(1:length(loopListAll))
  for(i in 1:length(loopListAll)){
    result = loopMeasure(loopListAll[[i]],dims)
    loopMeasure_results[[i]] = result
    loopMeasure_points[[i]] = result[[1]]
  }
  return(list(loopMeasure_points,loopMeasure_results))
}
#trash above h4h4h4
loop_info = function(loop_list, img_dim){
  major = loop_major(loop_list, img_dim)
  slope = find_i_slope(major$major_p1,major$major_p2,img_dim)
  print(slope)
  minor = loop_minor(loop_list,slope,img_dim)
  return(list(major = major,minor = minor))
}
loop_major = function(loop_list,img_dim){
  rowcol = i_to_rci(loop_list,img_dim)
  rows_y = rowcol[,'y'] 
  cols_x = rowcol[,'x']
  major_dist = -Inf
  y1 = rowcol[[1]]
  x1 = rowcol[[1]]
  furthest_index = NULL
  for(i in 1:length(loop_list)){
    cur_dist = sqrt((cols_x[[i]]-x1)^2+(rows_y[[i]]-y1)^2)
    if(cur_dist > major_dist ){
      major_dist = cur_dist
      furthest_index = loop_list[[i]]
    }
  }
  return(list(major_p1 = loop_list[[1]], major_p2 = furthest_index, major_dist = major_dist))
}
vector_to_mid = function(targ)
loop_minor = function(loop_list, slope, img_dim){
  #targ 29572, 30245
  #29243 29914 new slope:  -0.8333333 
  #30136 29353 new slope:  -0.8571429 
  i1 = NULL
  i2 = NULL
  neg_recip = -1/(slope)
  cat("neg recip: ",neg_recip,"\n")
  min_dif = Inf
  for(i in 1:length(loop_list)/2){
    for(j in length(loop_list)/2:1){
      if(i == j) next 
      else {
        new_slope = find_i_slope(loop_list[[i]],loop_list[[j]],img_dim)
        slope_dif = abs(new_slope-neg_recip)
        if(!is.nan(new_slope) & new_slope< -.8 & new_slope > -1){
          #cat(loop_list[[i]],loop_list[[j]],"new slope: ",new_slope, "\n")
          #cat("slope dif: ",slope_dif,"\n")
        }
      }
      if(!is.nan(slope_dif) & slope_dif < min_dif){
        cat(loop_list[[i]],loop_list[[j]],"new slope: ",new_slope, "\n")
        i1 = loop_list[[i]]
        i2 = loop_list[[j]]
        min_dif = slope_dif
        #cat(i1,i2," \n")
        #if(i1 == 29572 & i2 == 30245){
        #  print("bags")
        #}
      }
    }
  }
  return(list(minor_p1 = i1, minor_p2 = i2))
}
#given two indices drawing a line, find slope
# !!!!!!!! errrrrrrrrrrr does a + 1 need to be added in my letter lean ask nic
#  slope = ((img_dim[1] - rHalfCentroidrc$y)-(img_dim[1] - lHalfCentroidrc$y))/(rHalfCentroidrc$x-lHalfCentroidrc$x)
#1 261 53
#2 280 73
find_i_slope = function(starti, endi, img_dim, dbug = FALSE)
{
  rci = i_to_rci(c(starti,endi),img_dim)
  #print(rci)
  #standard for actual coordinate eq's
  rows_y = img_dim[[1]] - rci[,'y'] + 1
  cols_x = rci[,'x']
  x1 = cols_x[[1]]
  y1 = rows_y[[1]]
  x2 = cols_x[[2]]
  y2 = rows_y[[2]]
  if(dbug){
    cat("x1: ",x1[[1]]," y1: ", y1[[1]] ,"\n")
    cat("x2: ",x2[[1]]," y2: ", y2[[1]], "\n")
  }
  slope = (y2-y1)/(x2-x1)
  return(slope)
}

#driver for minor axis, rq'd feature by amy
perp_bisector = function(x1,x2,y1,y2,slope,img_dim,dbug = FALSE){
  midx = (x1+x2)/2
  midy = (y1+y2)/2
  neg_recip = -1/(slope)
}

#give two nodes, draw one line
plotNodesLine = function(img, thinned, nodeList, nodeSize = 3, nodeColor = "red")
{
  p = plotImageThinned(img, thinned)
  pointSet = data.frame(X = ((nodeList - 1) %/% dim(img)[1]) + 1, Y = dim(img)[1] - ((nodeList - 1) %% dim(img)[1]))
  sx = pointSet[[1]][[1]]
  sy = pointSet[[2]][[1]]
  ex = pointSet[[1]][[2]]
  ey = pointSet[[2]][[2]]
  p = p + geom_point(data = pointSet, aes(X, Y), size = nodeSize, shape = I(16), color = I(nodeColor), alpha = I(.4)) + geom_segment(x = sx, y = sy, xend = ex, yend = ey)
  return(p)
  #l.m = melt(img)
  #l.m$value[thinned] = 2
  #l.m$value[nodeList] = 3
  #n.m2 = n.m[nodeList,]
  #p = ggplot(l.m, aes(Var2, rev(Var1))) + geom_raster(aes(fill = as.factor(value != 1), alpha = ifelse(value==0,.3,1))) + scale_alpha_continuous(guide = FALSE) + scale_fill_manual(values = c("white", "black"), guide = FALSE) + theme_void() + geom_point(data= n.m2, aes(x = Var2, y = dim(img)[1] - Var1 + 1), shape = I(17), size = I(nodeSize), color = I("red"))
}

plotNodesLine1 = function(img, thinned, nodeList, nodeSize = 3, nodeColor = "red")
{
  p = plotImageThinned(img, thinned)
  pointSet = data.frame(X = ((nodeList - 1) %/% dim(img)[1]) + 1, Y = dim(img)[1] - ((nodeList - 1) %% dim(img)[1]))
  sx = pointSet[[1]][[1]]
  sy = pointSet[[2]][[1]]
  ex = pointSet[[1]][[2]]
  ey = pointSet[[2]][[2]]
  p = p + geom_point(data = pointSet, aes(X, Y), size = nodeSize, shape = I(16), color = I(nodeColor), alpha = I(.4)) + geom_curve(x = sx, y = sy, xend = ex, yend = ey, curvature = 0, angle = 180)
  return(p)
  #l.m = melt(img)
  #l.m$value[thinned] = 2
  #l.m$value[nodeList] = 3
  #n.m2 = n.m[nodeList,]
  #p = ggplot(l.m, aes(Var2, rev(Var1))) + geom_raster(aes(fill = as.factor(value != 1), alpha = ifelse(value==0,.3,1))) + scale_alpha_continuous(guide = FALSE) + scale_fill_manual(values = c("white", "black"), guide = FALSE) + theme_void() + geom_point(data= n.m2, aes(x = Var2, y = dim(img)[1] - Var1 + 1), shape = I(17), size = I(nodeSize), color = I("red"))
}
#' i_to_rc
#'
#' Convert indicies to respective row, col.
#' @param nodes Nodes to be converted.
#' @param img_dim Dimensions of binary image
#' @keywords row, column, binary, image
#' @return Returns matrix mapping nodes to respective row, col. 
#' @export
 
i_to_rc = function(nodes, dims)
{
  cs = (nodes-1)%/%dims[1] + 1
  rs = (nodes-1)%%dims[1] + 1
  return(matrix(c(rs,cs), ncol = 2))
}

#' i_to_rci
#'
#' Convert indicies to respective row, col and associates the original index.
#' @param nodes Nodes to be converted.
#' @param img_dim Dimensions of binary image
#' @keywords row, column, binary, image, index
#' @return Returns matrix mapping nodes' indices to respective row, col. 
#' @export

i_to_rci = function(nodes, dims, fixed = FALSE)
{
  cs = (nodes-1)%/%dims[1] + 1
  rs = (nodes-1)%%dims[1] + 1
  if(fixed) rs = dims[[1]] - rs
  rowcolmatrix = matrix(c(rs,cs,nodes), ncol = 3)
  colnames(rowcolmatrix) = c('y','x','index')
  return(rowcolmatrix)
}


#' rc_to_i
#'
#' Convert rows and columns to their respective indices.
#' This is index sensitive, so row_y[[1]] should correspond to col_x[[1]]
#' @param row_y Row(s) to be converted to an index
#' @param col_x Columns(s) to be converted to an index
#' @param img_dim Dimensions of binary image
#' @keywords row, column, binary, image, index
#' @return Returns index(icies) of all row_y's and col_x's
#' @export

rc_to_i = function(row_y,col_x,img_dim, fixed = FALSE)
{
  row_y = as.integer(row_y)
  if(fixed) row_y = img_dim[[1]] - row_y
  col_x = as.integer(col_x)
  return((col_x-1)*img_dim[1]+row_y)
}

#' get_aspect_info
#'
#' Extracts aspect ratio & supporting information from a character
#' Relevant Features:
#' Aspect Ratio: Row (Height) over (Column Width) //Determined after a meeting, can easily be switched around)
#' Height, Width (Each measure of pixels)
#' The rest are supporting features that are minor independently. 
#' @param character character to extract information from
#' @param img_dim Dimensions of binary image
#' @keywords aspect, ratio, character, width, height
#' @return List containing aspect_ratio, 
#' height, width, highest, lowest, leftmost,
#' points of character. (row, col representation)
#' @export

get_aspect_info = function(character, img_dim)
{
  rowcol = i_to_rci(character,img_dim)
  rows_y = rowcol[,'y'] 
  cols_x = rowcol[,'x']
  row_dist = max(rows_y) - min(rows_y) + 1#vertical distance
  col_dist = max(cols_x) - min(cols_x) + 1 #horizontal distance
  aspect_info = list(aspect_ratio = row_dist/col_dist,height = row_dist, width = col_dist,topmost_row = min(rows_y),bottom_row = max(rows_y),leftmost_col=min(cols_x),rightmost_col=max(cols_x))
  return(aspect_info)
}

#' get_centroid_info
#'
#' Extracts centroid & supporting information from a character
#' Relevant Features:
#' Centroid Index: R Index representation of centroid location
#' Centroid x,y: X,Y representations of the centroid, see ?i_to_rci (Written by Nick)
#' Centroid Horiz Location: How far along horizontally (Represented as a number between 0 and 1) the centroid is in its respective character.
#' Centroid Vertical Location: How far along vertically (Represented as a number between 0 and 1) the centroid is in its respective character.
#' Slope: 'Letter Lean', slope found between the centroids of each disjoint half in a single character.
#' The letter is split in half, each halve's centroid is calculated independently, the slope is taken between the two. 
#' Box Density: (Dimensions of box around letter width height) / (how much of the document it covers) //Might be a more document as opposed to letter based feature
#' Pixel Density: Ratio of black to white pixels found in box drawn around the letter.
#' @param character character to extract information from
#' @param img_dim Dimensions of binary image
#' @keywords centroid, skew, slant, lean, character
#' @return List containing centroid, pixel density,
#' letter 'lean', and all supporting information
#' @export

get_centroid_info = function(character, img_dim)
{
  rowcol = i_to_rci(character,img_dim)
  rows_y = rowcol[,'y'] 
  cols_x = rowcol[,'x']
  centroid_row = mean(rows_y)
  centroid_col = mean(cols_x)
  row_dist = max(rows_y) - min(rows_y) + 1 #vertical distance
  col_dist = max(cols_x) - min(cols_x) + 1 #horizontal distance
  centroid_index = rc_to_i(centroid_row,centroid_col,img_dim)
  
  #relative density: draw a box around the letter, ratio of black to white pixels in the box
  r_density = length(character)/(row_dist*col_dist)
  #box density: dimensions of box around letter / how much of the document it covers
  box_density = (row_dist*col_dist) / (img_dim[1]*img_dim[2])
  #bad?
  #centroid_horiz_location = (min(cols_x)+(centroid_col-min(cols_x))) / col_dist
  #centroid_vert_location = (min(rows_y)+(centroid_row-min(rows_y))) / row_dist
  
  centroid_horiz_location = (centroid_col-min(cols_x)) / col_dist
  centroid_vert_location = (centroid_row-min(rows_y)) / row_dist
  #used for getting skew, assuming centroid is more middle than the median col_x
  #probably can be removed, I just want nic to be able to plot them to determine if its an appropriate 'split' in the letter
  lHalf = list(rows_y = rows_y[which(cols_x<centroid_col)],cols_x = cols_x[which(cols_x<centroid_col)])
  rHalf = list(rows_y = rows_y[which(cols_x>centroid_col)],cols_x = cols_x[which(cols_x>centroid_col)])
  lHalfCentroidrc = list(y=mean(lHalf$rows_y),x=mean(lHalf$cols_x))
  rHalfCentroidrc = list(y=mean(rHalf$rows_y),x=mean(rHalf$cols_x))
  lHalfCentroidi = rc_to_i(mean(lHalf$rows_y),mean(lHalf$cols_x),img_dim)
  rHalfCentroidi = rc_to_i(mean(rHalf$rows_y),mean(rHalf$cols_x),img_dim)
  #indices of each half
  lHi = rc_to_i(lHalf$rows_y,lHalf$cols_x,img_dim)
  rHi = rc_to_i(rHalf$rows_y,rHalf$cols_x,img_dim)
  #finding slope, in case of long letters like e in csafe maybe account length?
  #errrrr does the y need a +1
  slope = ((img_dim[1] - rHalfCentroidrc$y)-(img_dim[1] - lHalfCentroidrc$y))/(rHalfCentroidrc$x-lHalfCentroidrc$x)
  lHalfCentroid = rc_to_i(mean(lHalf$rows_y),mean(lHalf$cols_x),img_dim)
  centroid_info = list(centroid_index = centroid_index, centroid_y = centroid_row, centroid_x = centroid_col, centroid_horiz_location = centroid_horiz_location,centroid_vert_location = centroid_vert_location,lHalf = lHi,rHalf=rHi,disjoint_centroids = list(left = lHalfCentroidi,right = rHalfCentroidi),slope = slope, pixel_density = r_density,box_density = box_density )
  return(centroid_info)
}

#' extract_character_features
#'
#' Primary driver of feature extraction. 
#' Parses all characters from a processed image.
#' For information detailing each feature, please see
#' ?get_aspect_info
#' ?get_centroid_info
#' ?get_loop_info
#' @param character_lists Output from processHandwriting$letterLists
#' @param img_dim Dimensions of binary image
#' @keywords centroid, skew, slant, lean, character
#' @return nested lists associating features to respective characters.
#' @export

extract_character_features = function(character_lists,img_dim){
  #i strongly believe line based features are useful too
  #nic take a look at this:http://old.cescg.org/CESCG-2008/papers/BratislavaC-Bozekova-Miroslava.pdf 
  character_features = list()
  
  for(i in 1:length(character_lists)){
    cur_features = char_to_feature(character_lists[[i]],img_dim, i)
    character_features = append(character_features,list(cur_features))
  }
  
  character_features = add_line_info(character_features,img_dim)
  character_features = nov_neighboring_char_dist(character_features)
  return(character_features)
}

#' char_to_feature
#'
#' Secondary driver of feature extraction
#' Extracts features from a single character
#' @param character character to extract information from
#' @param img_dim Dimensions of binary image
#' @param uniqueid Unique numerical reference to character
#' @keywords character, features
#' @return List containing features of character
#' @export

char_to_feature = function(character, img_dim, uniqueid){
  aspect_info = get_aspect_info(character$path,img_dim)
  centroid_info = get_centroid_info(character$path,img_dim)
  loop_info = get_loop_info(character,img_dim)
  features = c(aspect_info,centroid_info,loop_info)
  #just a persistent index for when we sort / rearrange the features list
  features$uniqueid = uniqueid
  return(features)
}

#' add_line_info
#'
#' Associates characters to their respective line numbers
#' Needs improvement if runtime becomes a problem
#' @param character_features All extracted features 
#' @param img_dim Dimensions of binary image
#' @keywords character, features, line number
#' @return Appends line information to character features
#' @export

add_line_info = function(character_features,img_dim){
  line_info = line_number_extract(all_centroids(character_features),img_dim)
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


#' lm_rm_nodes
#'
#' Helper function made before line information was available
#' Likely obsolete 11 27 18
#' N/A

lm_rm_nodes = function(character){
  lm_rm_nodelist = list()
  for(i in 1:length(character)){
    cur = character[[i]]$nodesInGraph
    lm_rm_nodelist = append(lm_rm_nodelist,list(list(leftMost = cur[[1]],rightMost = cur[[length(cur)]])))
  }
  return(lm_rm_nodelist)
}

#' get_loop_info
#'
#' Associator of loop to character association
#' Relevant Features:
#' Loop Count, how many loops are found in the letter
#' Loop Major, length of farthest line that can be drawn inside of a loop
#' Loop Minor, length of the perpindcular bisector of the loop major.
#' ! I've removed loop minor / loop major features due to instability during testing.
#' The outliers were frequent enough to deem my implementation unreliable for modeling.
#' I need additional help in some of the syntactical challenges I've experienced with those features..
#' @param character Target for loop association
#' @param img_dim Dimensions of binary image
#' @keywords character, loop, associate
#' @return Loop information to respective character
#' @export

get_loop_info = function(character,img_dim){
  
  #loops = loop_extract(character$allPaths)
  #loop_info = list(loop_count = length(loops),loops = loops)
  loop_info = list(loop_count = length(character$loops), loops = character$loops)
  return(loop_info)
}

#probably garbage
neighboring_char_dist = function(character_features){
  letterDist = list()
  for(i in 1:length(character_features)){
    dist_left = NULL
    dist_right = NULL
    cur_lm = character_features[[i]]$leftmost_col
    cur_rm = character_features[[i]]$rightmost_col
    if(i == 1){
      dist_left = -1
    }
    else{
      prev_rm = character_features[[i]]$rightmost_col
    }
    if(i == length(character_features)){
      dist_right = -1
    }
    else{
      next_lm = character_features[[i+1]]$leftmost_col
    }
    if(is.null(dist_left)){
      dist_left = cur_lm-prev_rm
    }
    if(is.null(dist_right)){
      dist_right = next_lm - cur_rm
    }
    letterDist = c(letterDist,list(dist_left,dist_right))
  }
    return(letterDist)
}

# Principle: Appending inside of a nested loop
# character_features[[i]] = c(character_features[[i]],list(line_number = j))
nov_neighboring_char_dist = function(character_features){
  by_line = character_features_by_line(character_features)
  for(line in 1:length(by_line)){
    for(i in 1:length(by_line[[line]])){
      cur_char = by_line[[line]][[i]]
      l_neighbor_centroid_dist = NULL
      r_neighbor_centroid_dist = NULL
      #tempted to fancy x = if(condition) val else alt_val but want to understand this later
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
  #is this the only way to initialize a buffer of static size?
  characters_by_line = rep(list(list()),max_line)
  
  for(j in 1:length(character_features)){
    
    characters_by_line[[character_features[[j]]$line_number]] = append(characters_by_line[[character_features[[j]]$line_number]],list(character_features[[j]]))
  }
  #!!! I don't paticularly know why but
  #These already seem to be sorted by x centroid in increasing order
  return(characters_by_line)
}

#TODO:
#Finish sort_by_centroid_x(character_features_by_line) if
#character_features_by_line stops magically already doing it 
#magical stack overflow 
#https://stackoverflow.com/questions/24203361/r-sorting-list-by-values-in-nested-list
#data_list = data_list[order(sapply(data_list, `[[`, i=1))]
byline_x_print = function(characters_by_line){
  for(i in 1:length(characters_by_line)){
    for(j in 1:length(characters_by_line[[i]])){
      print(characters_by_line[[i]][[j]]$centroid_x)
    }
    print("chris")
  }
}

#' loop_extract
#'
#' Iterates through all avaiable paths from processHandwriting()
#' Picks out loops for later character association.
#' @param allPaths All character (formerly letter) paths from processHandwriting()
#' @keywords character, loops, line
#' @return List of all loops
#' @export

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
#' all centroids found for later use in line numbering
#' @param character_features Features extracted from any given document
#' @keywords character, loops, line
#' @return All centroids concatenated with one another (unlisted)
#' @export

all_centroids = function(character_features){
  centroids = list()
  for(i in 1:length(character_features)){
    centroids = c(centroids,character_features[[i]]$centroid_index)
  }
  return(unlist(centroids))
}

#' line_number_extract
#'
#' Primary logic unit for line number to character association
#' @param all_centroids List of centroids extracted from cumulative character_features
#' @param img_dim Dimensions of binary image
#' @keywords character, features, line, number
#' @return List associating line numbers to characters
#' @export

line_number_extract = function(all_centroids,img_dim){
  centroid_rci = i_to_rci(all_centroids,img_dim)
  #sorting list based on y
  centroid_rci = centroid_rci[order(centroid_rci[,'y']),]
  lines = list()
  cur_line = vector(mode="double", length=0)
  threshold = vector(mode="double", length=0)
  i = 1
  while(i <= dim(centroid_rci)[1]){
    tm = mean(threshold)
    cur_index = centroid_rci[i,'index'][[1]]
    cur_y = centroid_rci[i,'y'][[1]]
    if(length(threshold)==0){
      cur_line = c(cur_line,cur_index)
    }
    else if(abs(tm-cur_y)<50){
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

#Various papers for later reference
#http://old.cescg.org/CESCG-2008/papers/BratislavaC-Bozekova-Miroslava.pdf
#Below are unused / unneeded functions

#Reserved Debug Function Call
#loop_info(looptest,dim(img))
#End Debug

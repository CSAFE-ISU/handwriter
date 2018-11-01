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
    y1 = rowList[i]
    x1 = colList[i]
    i_index = loopList[i]
    for(j in 1:length(rowList)){
      y2 = rowList[j]
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

#takes list of a grapheme, converts to row,col  
toRC = function(nodes, dims)
{
  cs = (nodes-1)%/%dims[1] + 1
  rs = (nodes-1)%%dims[1] + 1
  return(matrix(c(rs,cs), ncol = 2))
}
#same as toRC but associates indicies too
toRCi = function(nodes, dims)
{
  cs = (nodes-1)%/%dims[1] + 1
  rs = (nodes-1)%%dims[1] + 1
  rowcolmatrix = matrix(c(rs,cs,nodes), ncol = 3)
  colnames(rowcolmatrix) = c('y','x','index')
  return(rowcolmatrix)
}

#(column-1)*dim(image)[1] + row.
#ehh really weird behavior with non-ints
rc_to_i = function(row_y,col_x,img_dim)
{
  row_y = as.integer(row_y)
  col_x = as.integer(col_x)
  return((col_x-1)*img_dim[1]+row_y)
}


#returns list of {aspect ratio, vertical dist, horiz dist}
get_aspect_ratio = function(grapheme_list, img_dim)
{
  rowcol = toRCi(grapheme_list,img_dim)
  rows_y = rowcol[,'y'] 
  cols_x = rowcol[,'x']
  row_dist = max(rows_y) - min(rows_y) #vertical distance
  col_dist = max(cols_x) - min(cols_x) #horizontal distance
  aspect_info = list(aspect_ratio = row_dist/col_dist,height = row_dist, width = col_dist,topmost_row = min(rows_y),bottom_row = max(rows_y),leftmost_col=min(cols_x),rightmost_col=max(cols_x))
  return(aspect_info)
}
#gets centroid, calculates what % of the way the centroid appears both vertically and horizontally
get_centroid = function(grapheme_list, img_dim)
{
  rowcol = toRCi(grapheme_list,img_dim)
  rows_y = rowcol[,'y'] 
  cols_x = rowcol[,'x']
  centroid_row = mean(rows_y)
  centroid_col = mean(cols_x)
  row_dist = max(rows_y) - min(rows_y) #vertical distance
  col_dist = max(cols_x) - min(cols_x) #horizontal distance
  centroid_index = rc_to_i(centroid_row,centroid_col,img_dim)
  #bad?
  #centroid_horiz_location = (min(cols_x)+(centroid_col-min(cols_x))) / col_dist
  #centroid_vert_location = (min(rows_y)+(centroid_row-min(rows_y))) / row_dist
  
  centroid_horiz_location = (centroid_col-min(cols_x)) / col_dist
  centroid_vert_location = (centroid_row-min(rows_y)) / row_dist
  #used for getting skew, assuming centroid is more middle than the median col_x
  lHalf = list(rows_y = rows_y[which(cols_x<centroid_col)],cols_x = cols_x[which(cols_x<centroid_col)])
  rHalf = list(rows_y = rows_y[which(cols_x>centroid_col)],cols_x = cols_x[which(cols_x>centroid_col)])
  lHalfCentroidrc = list(y=mean(lHalf$rows_y),x=mean(lHalf$cols_x))
  rHalfCentroidrc = list(y=mean(rHalf$rows_y),x=mean(rHalf$cols_x))
  lHalfCentroidi = rc_to_i(mean(lHalf$rows_y),mean(lHalf$cols_x),img_dim)
  rHalfCentroidi = rc_to_i(mean(rHalf$rows_y),mean(rHalf$cols_x),img_dim)
  #indices of each half
  lHi = rc_to_i(lHalf$rows_y,lHalf$cols_x,img_dim)
  rHi = rc_to_i(rHalf$rows_y,rHalf$cols_x,img_dim)
  #finding slope
  slope = ((img_dim[1] - rHalfCentroidrc$y)-(img_dim[1] - lHalfCentroidrc$y))/(rHalfCentroidrc$x-lHalfCentroidrc$x)
  lHalfCentroid = rc_to_i(mean(lHalf$rows_y),mean(lHalf$cols_x),img_dim)
  centroid_info = list(centroid_index = centroid_index, centroid_y = centroid_row, centroid_x = centroid_col, centroid_horiz_location = centroid_horiz_location,centroid_vert_location = centroid_vert_location,lHalf = lHi,rHalf=rHi,disjoint_centroids = list(left = lHalfCentroidi,right = rHalfCentroidi),slope = slope)
  return(centroid_info)
}
#so x = processHandwriting(), x$graphemeList is what grapheme_lists should be
#processes a list of graphemes, returns list of graphemes at a list of features
#heres an idea, passing in all of x to associate stuff like loop quantity with graphemes too
graphemes_to_features = function(grapheme_lists,img_dim){
  grapheme_feature_list = list()
  for(i in 1:length(grapheme_lists)){
    cur_features = grapheme_to_features(grapheme_lists[[i]]$path,img_dim)
    grapheme_feature_list = append(grapheme_feature_list,list(cur_features))
  }
  return(grapheme_feature_list)
}
grapheme_to_features = function(grapheme_list, img_dim){
  aspect_info = get_aspect_ratio(grapheme_list,img_dim)
  centroid_info = get_centroid(grapheme_list,img_dim)
  features = c(aspect_info,centroid_info)
  return(features)
}


#feature ideas etc:
#quantity of loops

#skew idea:
#http://old.cescg.org/CESCG-2008/papers/BratislavaC-Bozekova-Miroslava.pdf
#draw and tilt the letters relative to angles, inside of the box that the grapheme takes up count the # of black pixels in each column
#take the tilt in degree (or value) on which ever one has the most amount of black pixels in each column

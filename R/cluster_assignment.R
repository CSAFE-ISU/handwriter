# The handwriter R package performs writership analysis of handwritten documents.
# Copyright (C) 2021 Iowa State University of Science and Technology on behalf of its Center for Statistics and Applications in Forensic Evidence
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


#' get_clusters_batch
#'
#' @param template A cluster template created with [`make_clustering_template`]
#' @param input_dir A directory containing graphs created with
#'   [`process_batch_dir`]
#' @param output_dir Output directory for cluster assignments
#' @param writer_indices Vector of start and end indices for the writer id in
#'   the graph file names
#' @param doc_indices Vector of start and end indices for the document id in the
#'   graph file names
#' @param num_cores Integer number of cores to use for parallel processing
#' @param save_master_file TRUE or FALSE. If TRUE, a master file named
#'   'all_clusters.rds' containing the cluster assignments for all documents in
#'   the input directory will be saved to the output directory. If FASLE, a
#'   master file will not be saved, but the individual files for each document
#'   in the input directory will still be saved to the output directory.
#'
#' @return A list of cluster assignments
#'
#' @examples
#' \dontrun{
#' template <- readRDS('path/to/template.rds')
#' get_clusters_batch(template=template, input_dir='path/to/dir', output_dir='path/to/dir',
#' writer_indices=c(2,5), doc_indices=c(7,18), num_cores=1)
#'
#' get_clusters_batch(template=template, input_dir='path/to/dir', output_dir='path/to/dir',
#' writer_indices=c(1,4), doc_indices=c(5,10), num_cores=5)
#' }
#'
#' @export
#' @md
get_clusters_batch <- function(template, input_dir, output_dir, writer_indices, doc_indices, num_cores = 1, save_master_file = FALSE) {
  # bind global variables to fix check() note
  i <- outliercut <- docname <- NULL

  # check num_cores
  if (length(num_cores) > 1){
    stop("num_cores is longer than 1")
  } else if (!is.numeric(num_cores)){ 
    stop("num_cores is not numeric")
  } else if (num_cores %% 1 != 0) {
    stop("num_cores is not an integer")
  } else if (num_cores < 1) {
    stop("num_cores is not greater than or equal to 1")
  }

  # make output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # list files in input dir
  proclist <- list.files(input_dir, pattern = ".rds", full.names = TRUE)

  if (num_cores > 1) { # run in parallel
    my_cluster <- parallel::makeCluster(num_cores)
    doParallel::registerDoParallel(my_cluster)

    proclist <- foreach::foreach(
      i = 1:length(proclist),
      .combine = "rbind",
      .export = c(
        "AddLetterImages",
        "angle",
        "centeredImage",
        "delete_graphs",
        "list_images",
        "make_clusters_df",
        "makeassignment",
        "MakeLetterListLetterSpecific",
        "move_problem_file")
    ) %dopar% { # for each document i

      doc <- readRDS(proclist[i])

      # check that doc$docname is not blank
      if (!("docname" %in% names(doc))) {
        move_problem_file(path = proclist[i], output_dir = output_dir)
        return()
      }
      
      # return warning if document doesn't contain any graphs (E.g. document is blank)
      if (!length(doc$process$letterList)){
        return()
      }

      # load outfile if it already exists
      outfile <- file.path(output_dir, paste0(stringr::str_replace(doc$docname, ".png", ""), ".rds"))
      if (file.exists(outfile)) {
        df <- readRDS(outfile)
        return(df)
      }

      # extra processing
      doc$process$letterList <- AddLetterImages(doc$process$letterList, dim(doc$image))
      doc$process$letterList <- MakeLetterListLetterSpecific(doc$process$letterList, dim(doc$image)) ### THIS SCREWS UP PLOTLETTER AND OTHER PLOTTING!!!
    
      # delete graphs that have more than 30 paths
      doc <- delete_graphs(doc = doc, max_edges = 30)
      
      imagesList <- list_images(doc)

      # get cluster assignments
      cluster_assign <- sapply(imagesList, makeassignment, templateCenterList = template$centers, outliercut = outliercut)
      
      df <- make_clusters_df(cluster_assign, doc, writer_indices, doc_indices)

      saveRDS(df, file = outfile)
      
      return(df)
    }
  } else { # run sequentially
    out_proclist <- list()
    for (i in 1:length(proclist)) {
      # load doc
      message(paste("Loading graphs for", basename(proclist[i])))
      doc <- readRDS(proclist[i])

      # check that doc$docname is not blank
      if (!("docname" %in% names(doc))) {
        move_problem_file(path = proclist[i], output_dir = output_dir)
        message(paste("docname is NULL for", path, "\n"))
        next
      }
      
      # return warning if document doesn't contain any graphs (E.g. document is blank)
      if (!length(doc$process$letterList)){
        warning("The document does not contain any graphs.")
        next
      }

      # load outfile if it already exists
      outfile <- file.path(output_dir, paste0(stringr::str_replace(doc$docname, ".png", ""), ".rds"))
      if (file.exists(outfile)) {
        message(paste("     Cluster assignments already exist for", doc$docname, "\n"))
        df <- readRDS(outfile)
        out_proclist[[i]] <- df
        next
      }

      # extra processing
      doc$process$letterList <- AddLetterImages(doc$process$letterList, dim(doc$image))
      doc$process$letterList <- MakeLetterListLetterSpecific(doc$process$letterList, dim(doc$image)) ### THIS SCREWS UP PLOTLETTER AND OTHER PLOTTING!!!

      # delete graphs that have more than 30 paths
      doc <- delete_graphs(doc = doc, max_edges = 30)
      
      imagesList <- list_images(doc)

      # get cluster assignments
      message(paste("Getting cluster assignments for", doc$docname))
      cluster_assign <- sapply(imagesList, makeassignment, templateCenterList = template$centers, outliercut = outliercut)

      df <- make_clusters_df(cluster_assign, doc, writer_indices, doc_indices)

      saveRDS(df, file = outfile)
      message(paste("Saving cluster assignments for ", doc$docname, "\n"))

      out_proclist[[i]] <- df
    }
    # rename
    proclist <- do.call(rbind, out_proclist)
  }
  
  # check for missing clusters. NOTE: The foreach loop could be restructured to
  # allow a warning to be given if a document without graphs is encountered and
  # this message could be removed.
  outfiles <- list.files(output_dir, pattern = ".rds", full.names = TRUE)
  outfiles <- outfiles[which(basename(outfiles) != "all_clusters.rds")]
  if (length(outfiles) != length(unique(proclist$docname))){
    warning('Unable to get cluster assignments for one or more documents.')
  }

  # save clusters
  if (save_master_file){
    saveRDS(proclist, file.path(output_dir, "all_clusters.rds"))
  }

  return(proclist)
}


# Internal Functions ------------------------------------------------------

#' Create a Directory
#'
#' Create a directory if it doesn't already exist. The user can optionally
#' display a message in the console.
#'
#' @param path Path to new directory
#' @param msg Optional message to display in the console
#' @param ... Optional arguments passed to 'dir.create'
#'
#' @return Nothing is returned
#'
#' @noRd
create_dir <- function(path, msg = NULL, ...) {
  if (!is.null(msg)){
    message(msg)
  }
  
  if (!dir.exists(path)) {
    dir.create(path, ...)
  } else {
    message("Directory already exists.")
  }
}

move_problem_file <- function(path, output_dir) {
  create_dir(path = file.path(output_dir, "problem_files"))
  file.rename(path, file.path(output_dir, "problem_files", basename(path)))
  
}

list_images <- function(doc) {
  imagesList <- list()
  imagesList <- c(imagesList, lapply(doc$process$letterList, function(x) {
    centeredImage(x)
  }))
  imagesList <- lapply(imagesList, function(x) {
    x$nodesrc <- cbind(((x$nodes - 1) %/% dim(x$image)[1]) + 1, dim(x$image)[1] - ((x$nodes - 1) %% dim(x$image)[1]))
    x$nodesrc <- x$nodesrc - matrix(rep(x$centroid, each = dim(x$nodesrc)[1]), ncol = 2)
    x$pathEndsrc <- lapply(x$allPaths, function(z) {
      cbind(((z[c(1, length(z))] - 1) %/% dim(x$image)[1]) + 1, dim(x$image)[1] - ((z[c(1, length(z))] - 1) %% dim(x$image)[1]))
    })
    x$pathEndsrc <- lapply(x$pathEndsrc, function(z) {
      z - matrix(rep(x$centroid, each = 2), ncol = 2)
    })
    return(x)
  })
  return(imagesList)
}

makeassignment <- function(imageListElement, templateCenterList, outliercut) {
  dist <- min(unlist(lapply(templateCenterList, function(x) {
    getGraphDistance(imageList1 = imageListElement, imageList2 = x, isProto2 = TRUE)$matching_weight
  })))
  cluster <- which.min(unlist(lapply(templateCenterList, function(x) {
    getGraphDistance(imageList1 = imageListElement, imageList2 = x, isProto2 = TRUE)$matching_weight
  })))
  return(cluster)
}

#' MakeLetterListLetterSpecific
#'
#' Description
#' 
#' @param letterList List of letters in a handwriting sample
#' @param dims Dimensions of the handwriting sample
#' @return letterList with locations given with respect to each letter
#' @noRd
MakeLetterListLetterSpecific = function(letterList, dims)
{
  # NOTE: There are two ways to specify the location of the paths and nodes in a
  # letter. 1) by index number - each pixel in the letter's image (binary
  # matrix) is numbered moving top to bottom and left to right. index =
  # matrix(1:15, nrow=5, ncol=3) shows the index numbers of the pixels in a 5x3
  # image. 2) by row and column numbers - each pixel in the letter's image can
  # be referred to by its row number and column number. Rows are numbered from
  # top to bottom and columns are numbered from left to right. Much of the code
  # in this function switches between the two location types.
  
  # Make a list of the path from each letter. skeletons[[i]] contains the
  # locations (index numbers) of the path of the i-th letter.
  skeletons = lapply(letterList, function(x) x$path)
  
  # Make a list of the nodes in each letter. nodes[[i]] contains the
  # locations (index numbers) of the nodes in the i-th letter.
  nodes = lapply(letterList, function(x) x$nodes)
  
  # For each letter in the handwriting sample
  for(i in 1:length(letterList))
  { # Find locations (row and column numbers) of the path of the letter
    # relative to the handwriting sample
    path_rc = i_hs_to_rc_hs(index_nums = skeletons[[i]], hs_num_rows = dims[1])
    
    # Find the row number of the top of the letter
    letter_topmost_row = min(path_rc$row)
    
    # Find the row number of the bottom of the letter
    letter_bottom_row = max(path_rc$row)
    
    # Find the column number of the leftmost column of the letter
    letter_leftmost_col = min(path_rc$col)
    
    # Find the number of rows spanned by the letter
    letter_num_rows  = letter_bottom_row - letter_topmost_row + 1
    
    # Find the locations (row and column number) of the nodes relative to the
    # top and left side of the letter
    nodes_rc = i_hs_to_rc_letter(index_nums = nodes[[i]], 
                                 hs_num_rows = dims[1], 
                                 letter_topmost_row = letter_topmost_row,
                                 letter_leftmost_col = letter_leftmost_col)
    
    # Find the locations (index numbers) of each path in the letter relative to the top and left
    # side of the letter
    letterList[[i]]$allPaths = lapply(letterList[[i]]$allPaths, 
                                      function(x){
                                        # Find the locations (index numbers) of the path
                                        # in the letter image
                                        i_hs_to_i_letter(index_nums = x, 
                                                         hs_num_rows = dims[1],
                                                         letter_num_rows = letter_num_rows,
                                                         letter_topmost_row = letter_topmost_row,
                                                         letter_leftmost_col = letter_leftmost_col)
                                      })
    
    # Find the locations (index numbers) of the nodes in the adjacency matrix relative to the 
    # handwriting sample
    nameVect_i = i_hs_to_i_letter(index_nums = as.numeric(colnames(letterList[[i]]$adjMatrix)), 
                                  hs_num_rows = dims[1], 
                                  letter_num_rows = letter_num_rows,
                                  letter_topmost_row = letter_topmost_row,
                                  letter_leftmost_col = letter_leftmost_col)
    
    # Change the row and column names of the adjacency matrix to the locations (index numbers)
    # of the nodes relative to the top and left of the letter
    colnames(letterList[[i]]$adjMatrix) = format(nameVect_i, scientific = FALSE, trim = TRUE)
    rownames(letterList[[i]]$adjMatrix) = colnames(letterList[[i]]$adjMatrix)
    
    # Find the locations (row and column numbers) of the letter's centroid relative to the 
    # top and left of the letter
    centroid_rc = rc_hs_to_rc_letter(row_nums = letterList[[i]]$characterFeatures$centroid_y,
                                     col_nums = letterList[[i]]$characterFeatures$centroid_x,
                                     letter_topmost_row = letter_topmost_row,
                                     letter_leftmost_col = letter_leftmost_col)
    letterList[[i]]$characterFeatures$centroid_y = centroid_rc$row
    letterList[[i]]$characterFeatures$centroid_x = centroid_rc$col
    
    # Store the locations (row and column numbers) of lHalf relative to the
    # top and left of the letter.
    lHalf_rc = i_hs_to_rc_letter(index_nums = letterList[[i]]$characterFeatures$lHalf, 
                                 hs_num_rows = dims[1], 
                                 letter_topmost_row = letter_topmost_row,
                                 letter_leftmost_col = letter_leftmost_col)
    letterList[[i]]$characterFeatures$lHalfr = lHalf_rc$row
    letterList[[i]]$characterFeatures$lHalfc = lHalf_rc$col
    
    # Store the locations (row and column numbers) of rHalf relative to the
    # top and left of the letter.
    rHalf_rc = i_hs_to_rc_letter(index_nums = letterList[[i]]$characterFeatures$rHalf, 
                                 hs_num_rows = dims[1], 
                                 letter_topmost_row = letter_topmost_row,
                                 letter_leftmost_col = letter_leftmost_col)
    letterList[[i]]$characterFeatures$rHalfr = rHalf_rc$row
    letterList[[i]]$characterFeatures$rHalfc = rHalf_rc$col
    
    # Find the locations (row and column numbers) of the left and right centroids
    # relative to the top and left of the letter
    letterList[[i]]$characterFeatures$lCentroid = c(mean(lHalf_rc$row), mean(lHalf_rc$col))
    letterList[[i]]$characterFeatures$rCentroid = c(mean(rHalf_rc$row), mean(rHalf_rc$col))
    
    # Store the locations (index number) of lHalf and rHalf relative to the
    # top and left of the letter
    letterList[[i]]$characterFeatures$lHalf = i_hs_to_i_letter(index_nums = letterList[[i]]$characterFeatures$lHalf, 
                                                               hs_num_rows = dims[1], 
                                                               letter_num_rows = letter_num_rows,
                                                               letter_topmost_row = letter_topmost_row,
                                                               letter_leftmost_col = letter_leftmost_col)
    letterList[[i]]$characterFeatures$rHalf = i_hs_to_i_letter(index_nums = letterList[[i]]$characterFeatures$rHalf, 
                                                               hs_num_rows = dims[1], 
                                                               letter_num_rows = letter_num_rows,
                                                               letter_topmost_row = letter_topmost_row,
                                                               letter_leftmost_col = letter_leftmost_col)
    
    # Set the locations (row and column numbers) of the bottom row and rightmost column of the letter
    # relative to the top and left of the letter. Set the topmost row and leftmost column both to 1.
    bottom_right_rc = rc_hs_to_rc_letter(row_nums = letterList[[i]]$characterFeatures$bottom_row,
                                         col_nums = letterList[[i]]$characterFeatures$rightmost_col,
                                         letter_topmost_row = letter_topmost_row,
                                         letter_leftmost_col = letter_leftmost_col)
    letterList[[i]]$characterFeatures$bottom_row = bottom_right_rc$row
    letterList[[i]]$characterFeatures$rightmost_col = bottom_right_rc$col
    letterList[[i]]$characterFeatures$topmost_row = 1
    letterList[[i]]$characterFeatures$leftmost_col = 1
    
    # Set the locations (index numbers) of the path of the letter 
    # relative to the top and left of the letter
    letterList[[i]]$path = i_hs_to_i_letter(index_nums = letterList[[i]]$path,
                                            hs_num_rows = dims[1],
                                            letter_num_rows = letter_num_rows,
                                            letter_topmost_row = letter_topmost_row,
                                            letter_leftmost_col = letter_leftmost_col)
    
    # Set the locations (index numbers) of the nodes in the letter 
    # relative to the top and left of the letter
    letterList[[i]]$nodes = i_hs_to_i_letter(index_nums = letterList[[i]]$nodes,
                                             hs_num_rows = dims[1],
                                             letter_num_rows = letter_num_rows,
                                             letter_topmost_row = letter_topmost_row,
                                             letter_leftmost_col = letter_leftmost_col)
  }
  return(letterList)
}

make_clusters_df <- function(cluster_assign, doc, writer_indices, doc_indices) {
  # calculate pc rotation angle and wrapped pc rotation angle
  # NOTE: foreach can't find get_pc_rotation unless it is nested in make_clusters_df
  get_pc_rotation <- function(x) {
    xv <- as.numeric(x["xvar"])
    yv <- as.numeric(x["yvar"])
    cv <- as.numeric(x["covar"])
    eig <- eigen(cbind(c(xv, cv), c(cv, yv)), symmetric = TRUE)
    return(angle(t(as.matrix(eig$vectors[, 1])), as.matrix(c(1, 0))))
  }
  
  df <- data.frame(cluster = cluster_assign)
  
  # add docname, writer, doc, slope, xvar, yvar, and covar
  df$docname <- doc$docname
  df$writer <- sapply(df$docname, function(x) substr(x, start = writer_indices[1], stop = writer_indices[2]))
  df$doc <- sapply(df$docname, function(x) substr(x, start = doc_indices[1], stop = doc_indices[2]), USE.NAMES = FALSE)
  df$slope <- sapply(doc$process$letterList, function(x) x$characterFeatures$slope)
  df$xvar <- sapply(doc$process$letterList, function(x) x$characterFeatures$xvar)
  df$yvar <- sapply(doc$process$letterList, function(x) x$characterFeatures$yvar)
  df$covar <- sapply(doc$process$letterList, function(x) x$characterFeatures$covar)
  df$pc_rotation <- apply(df, 1, get_pc_rotation)
  df$pc_wrapped <- 2 * df$pc_rotation
  
  # sort columns
  df <- df[, c("docname", "writer", "doc", "cluster", "slope", "xvar", "yvar", "covar", "pc_rotation", "pc_wrapped")]
  return(df)
}

#' Delete Graphs
#'
#' Delete any graphs from a processed document that have more than the maximum
#' number of edges. 
#'
#' NOTE: `delete_graphs` counts the number of paths (edges) per graph
#' and uses that number to determine which, if any, graphs to delete.
#' `delete_crazy_graphs` groups graphs by stata - 1 loop, 2 loops, 1 edge, 2
#' edges, 3 edges, and so on. However, if a graphs with tons of paths (edges)
#' also has 1 or 2 loops, the graph will have the strata "1 loop" or "2 loops" 
#' and will NOT be deleted by `delete_crazy_graphs`.
#'
#' @param doc A document processed with `processDocument`
#' @param max_edges Graphs with more than the maximum number of edges will be
#'   deleted from `doc$process$letterList`
#'
#' @return A list
#'
#' @noRd
delete_graphs <- function(doc, max_edges = 30){
  lengths <- sapply(doc$process$letterList, function(x) length(x$allPaths))
  doc$process$letterList <- doc$process$letterList[which(lengths <= 30)]
  return(doc)
}

#' get_clusterassignment
#'
#' An internal function for getting cluster assignments for model or questioned
#' documents. This function runs 'get_clusters_batch'.
#'
#' @param main_dir Directory containing a cluster template created with
#'   `make_clustering_template`
#' @param input_type `model` or `questioned`
#' @param writer_indices Vector of start and end indices for the writer id in
#'   the document names
#' @param doc_indices Vector of start and end indices for the document id in the
#'   document names
#' @param num_cores Integer number of cores to use for parallel processing
#'
#' @return list of processed handwriting with cluster assignments for each graph
#'
#' @noRd
get_clusterassignment <- function(main_dir, input_type, writer_indices, doc_indices, num_cores) {
  # bind global variables to fix check() note
  i <- outliercut <- docname <- NULL

  # load cluster file if it already exists
  if (input_type == "model") {
    cluster_file <- file.path(main_dir, "data", "model_clusters.rds")
  } else if (input_type == "questioned") {
    cluster_file <- file.path(main_dir, "data", "questioned_clusters.rds")
  } else {
    stop("Unknown input type. Use model or questioned.")
  }
  if (file.exists(cluster_file)) {
    proclist <- readRDS(cluster_file)
    return(proclist)
  }

  # load template
  if (file.exists(file.path(main_dir, "data", "template.rds"))) {
    template <- readRDS(file.path(main_dir, "data", "template.rds"))
  } else {
    stop(paste("There is no cluster template in", main_dir))
  }

  # get input directory
  if (input_type == "model") {
    input_dir <- file.path(main_dir, "data", "model_graphs")
  } else {
    input_dir <- file.path(main_dir, "data", "questioned_graphs")
  }

  # make output directory
  if (input_type == "model") {
    output_dir <- file.path(main_dir, "data", "model_clusters")
  } else {
    output_dir <- file.path(main_dir, "data", "questioned_clusters")
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  # list files in input dir
  proclist <- list.files(input_dir, pattern = '.rds', full.names = TRUE)
  
  # get cluster assignments  
  proclist <- get_clusters_batch(template, input_dir, output_dir, writer_indices, doc_indices, num_cores, save_master_file = FALSE)

  # save clusters in output dir
  if (input_type == "model") {
    saveRDS(proclist, file.path(main_dir, "data", "model_clusters.rds"))
  } else {
    saveRDS(proclist, file.path(main_dir, "data", "questioned_clusters.rds"))
  }

  return(proclist)
}


GetImageMatrix <- function(letterList, maxImageSize = 50) {
  imagesList <- list()
  imagesList <- c(imagesList, lapply(letterList, function(x) {
    centeredImage(x)
  }))

  # letterList = unlist(letListFull,recursive=FALSE)

  for (i in 1:length(imagesList)) {
    l <- imagesList[[i]]$centroid[1]
    r <- dim(imagesList[[i]]$image)[2] - imagesList[[i]]$centroid[1] + 1
    t <- dim(imagesList[[i]]$image)[1] - imagesList[[i]]$centroid[2] + 1
    b <- imagesList[[i]]$centroid[2]
    if (l > r) {
      imagesList[[i]]$image <- cbind(imagesList[[i]]$image, matrix(1, ncol = l - r, nrow = dim(imagesList[[i]]$image)[1]))
    } else if (l < r) {
      imagesList[[i]]$image <- cbind(matrix(1, ncol = r - l, nrow = dim(imagesList[[i]]$image)[1]), imagesList[[i]]$image)
    }
    if (t > b) {
      imagesList[[i]]$image <- rbind(imagesList[[i]]$image, matrix(1, nrow = t - b, ncol = dim(imagesList[[i]]$image)[2]))
    } else if (t < b) {
      imagesList[[i]]$image <- rbind(matrix(1, nrow = b - t, ncol = dim(imagesList[[i]]$image)[2]), imagesList[[i]]$image)
    }
  }

  for (i in 1:length(imagesList)) {
    if (any(dim(imagesList[[i]]$image) > maxImageSize)) {
      imagesList[[i]]$image <- imagesList[[i]]$image %>%
        as.raster() %>%
        magick::image_read() %>%
        magick::image_resize(paste0(maxImageSize, "x", maxImageSize)) %>%
        magick::image_quantize(max = 2, dither = FALSE, colorspace = "gray") %>%
        `[[`(1) %>%
        as.numeric() %>%
        `[`(, , 1)
      imagesList[[i]]$image <- rbind(1, cbind(1, imagesList[[i]]$image, 1), 1)
      thinned <- thinImage(imagesList[[i]]$image)
      imagesList[[i]]$image[] <- 1
      imagesList[[i]]$image[thinned] <- 0

      imagesList[[i]]$image <- imagesList[[i]]$image[-c(1, dim(imagesList[[i]]$image)[1]), -c(1, dim(imagesList[[i]]$image)[2])]
      # print(plotImage(imagesList[[i]]$image) + theme_bw())
      cat(i, " ")
    }
  }

  for (i in 1:length(imagesList)) {
    dims <- dim(imagesList[[i]]$image)
    lrPad <- maxImageSize + 2 - dims[2]
    tbPad <- maxImageSize + 2 - dims[1]
    l <- floor(lrPad / 2)
    r <- ceiling(lrPad / 2)
    b <- ceiling(tbPad / 2)
    t <- floor(tbPad / 2)

    imagesList[[i]]$image <- rbind(matrix(1, nrow = t, ncol = dims[2]), imagesList[[i]]$image, matrix(1, nrow = b, ncol = dims[2]))
    imagesList[[i]]$image <- cbind(matrix(1, ncol = l, nrow = dim(imagesList[[i]]$image)[1]), imagesList[[i]]$image, matrix(1, ncol = r, nrow = dim(imagesList[[i]]$image)[1]))

    imagesList[[i]]$image <- imagesList[[i]]$image[, -c(1, maxImageSize + 2)]
    imagesList[[i]]$image <- imagesList[[i]]$image[-c(1, maxImageSize + 2), ]
  }
  # apply(matrix(unlist(lapply(imagesList, function(x){dim(x$image)})), ncol = 2, byrow = TRUE), 2, function(x){all(x == maxImageSize)})


  images <- array(NA, c(maxImageSize, maxImageSize, length(imagesList)))
  for (i in 1:length(imagesList)) {
    images[, , i] <- imagesList[[i]]$image
  }
  return(images)
}

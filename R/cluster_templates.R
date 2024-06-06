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


# EXPORTED ----------------------------------------------------------------


#' Make Clustering Template
#'
#' `make_clustering_template()` applies a K-means clustering algorithm to the
#' input handwriting samples pre-processed with [`process_batch_dir()`] and saved
#' in the input folder `main_dir > data > template_graphs`. The K-means
#' algorithm sorts the graphs in the input handwriting samples into groups, or
#' *clusters*, of similar graphs.
#'
#' @param main_dir Main directory that will store template files
#' @param template_docs A directory containing template training images
#' @param writer_indices A vector of the starting and ending location of the
#'   writer ID in the file name.
#' @param centers_seed Integer seed for the random number generator when
#'   selecting starting cluster centers.
#' @param K Integer number of clusters
#' @param num_dist_cores Integer number of cores to use for the distance
#'   calculations in the K-means algorithm. Each iteration of the K-means
#'   algorithm calculates the distance between each input graph and each cluster
#'   center.
#' @param max_iters Maximum number of iterations to allow the K-means algorithm
#'   to run
#' @return List containing the cluster template
#'
#' @examples
#' \dontrun{
#' main_dir <- "path/to/folder"
#' template_docs <- system.file("extdata/example_images/template_training_images",
#'   package = "handwriter"
#' )
#' template_list <- make_clustering_template(
#'   main_dir = main_dir,
#'   template_docs = template_docs,
#'   writer_indices = c(2, 5),
#'   K = 10,
#'   num_dist_cores = 2,
#'   max_iters = 25,
#'   centers_seed = 100,
#' )
#' }
#'
#' @export
#' @md
make_clustering_template <- function(main_dir,
                                     template_docs,
                                     writer_indices,
                                     centers_seed,
                                     K = 40,
                                     num_dist_cores = 1,
                                     max_iters = 25) {
  
  options(scipen = 999)
  
  # Check maximum number of iterations ----
  if (max_iters < 25) {
    warning("For case-work, the maximum number of iterations must be greater than or equal to 25. Fewer iterations are only intended for development testing.")
  }
  
  # Check number of clusters ----
  if (K != 40) {
    warning("For case-work, the number of clusters K must be 40. Other numbers of clusters are only intended for development testing.")
  }
  
  # Set internal parameters ----
  num_path_cuts = 8
  max_edges = 30
  gamma = 3
  
  # Setup folders ----
  do_setup(main_dir = main_dir)
  
  # Process training documents ----
  message("Processing template training documents...")
  process_batch_dir(
    input_dir = template_docs,
    output_dir = file.path(main_dir, "data", "template_graphs")
  )
  
  # Make proclist ----
  template_proc_list <- make_proc_list(main_dir = main_dir)
  
  # Delete large graphs ----
  # Make table of number of graphs with various numbers of loops and edges
  strata <- get_strata(template_proc_list = template_proc_list, main_dir = main_dir)
  
  # Delete graphs with too many edges
  template_proc_list <- delete_crazy_graphs(template_proc_list = template_proc_list, max_edges = max_edges, main_dir = main_dir)
  
  # Make images list ----
  template_images_list <- make_images_list(
    template_proc_list = template_proc_list,
    main_dir = main_dir,
    writer_indices = writer_indices
  )
  
  # Set outliers parameter
  num_outliers <- round(.25 * length(template_images_list))
  
  # Choose cluster centers. NOTE: Even if you are testing the code on a small number of
  # graphs, you need to select centers from the full list of graphs.
  message("Choosing starting cluster centers...")
  centers <- chooseCenters(seed = centers_seed, K = K, template_proc_list = template_proc_list, template_images_list = template_images_list)
  
  # Run Kmeans
  template <- letterKmeansWithOutlier_parallel(
    template_proc_list = template_proc_list,
    template_images_list = template_images_list,
    K = K,
    num_path_cuts = num_path_cuts,
    max_iters = max_iters,
    gamma = gamma,
    num_outliers = num_outliers,
    centers = centers,
    num_dist_cores = num_dist_cores,
    centers_seed = centers_seed
  )
  message("Saving template...")
  saveRDS(template, file = file.path(main_dir, "data", "template.rds"))
  return(template)
}


# Internal Functions ------------------------------------------------------

#' AddSamplingStrata
#'
#' Add two new fields, numloops and stratum, to the characterFeatures list for
#' each letter in letterList.
#'
#' @param letterList List of letters
#' @return List of letters
#'
#' @noRd
AddSamplingStrata = function(letterList){
  # For each letter in the sample
  for(i in 1:length(letterList)){
    # Count the number of loops in the letter and store as a new field called numloops under characterFields
    letterList[[i]]$characterFeatures$numloops = length(loop_extract(letterList[[i]]$allPaths))
    # Add a new field called stratum under characterFields, where stratum is "1loop", "2loops", or the length of allPaths
    if(letterList[[i]]$characterFeatures$numloops == 2){
      letterList[[i]]$characterFeatures$stratum = "2loop"
    } else if(letterList[[i]]$characterFeatures$numloops == 1){
      letterList[[i]]$characterFeatures$stratum = "1loop"
    } else {
      letterList[[i]]$characterFeatures$stratum = length(letterList[[i]]$allPaths)
    }
  }
  return(letterList)
}



#' Make Processed List
#'
#' `process_batch_dir()` needs to be run first to processes handwriting documents
#' in a specified folder. `process_batch_dir()` creates an RDS file for each
#' document that contains the extracted graphs in the document and saves the
#' file in `main_dir > data > template_graphs` in rds files. `make_proc_list()`
#' loads the graph RDS files from `main_dir > data > template_graphs` into a
#' single list. This function also adds the graph image matrix and the number of
#' loops and edges to each item in the list. This function also records the
#' locations information of each graph with respect to the individual graph
#' instead of the handwriting document.
#'
#' @param main_dir Input directory
#' @return List containing graphs prepared for template creation.
#'
#' @noRd
make_proc_list <- function(main_dir) {
  
  # List files in template directory > data > template_graphs
  df <- data.frame(graph_paths = list.files(file.path(main_dir, "data", "template_graphs"), pattern = ".rds", full.names = TRUE), stringsAsFactors = FALSE)
  
  # Load graphs
  message("Loading processed template training documents...")
  template_proc_list <- lapply(df$graph_paths, readRDS)
  
  # Get the image plot (binarized matrix) of each graph from each handwriting sample
  message("Adding image plots for each graph...")
  template_proc_list <- lapply(template_proc_list, function(x) {
    x$process$letterList <- AddLetterImages(x$process$letterList, dim(x$image))
    return(x)
  })
  
  # Get the number of loops and edges in each graph
  message("Adding sampling strata for each graph...")
  template_proc_list <- lapply(template_proc_list, function(x) {
    x$process$letterList <- AddSamplingStrata(x$process$letterList)
    return(x)
  })
  
  # For each graph in each handwriting sample, get the locations of the nodes,
  # paths, centroids, and other items with respect to the individual graph
  # instead of the handwriting sample. WARNING: This messes up plotting.
  message("Reorienting graphs...")
  template_proc_list <- lapply(template_proc_list, function(x) {
    x$process$letterList <- MakeLetterListLetterSpecific(x$process$letterList, dim(x$image))
    return(x)
  }) # THIS MESSES UP PLOTTING!!
  
  return(template_proc_list)
}


#' Get Strata
#'
#' `get_strata()` creates a table that shows the number of graphs in template_proc_list for
#' each strata (number of loops or edges)
#'
#' @param template_proc_list List of graphs output by make_proc_list()
#' @param main_dir Input directory
#' @return Dataframe of number of graphs per strata. The dataframe is saved in
#'   tempalte_dir > data.
#'
#' @noRd
get_strata <- function(template_proc_list, main_dir) {
  tic <- Sys.time() # start timer
  message("Start making a dataframe of the number of graphs with various numbers of loops and edges...")
  
  # Set Max Number of Edges Per Graph -------------------------------------------------
  # Make vectors of document #, letter #, and strata for each graph
  doc0 <- letter0 <- stratum0 <- c()
  for (i in 1:length(template_proc_list)) {
    for (j in 1:length(template_proc_list[[i]]$process$letterList)) {
      doc0 <- c(doc0, i)
      letter0 <- c(letter0, j)
      stratum0 <- c(stratum0, template_proc_list[[i]]$process$letterList[[j]]$characterFeatures$stratum)
    }
  }
  
  # Make dataframe of strata
  stratum0_fac <- factor(stratum0, levels = c("1loop", "2loop", sort(as.numeric(unique(stratum0[!(stratum0 %in% c("1loop", "2loop"))])))))
  stratum_df <- data.frame(doc0, letter0, stratum0_fac)
  stratum_table <- stratum_df %>%
    dplyr::group_by(stratum0_fac) %>%
    dplyr::summarize(n = dplyr::n())
  
  return(stratum_table)
}


#' Delete Crazy Graphs
#'
#' `delete_crazy_graphs()` removes graphs with more than max_edges from the
#' template_proc_list output by `make_proc_list()`
#'
#' @param template_proc_list List of graphs output by make_proc_list()
#' @param max_edges Maximum number of edges to allow in each graph
#' @param main_dir Input directory
#' @return List of graphs
#'
#' @noRd
delete_crazy_graphs <- function(template_proc_list, max_edges, main_dir) {
  tic <- Sys.time() # start timer
  
  # Make vectors of document #, letter #, and strata for each graph
  doc0 <- letter0 <- stratum0 <- c()
  for (i in 1:length(template_proc_list)) {
    for (j in 1:length(template_proc_list[[i]]$process$letterList)) {
      doc0 <- c(doc0, i)
      letter0 <- c(letter0, j)
      stratum0 <- c(stratum0, template_proc_list[[i]]$process$letterList[[j]]$characterFeatures$stratum)
    }
  }
  
  # Make dataframe of strata
  stratum0_fac <- factor(stratum0, levels = c("1loop", "2loop", sort(as.numeric(unique(stratum0[!(stratum0 %in% c("1loop", "2loop"))])))))
  stratum_df <- data.frame(doc0, letter0, stratum0_fac)
  
  # Delete graphs that have more than max edges from template_proc_list
  message("Deleting graphs with too many edges...")
  ok_edges <- sort(as.numeric(unique(stratum0[!(stratum0 %in% c("1loop", "2loop"))])))
  num_delete <- sum(ok_edges > max_edges)
  message(sprintf("%d graphs deleted...", num_delete))
  ok_edges <- ok_edges[ok_edges <= max_edges]
  ok_edges <- c("1loop", "2loop", ok_edges)
  ok_df <- stratum_df %>% dplyr::filter(stratum0 %in% ok_edges)
  for (i in 1:length(template_proc_list)) {
    # Find indices of graphs in document i with fewer than max_edges edges
    keep_graphs <- ok_df %>%
      dplyr::filter(doc0 == i)
    keep_graphs <- keep_graphs$letter
    
    # Only keep graphs in doc i with fewer than max_edges edges
    template_proc_list[[i]]$process$letterList <- template_proc_list[[i]]$process$letterList[keep_graphs]
  }
  
  message(sprintf("Deleted graphs with more than %d edges...", max_edges))
  
  return(template_proc_list)
}


#' Make Images List
#'
#' `make_images_list()` takes as input the template_proc_list output by make_proc_list().
#' It finds the graph locations (column and row numbers) relative to the
#' centroid of the graph image.
#'
#' @param template_proc_list List of graphs output by make_proc_list()
#' @param main_dir Input directory
#' @return List of graphs.
#'
#' @noRd
make_images_list <- function(template_proc_list, main_dir, writer_indices) {
  tic <- Sys.time() # start timer
  
  message("Processing the image (matrix) for each graph in template_proc_list...")
  
  # For each graph, find the locations (column and row numbers) relative to the bottom left corner of the graph image.
  template_images_list <- NULL
  for (i in 1:length(template_proc_list))
  {
    template_images_list <- c(template_images_list, lapply(template_proc_list[[i]]$process$letterList, function(x) {
      centeredImage(x)
    }))
  }
  
  # Add writer and docname to images list
  docname <- NULL
  for (i in 1:length(template_proc_list)) {
    docname <- c(docname, lapply(template_proc_list[[i]]$process$letterList, function(x) {
      template_proc_list[[i]]$docname
    }))
  }
  for (i in 1:length(template_images_list)) {
    template_images_list[[i]]$docname <- docname[[i]]
    template_images_list[[i]]$writer <- substr(docname[[i]], start = writer_indices[1], stop = writer_indices[2])
  }
  
  # For each graph, find the locations (column and row numbers) relative to centroid of the graph image.
  template_images_list <- lapply(template_images_list, function(x) {
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
  
  return(template_images_list)
}


#' Do Setup
#'
#' `do_setup()` creates the folder main_dir > starting_seed. It also creates
#' data and logs subfolders in the starting_seed folder.
#'
#' @param main_dir Input directory
#'
#' @noRd
do_setup <- function(main_dir) {
  
  # Create subfolder in main_dir if it doesn't already exist
  make_dir(file.path(main_dir, "data"))
}


#' Make Directory
#'
#' `make_dir()` creates the folder dir_path if it doesn't already exist. Note
#' that dirname(dir_path) must already exist.
#'
#' @param dir_path Path to a directory
#'
#' @noRd
make_dir <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
  }
}


#' Choose Centers
#'
#' `chooseCenters()` selects starting centers for the K-means algorithm. All
#' graphs in template_proc_list and template_images_list are sorted by strata, the number of loops
#' and edges. For each stratum, a set number of graphs is selected as a starting
#' cluster center. Previous experiments showed that stratified sampling produces
#' better cluster templates than a simple random sample. Amy Crawford found the
#' numbers hardcoded into numstrat to yield good results when K=40.
#' 
#' @param seed Integer seed for the random number generator for
#'   selecting starting cluster centers
#' @param K Integer number of clusters
#' @param template_proc_list List of graphs output by make_proc_list()
#' @param template_images_list List of graphs output by make_images_list()
#' @return List of starting cluster centers
#'
#' @noRd
chooseCenters <- function(seed, K, template_proc_list, template_images_list) {
  stratum <- stratumfac <- data <- n <- samp <- NULL
  
  set.seed(seed = seed)
  
  # Make vectors of document #, letter #, and strata for each graph
  doc <- letter <- stratum_a <- c()
  for (i in 1:length(template_proc_list)) {
    for (j in 1:length(template_proc_list[[i]]$process$letterList)) {
      doc <- c(doc, i)
      letter <- c(letter, j)
      stratum_a <- c(stratum_a, template_proc_list[[i]]$process$letterList[[j]]$characterFeatures$stratum)
    }
  }
  
  lvls <- c("1loop", "2loop", sort(as.numeric(unique(stratum_a[!(stratum_a %in% c("1loop", "2loop"))]))))
  # Select graphs as starting cluster centers by randomly selecting 5 graphs with 1 loop, 2 graphs with 2 loops,
  # 5 graphs with 1 edge, 6 graphs with 2 edges, and so on. NOTE: numstrat must sum to # of
  # clusters K.
  numstrat <- c(5, 2, 5, 6, 5, 3, 2, 2, 2, rep(1, 8))
  if (length(lvls) > length(numstrat)) {
    # Add trailing zeros to make numstrat the same length as lvls
    numstrat <- c(numstrat, rep(0, length(lvls) - length(numstrat)))
  } else if (length(lvls) < length(numstrat)) {
    # Drop trailing items in numstrat to make it the same length as lvls
    numstrat <- numstrat[1:length(lvls)]
  }
  
  samplingdf <- data.frame(doc = doc, letter = letter, stratum = stratum_a, ind = 1:length(stratum_a))
  samplingdf <- samplingdf %>%
    dplyr::mutate(stratumfac = factor(stratum, levels = lvls)) %>%
    dplyr::group_by(stratumfac) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::arrange(stratumfac) %>%
    dplyr::mutate(n = numstrat) %>%
    dplyr::mutate(samp = map2(data, n, dplyr::sample_n)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(samp)
  
  # Reformat starting cluster centers as prototype graphs
  centerstarts <- replicate(K, list())
  for (k in 1:K) {
    centerstarts[[k]] <- letterToPrototype(template_images_list[[samplingdf$ind[k]]], numPathCuts = 8)
  }
  
  return(centerstarts)
}

#' Path to RC
#'
#' Helper function for chooseCenters
#' 
#' @param pathList List of paths
#' @param dims Dimensions of image
#' @return Path in rows and columns
#'
#' @noRd
PathToRC = function(pathList, dims)
{
  return(cbind((pathList -1 ) %/% dims[1] + 1, dims[1] - (pathList - 1) %% dims[1]))
}

#' Letter Kmeans with Outliers Parallel
#'
#' `letterKmeansWithOutlier_parallel()` runs the K-means clustering algorithm on
#' template_images_list with centers as the starting cluster centers.
#'
#' @param template_images_list Either the full template_images_list or a subset of graphs in template_images_list output by make_images_list()
#' @param K Integer number of clusters
#' @param centers List of starting cluster centers output by chooseCenters()
#' @param num_path_cuts Integer number of sections to cut each graph into for
#'   shape comparison
#' @param max_iters Maximum number of iterations to allow the K-means algorithm
#'   to run
#' @param gamma Parameter for outliers
#' @param num_outliers Fixed value round(.25*length(template_images_list))
#' @param num_dist_cores Integer number of cores to use for the distance
#'   calculations in the K-means algorithm. Each iteration of the K-means
#'   algorithm calculates the distance between each input graph and each cluster
#'   center.
#' @param centers_seed Integer seed for the random number generator when
#'   selecting starting cluster centers.
#' @return Cluster template
#'
#' @noRd
letterKmeansWithOutlier_parallel <- function(template_proc_list, template_images_list, K, centers, num_path_cuts, max_iters, gamma,
                                             num_outliers, num_dist_cores, centers_seed) {
  get("within_cluster_sum_of_squares")
  
  # Initialize ----
  n <- length(template_images_list)
  oldCluster <- rep(0, n) # previous cluster assignment for each graph
  cluster <- rep(NA, n) # current cluster assignment for each graph
  iters <- 0
  dists <- matrix(NA, ncol = K, nrow = n) # distance between each graph and each cluster center
  wcd <- matrix(NA, nrow = max_iters, ncol = n) # within-cluster distances
  centerMoved <- rep(TRUE, K) # whether each cluster lost or grained graphs during current iteration
  centerMovedOld <- rep(TRUE, K) # whether each cluster lost or grained graphs on last iteration
  oldCenters <- centers
  changes <- c() # number of graphs that changed clusters on each iteration
  wcss <- c() # Within-cluster sum of squares
  
  # Initial settings for outliers
  current_outlierCutoff <- Inf
  outlierCutoff <- c() # save outlier cutoff on each iteration
  outliers <- NULL
  potentialOutliers <- NULL
  
  ## ADDING PARALLEL
  doParallel::registerDoParallel(num_dist_cores)
  vecoflengthj <- rep(0, K)
  
  while (TRUE) {
    # Cluster Assignment Step ----
    iters <- iters + 1
    message(sprintf("\nStarting iteration %d of the k-means algorithm...", iters))
    
    message("Calculating the distances between graphs and cluster centers...")
    # Calculate the distance between each graph and each cluster center. If the cluster center didn't change, the distances for that
    # cluster don't need to be recalculated
    listoflengthi <- foreach::foreach(i = 1:length(template_images_list), .export = c("getGraphDistance"), .packages = c("lpSolve")) %dopar% { # for each graph i
      for (j in 1:K) # for each cluster j
      {
        if (centerMoved[j] | centerMovedOld[j]) # if cluster j's center changed
        { # calculate the distance between graph i and cluster j
          vecoflengthj[j] <- getGraphDistance(template_images_list[[i]], centers[[j]], isProto2 = TRUE, numPathCuts = num_path_cuts)$matching_weight
        } else {
          vecoflengthj[j] <- dists[i, j]
        } # distance didn't change from last iteration
      }
      return(vecoflengthj)
    }
    
    message("Assigning graphs to clusters...")
    # put distances between graphs and cluster centers in a matrix
    dists <- matrix(unlist(listoflengthi), nrow = length(template_images_list), ncol = K, byrow = TRUE)
    
    # assign each graph to the closet cluster
    cluster <- apply(dists, 1, which.min)
    
    # get the within-cluster distances
    current_wcd <- apply(dists, 1, min)
    
    # find graphs that are further from their assigned cluster than current_outlierCutoff. NOTE: no outliers are assigned on the first iteration
    potentialOutliers <- which(current_wcd > current_outlierCutoff)
    
    # Sort the potential outliers by distance from their respective cluster centers. If there are more potential outliers than num_outliers
    # only keep the outliers furthest from their clusters
    outliers <- potentialOutliers[rank(-1 * current_wcd[current_wcd > current_outlierCutoff]) < num_outliers]
    
    # assign outliers to the outlier cluster
    cluster[outliers] <- -1
    
    # If a cluster is empty or only has one graph, reassign that graph (if any) to the outlier cluster.
    # Then find the non-outlier graph that is furthest from its cluster center and reassign it to the
    # now empty cluster
    for (i in 1:K) {
      if (sum(cluster == i) < 2) {
        cluster[which(cluster == i)] <- -1
        cluster[cluster != -1][which.max(current_wcd[cluster != -1])] <- i
      }
    }
    
    # Add current outlier cutoff to list
    outlierCutoff <- c(outlierCutoff, current_outlierCutoff)
    
    # Calculate the new outlierCutoff as
    # gamma*(current_wcd of non-outlier clusters)/(total # graphs - # outliers)
    current_outlierCutoff <- gamma * sum(current_wcd[cluster > 0]) / (n - sum(cluster < 0))
    
    # Recalculate the within cluster distances
    for (i in 1:n) {
      if (cluster[i] == -1) {
        current_wcd[i] <- NA
      } else {
        current_wcd[i] <- dists[i, cluster[i]]
      }
    }
    wcd[iters, ] <- current_wcd
    
    # Record number and percent of graphs that changed clusters
    current_changes <- sum(cluster != oldCluster)
    changes <- c(changes, current_changes)
    current_perc_changes <- 100 * current_changes / n
    message(sprintf("%d graphs changed clusters...", current_changes))
    message(sprintf("%f percent of total graphs changed clusters...", current_perc_changes))
    
    # Performance Measures ----
    # Caclulate the Within-Cluster Sum of Squares
    current_wcss <- within_cluster_sum_of_squares(wcd = current_wcd, cluster = cluster)
    wcss <- c(wcss, current_wcss)
    message(sprintf("The within-cluster sum of squares is %f...", current_wcss))
    
    # Check Stopping Criteria ----
    # Stop if the percent of graphs that changed clusters is <= 3%, if the
    # number of graphs that changed clusters has been constant for 3 consecutive
    # iterations, or if the max number of iterations has been reached
    if (current_perc_changes <= 3) {
      message("Percent of graphs that changed clusters is 3% or less. Stopping K-means algorithm...")
      stop_reason <- "3 percent"
      break
    }
    if ((length(utils::tail(changes, n = 3)) == 3) & (length(unique(utils::tail(changes, n = 3))) == 1)) {
      message("The same number of graphs have changed clusters on the last three iterations. Stopping K-means algorithm...")
      stop_reason <- "flatline"
      break
    }
    if (iters >= max_iters) {
      message("The maximum number of iterations has been reached. Stopping K-means algorithm...")
      stop_reason <- "max iterations"
      break
    }
    
    # Update Centers Step -------------------------------------------------------------
    message("Calculating new cluster centers...")
    # Record whether each graph changed clusters
    whichChanged <- !(oldCluster == cluster)
    
    # For each cluster, record whether any of its graphs left or joined the cluster
    centerMovedOld <- centerMoved
    centerMoved <- rep(FALSE, K)
    for (i in 1:K)
    {
      if (any(whichChanged[cluster == i])) {
        centerMoved[i] <- TRUE
      }
    }
    
    # For each cluster, if the cluster changed graphs, calculate the new cluster center
    for (i in 1:K)
    {
      if (centerMoved[i]) { # call the mean graph function with the graphs in cluster i sorted by distance, from closest to furthest) to cluster i's center
        centers[[i]] <- meanGraphSet_slowchange(template_images_list[cluster == i][order(current_wcd[cluster == i])], num_path_cuts = num_path_cuts,num_dist_cores=num_dist_cores) # meanGraphSet_Kmeans(template_images_list[cluster == i], num_path_cuts=num_path_cuts)
      }
    }
    
    # store current cluster assignments
    oldCluster <- cluster
  }
  # list docs and writers
  docnames <- sapply(template_images_list, function(x) x$docname)
  writers <- sapply(template_images_list, function(x) x$writer)
  
  return(list(
    centers_seed = centers_seed,
    template_graphs = template_images_list,
    cluster = cluster, 
    centers = centers, 
    K = K, 
    n = n, 
    docnames = docnames, 
    writers = writers, 
    iters = iters, 
    changes = changes, 
    outlierCutoff = outlierCutoff,
    stop_reason = stop_reason, 
    wcd = wcd, 
    wcss = wcss
  ))
}


#' Mean Graph Set Slow Change
#'
#' `meanGraphSet_slowchange()` calculates the mean graph of the graphs in the input template_images_list and returns the
#' graph in template_images_list closest to the mean graph
#'
#' @param template_images_list List of graphs contained in a specific cluster
#' @param num_path_cuts Integer number of sections to cut each graph into for
#'   shape comparison
#' @return The input graph closest to the mean graph
#'
#' @noRd
meanGraphSet_slowchange <- function(template_images_list, num_path_cuts, num_dist_cores) {
  # indices=sample.int(length(template_images_list)) # adds graphs to mean calculations in a random order
  indices <- 1:length(template_images_list) # adds graphs in order
  if (length(template_images_list) < 1) stop("Please specify more than 0 graphs to mean.")
  
  # initialize
  dists <- rep(0, length(template_images_list))
  
  # convert the first graph in the list to a prototype
  meanGraph1 <- letterToPrototype(template_images_list[[indices[1]]], numPathCuts = num_path_cuts)
  
  # add graphs one-by-one to the mean graph calculation
  if (length(template_images_list) > 1) {
    for (i in 2:length(template_images_list))
    {
      meanGraph1 <- weightedMeanGraphs(template_images_list[[indices[i]]], meanGraph1, 1 / i, isProto2 = TRUE, numPathCuts = num_path_cuts)
    }
  }
  
  # calculate the distance between the new mean graph and each graph in the cluster
  # for (i in 1:length(template_images_list))
  # {
  #   dists[i] <- getGraphDistance(meanGraph1, template_images_list[[i]], isProto1 = TRUE, numPathCuts = num_path_cuts)$matching_weight
  # }
  doParallel::registerDoParallel(num_dist_cores)
  dists <- foreach::foreach(i = 1:length(template_images_list), .export = c("getGraphDistance"), .packages = c("lpSolve"),.combine='c') %dopar% {
    return(getGraphDistance(meanGraph1, template_images_list[[i]], isProto1 = TRUE, numPathCuts = num_path_cuts)$matching_weight)
  }
  
  # find the index of the graph that is closest to the mean graph
  if (dists[1] >= stats::quantile(dists, 0)) {
    retindex <- which.min(dists)
  } else {
    retindex <- 1
  }
  
  # return the graph closest to the mean graph as a protoype
  return(letterToPrototype(template_images_list[[retindex]], numPathCuts = num_path_cuts))
}


#' Overall Mean Graph
#'
#' `overall_meanGraph()` calculates the overall mean graph of the cluster
#' centers and returns the cluster center that is closest to the overall mean
#' graph. Calculating the true overall mean graph from every graph used to
#' create the clustering template is too computationally expensive, so we use
#' this as an estimation of the true overall mean.
#'
#' @param centers List of cluster centers
#' @param num_path_cuts Integer number of sections to cut each graph into for
#'   shape comparison
#' @return The cluster center closest to the overall mean graph
#'
#' @noRd
overall_meanGraph <- function(centers, num_path_cuts) {
  indices <- 1:length(centers) # adds graphs in order
  if (length(centers) < 1) stop("Please specify more than 0 graphs to mean.")
  
  # initialize
  dists <- rep(0, length(centers))
  meanGraph1 <- centers[[1]]
  
  # add centers one-by-one to the mean graph calculation
  if (length(centers) > 1) {
    for (i in 2:length(centers)) {
      meanGraph1 <- weightedMeanGraphs(centers[[indices[i]]], meanGraph1, 1 / i, isProto1 = TRUE, isProto2 = TRUE, numPathCuts = num_path_cuts)
    }
  }
  
  # calculate the distance between the new mean graph and each center
  for (i in 1:length(centers))
  {
    dists[i] <- getGraphDistance(meanGraph1, centers[[i]], isProto1 = TRUE, isProto2 = TRUE, numPathCuts = num_path_cuts)$matching_weight
  }
  
  # find the index of the graph that is closest to the mean graph
  if (dists[1] >= stats::quantile(dists, 0)) {
    retindex <- which.min(dists)
  } else {
    retindex <- 1
  }
  
  return(list("overall_center_dists" = dists, overall_center = centers[[retindex]]))
}

#' Within Cluster Sum of Squares
#'
#' `within_cluster_sum_of_squares()` calculates the the within-cluster sum of squares for the
#' current iteration of the K-means algorithm
#'
#' @param wcd Matrix of within-cluster distances: the distances between each
#'   graph and each cluster center
#' @param cluster Vector of the cluster assignment for each graph
#' @return The within-cluster sum of squares
#'
#' @noRd
within_cluster_sum_of_squares <- function(wcd, cluster) {
  # Get within cluster distances of non-outlier clusters
  wcd <- wcd[cluster != -1]
  
  # Calculate within-cluster sum of squares
  wcss <- sum(wcd^2)
  
  return(wcss)
}

#' Root Mean Square Error
#'
#' `root_mean_square_error()` calculates the the root mean square error for the
#' current iteration of the K-means algorithm
#'
#' @param wcd Matrix of within-cluster distances: the distances between each
#'   graph and each cluster center
#' @param cluster Vector of the cluster assignment for each graph
#' @return The root mean square error
#'
#' @noRd
root_mean_square_error <- function(wcd, cluster) {
  # Get within cluster distances of non-outlier clusters
  wcd <- wcd[cluster != -1]
  
  # Get number of non-outlier graphs
  n <- length(wcd)
  
  # Calculate within-cluster sum of squares
  wcss <- sum(wcd^2)
  
  # Calculate the root means square error
  rmse <- sqrt(wcss / n)
  
  return(rmse)
}

#' loop_extract
#'
#' Iterates through all available paths from processHandwriting()
#' Picks out loops for later character association.
#' 
#' @param allPaths All character (formerly letter) paths from processHandwriting()
#' 
#' @return List of loops
#' 
#' @noRd
loop_extract <- function(allPaths){
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

#' centeredImage
#'
#' Find the letter's centroid and proptroid relative to the bottom left corner of the
#' letter's image.
#'
#' @param letter A letter
#' @return a named list with fields nodes, centroid, proptroid, image and allPaths
#'
#' @noRd
centeredImage = function(letter)
{
  res = list()
  res$nodes = letter$nodes
  # Find the location (column and row numbers) of the centroid relative
  # to the bottom left of the lettter image. Like (x,y) coordinates with 
  # (0,0) in the bottom left corner of the image.
  res$centroid = round(c(letter$characterFeatures$centroid_x, letter$characterFeatures$height - letter$characterFeatures$centroid_y + 1))
  # Calculate the proptroid
  res$proptroid = c(letter$characterFeatures$centroid_x, letter$characterFeatures$height - letter$characterFeatures$centroid_y + 1)/c(letter$characterFeatures$width, letter$characterFeatures$height)
  res$image = letter$image
  res$allPaths = letter$allPaths
  return(res)
}

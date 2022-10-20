### Cluster Template Creation Functions


#' make_clustering_templates
#'
#' `make_clustering_templates()` applies a K-means clustering algorithm to the
#' input handwriting samples pre-processed with
#' [process_batch_dir()] and saved in the input folder `template_dir >
#' data > template_graphs`. The K-means algorithm sorts the graphs in the
#' input handwriting samples into groups, or *clusters*, of similar graphs.
#'
#' @param template_dir Input directory
#' @param writer_indices A vector of the starting and ending location of the writer ID in the file name.
#' @param max_edges Maximum number of edges allowed in input graphs. Graphs with
#'   more than the maximum number will be ignored.
#' @param starting_seed Integer seed for the random number generator. If
#'   `num_runs` is 1 then a single cluster template is created with the starting
#'   seed. If `num_runs` is greater than 1, multiple cluster templates will be
#'   created by adding 1, 2, 3, and so on to the starting seed.
#' @param K Integer number of clusters
#' @param num_runs Integer number of cluster templates to create
#' @param num_cores Integer number of cores. If `num_runs` is greater than 1,
#'   cluster templates will be created on different cores.
#' @param num_dist_cores Integer number of cores to use for the distance
#'   calculations in the K-means algorithm. Each iteration of the K-means
#'   algorithm calculates the distance between each input graph and each cluster
#'   center.
#' @param num_path_cuts Integer number of sections to cut each graph into for
#'   shape comparison
#' @param max_iters Maximum number of iterations to allow the K-means algorithm
#'   to run
#' @param gamma Parameter for outliers
#' @param num_graphs Number of graphs to use to create the cluster template.
#'   `All` uses all available graphs. An integer uses a random sample of graphs.
#' @return List containing the cluster template(s). The list of all templates is
#'   saved in template_dir > starting_seed > data. Each individual template is
#'   saved in template_dir > starting_seed > run seed > data.
#'
#' @examples
#' \dontrun{
#' main_dir <- "path/to/folder"
#' template_images_dir <- system.file("extdata/example_images/template_training_images", 
#'                                    package = "handwriter")
#' process_batch_dir(image_batch = template_images_dir,
#                    batch_output_dir = file.path(main_dir, "data", "template_graphs"),
#                    transform_output = 'document')
#' template_list <- make_clustering_templates(template_dir = main_dir,
#'                                            writer_indices = c(2,5), 
#'                                            K = 10, 
#'                                            num_dist_cores = 2, 
#'                                            max_iters = 3,
#'                                            num_graphs = 1000, 
#'                                            starting_seed = 100, 
#'                                            num_runs = 1)
#' }  
#'
#' @keywords clustering templates
#'
#' @export
#' @md
make_clustering_templates <- function(template_dir,
                                      writer_indices,
                                      max_edges = 30,
                                      starting_seed = 100,
                                      K = 40,
                                      num_runs = 1,
                                      num_cores = 1,
                                      num_dist_cores = 1,
                                      num_path_cuts = 8,
                                      max_iters = 1,
                                      gamma = 3,
                                      num_graphs = "All") { # use integer for testing with a subset of graphs or use 'All'

  options(scipen = 999)

  # Setup folders ----
  do_setup(template_dir = template_dir, starting_seed = starting_seed)
  seed_folder <- file.path(template_dir, paste0("template_seed", starting_seed))

  # Make proclist ----
  template_proc_list <- make_proc_list(template_dir = template_dir)

  # Delete large graphs ----
  # Make table of number of graphs with various numbers of loops and edges
  strata <- get_strata(template_proc_list = template_proc_list, template_dir = template_dir)

  # Delete graphs with too many edges
  template_proc_list <- delete_crazy_graphs(template_proc_list = template_proc_list, max_edges = max_edges, template_dir = template_dir)

  # Make images list ----
  full_template_images_list <- make_images_list(template_proc_list = template_proc_list, 
                                                template_dir = template_dir,
                                                writer_indices = writer_indices)

  # Set outliers parameter
  num_outliers <- round(.25 * length(full_template_images_list))

  # Log parameters ----
  futile.logger::flog.info(
    "Starting the k-means clustering algorithm with... \n 
    num_runs=%d \n 
    num_cores=%d \n 
    num_dist_cores=%d \n 
    K=%d \n 
    num_path_cuts=%d \n 
    max_iters=%d \n 
    gamma=%f \n (max) 
    num_outliers=%d \n 
    starting_seed=%d",
    num_runs,
    num_cores,
    num_dist_cores,
    K,
    num_path_cuts,
    max_iters,
    gamma,
    num_outliers,
    starting_seed
  )

  # Choose how many graphs to use to create the template(s)
  if (num_graphs == "All") {
    template_images_list <- full_template_images_list
  } else {
    num_graphs <- as.integer(num_graphs)  # needed for shiny app 
    stratified_sample <- function(full_template_images_list, num_graphs){
      # randomly select (num_graphs / (# docs per writer * # writers) graphs from each document
      df <- data.frame(writer = sapply(full_template_images_list, function(x) x$writer),
                       docname = sapply(full_template_images_list, function(x) x$docname),
                       graph_num = 1:length(full_template_images_list))
      D <- length(unique(df$docname))
      df <- df %>% dplyr::group_by(docname) %>% dplyr::slice_sample(n = floor(num_graphs/D))
      template_images_list <- full_template_images_list[df$graph_num]
      return(template_images_list)
    }
    template_images_list <- stratified_sample(full_template_images_list, num_graphs)
  }

  doParallel::registerDoParallel(num_cores)
  templates <- foreach::foreach(
    i = 1:num_runs,
    .export = c(
      "make_dir", "chooseCenters", "runAndSaveKmeans",
      "letterKmeansWithOutlier_parallel", "within_cluster_sum_of_squares", "root_mean_square_error", "davies_bouldin", "variance_ratio_criterion", "overall_meanGraph"
    ),
    .packages = c("futile.logger", "tidyr", "dplyr", "purrr", "handwriter", "parallel", "doParallel", "lpSolve")
  ) %dopar% {
    .GlobalEnv$letterKmeansWithOutlier_parallel <- letterKmeansWithOutlier_parallel
    .GlobalEnv$within_cluster_sum_of_squares <- within_cluster_sum_of_squares
    .GlobalEnv$root_mean_square_error <- root_mean_square_error
    .GlobalEnv$davies_bouldin <- davies_bouldin
    .GlobalEnv$variance_ratio_criterion <- variance_ratio_criterion
    .GlobalEnv$overall_meanGraph <- overall_meanGraph

    # Add i to the starting seed
    run_seed <- starting_seed + i - 1

    # Create folder for run and subfolders for data and logs
    template_name <- paste0("seed", run_seed, "_run", i)
    run_folder <- file.path(seed_folder, template_name)
    make_dir(dir_path = run_folder)
    make_dir(dir_path = file.path(run_folder, "data"))
    make_dir(dir_path = file.path(run_folder, "logs"))

    # Start new log file for run i
    futile.logger::flog.appender(futile.logger::appender.file(file.path(run_folder, "logs", paste0(template_name, ".txt"))), name = paste0("run", i))
    futile.logger::flog.info("Start creating template %d.", i, name = paste0("run", i))

    # Choose cluster centers. NOTE: Even if you are testing the code on a small number of
    # graphs, you need to select centers from the full list of graphs.
    futile.logger::flog.info("Choosing starting cluster centers.", name = paste0("run", i))
    centers <- chooseCenters(run_seed = run_seed, K = K, template_proc_list = template_proc_list, template_images_list = full_template_images_list)

    # Run Kmeans
    template <- letterKmeansWithOutlier_parallel(
      template_proc_list = template_proc_list,
      run_number = i,
      template_images_list = template_images_list,
      K = K,
      num_path_cuts = num_path_cuts,
      max_iters = max_iters,
      gamma = gamma,
      num_outliers = num_outliers,
      centers = centers,
      num_dist_cores = num_dist_cores
    )
    futile.logger::flog.info("Saving template %d .", i, name = paste0("run", i))
    saveRDS(template, file = file.path(run_folder, "data", paste0(template_name, ".rds")))
    return(template)
  }

  # Save template(s) in a single file on the server
  futile.logger::flog.info("Savinging template(s)...")
  saveRDS(templates, file.path(seed_folder, "data", "all_templates.rds"))

  return(templates)
}


#' make_proc_list
#'
#' process_batch_dir() needs to be run first to processes handwriting documents
#' in a specified folder. process_batch_dir() creates an RDS file for each
#' document that contains the extracted graphs in the document and saves the
#' file in template_dir > data > template_graphs in rds files. make_proc_list()
#' loads the graph RDS files from template_dir > data > template_graphs into a
#' single list. This function also adds the graph image matrix and the number of
#' loops and edges to each item in the list. This function also records the
#' locations information of each graph with respect to the individual graph
#' instead of the handwriting document.
#'
#' @param template_dir Input directory
#' @return List containing graphs prepared for template creation. template_proc_list.rds
#'   is saved in template_dir > data.
#'
#' @noRd
make_proc_list <- function(template_dir) {
  tic <- Sys.time() # start timer

  # List files in template directory > data > template_graphs
  df <- data.frame(graph_paths = list.files(file.path(template_dir, "data", "template_graphs"), pattern = ".rds", full.names = TRUE), stringsAsFactors = FALSE)

  template_proc_list <- df$graph_paths %>%
    purrr::map(readRDS) %>%
    purrr::list_merge()

  # Get the image plot (binarized matrix) of each graph from each handwriting sample
  template_proc_list <- lapply(template_proc_list, function(x) {
    x$process$letterList <- AddLetterImages(x$process$letterList, dim(x$image))
    return(x)
  })

  # Get the number of loops and edges in each graph
  template_proc_list <- lapply(template_proc_list, function(x) {
    x$process$letterList <- AddSamplingStrata(x$process$letterList)
    return(x)
  })

  # For each graph in each handwriting sample, get the locations of the nodes,
  # paths, centroids, and other items with respect to the individual graph
  # instead of the handwriting sample. WARNING: This messes up plotting.
  template_proc_list <- lapply(template_proc_list, function(x) {
    x$process$letterList <- MakeLetterListLetterSpecific(x$process$letterList, dim(x$image))
    return(x)
  }) # THIS MESSES UP PLOTTING!!

  # Save to template directory
  saveRDS(template_proc_list, file.path(template_dir, "data", "template_proc_list.rds"))

  # Save time elapsed to metadata file
  toc <- Sys.time()
  elapsed <- paste0(round(as.numeric(difftime(time1 = toc, time2 = tic, units = "min")), 3), " minutes")
  message("Processing graphs from training documents to make them ready for template creation: %s", elapsed)

  return(template_proc_list)
}


#' get_strata
#'
#' get_strata() creates a table that shows the number of graphs in template_proc_list for
#' each strata (number of loops or edges)
#'
#' @param template_proc_list List of graphs output by make_proc_list()
#' @param template_dir Input directory
#' @return Dataframe of number of graphs per strata. The dataframe is saved in
#'   tempalte_dir > data.
#'
#' @noRd
get_strata <- function(template_proc_list, template_dir) {
  tic <- Sys.time() # start timer

  metadata_file <- file.path(template_dir, "logs", "metadata.txt")
  futile.logger::flog.appender(futile.logger::appender.file(metadata_file), name = "metadata")
  futile.logger::flog.info("Starting making a dataframe of the number of graphs with various numbers of loops and edges...", name = "metadata")

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

  # Save strata to csv file
  futile.logger::flog.info("Saving dataframe to template_dir > data > template_strata.rds.", name = "metadata")
  saveRDS(stratum_table, file.path(template_dir, "data", "template_strata.rds"))

  # Calculate processing time
  toc <- Sys.time()
  elapsed <- paste0(round(as.numeric(difftime(time1 = toc, time2 = tic, units = "min")), 3), " minutes")
  metadata_file <- file.path(template_dir, "logs", "metadata.txt")
  futile.logger::flog.appender(futile.logger::appender.file(metadata_file), name = "metadata")
  futile.logger::flog.info("Creating and saving strata dataframe: %s", elapsed, name = "metadata")
  futile.logger::flog.info("Strata dataframe saved to template_dir > data > template_strata.rds.", name = "metadata")

  return(stratum_table)
}


#' delete_crazy_graphs
#'
#' delete_crazy_graphs() removes graphs with more than max_edges from the
#' template_proc_list output by make_proc_list()
#'
#' @param template_proc_list List of graphs output by make_proc_list()
#' @param max_edges Maximum number of edges to allow in each graph
#' @param template_dir Input directory
#' @return List of graphs
#'
#' @noRd
delete_crazy_graphs <- function(template_proc_list, max_edges, template_dir) {
  tic <- Sys.time() # start timer

  metadata_file <- file.path(template_dir, "logs", "metadata.txt")
  futile.logger::flog.appender(futile.logger::appender.file(metadata_file), name = "metadata")
  futile.logger::flog.info("Making dataframe of strata...", name = "metadata")
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
  futile.logger::flog.info("Deleting graphs with more than the max number of edges...", name = "metadata")
  ok_edges <- sort(as.numeric(unique(stratum0[!(stratum0 %in% c("1loop", "2loop"))])))
  num_delete <- sum(ok_edges > max_edges)
  futile.logger::flog.info("%d graphs deleted...", num_delete, name = "metadata")
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

  # Save to template directory
  futile.logger::flog.info("Saving updated graph list to template_dir > data > template_proc_list.rds", name = "metadata")
  saveRDS(template_proc_list, file.path(template_dir, "data", "template_proc_list.rds"))

  # Calculate processing time
  toc <- Sys.time() # stop timer
  elapsed <- paste0(round(as.numeric(difftime(time1 = toc, time2 = tic, units = "min")), 3), " minutes")
  futile.logger::flog.info("Deleted graphs with more than %d edges: %s", max_edges, elapsed, name = "metadata")

  return(template_proc_list)
}


#' make_images_list
#'
#' make_images_list() takes as input the template_proc_list output by make_proc_list().
#' It finds the graph locations (column and row numbers) relative to the
#' centroid of the graph image.
#'
#' @param template_proc_list List of graphs output by make_proc_list()
#' @param template_dir Input directory
#' @return List of graphs. The list is saved as template_dir > data > template_images_list.rds
#'
#' @noRd
make_images_list <- function(template_proc_list, template_dir, writer_indices) {
  tic <- Sys.time() # start timer

  metadata_file <- file.path(template_dir, "logs", "metadata.txt")
  futile.logger::flog.appender(futile.logger::appender.file(metadata_file), name = "metadata")
  futile.logger::flog.info("Processing the image (matrix) for each graph in template_proc_list...", name = "metadata")

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
  for (i in 1:length(template_proc_list)){
   docname <- c(docname, lapply(template_proc_list[[i]]$process$letterList, function(x) {
      template_proc_list[[i]]$docname
    }))
  }
  for (i in 1:length(template_images_list)){
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

  futile.logger::flog.info("Saving list of images to template_dir > data > template_images_list.rds", name = "metadata")
  saveRDS(template_images_list, file.path(template_dir, "data", "template_images_list.rds"))

  # Calculate processing time
  toc <- Sys.time() # stop timer
  elapsed <- paste0(round(as.numeric(difftime(time1 = toc, time2 = tic, units = "min")), 3), " minutes")
  futile.logger::flog.info("Saved images list to template_dir > data > template_images_list.rds: %s", elapsed, name = "metadata")

  return(template_images_list)
}


#' do_setup
#'
#' `do_setup()` creates the folder template_dir > starting_seed. It also creates
#' data and logs subfolders in the starting_seed folder.
#'
#' @param template_dir Input directory
#' @param starting_seed Integer seed for the random number generator.
#'
#' @noRd
do_setup <- function(template_dir, starting_seed) {

  # Create subfolder in template_dir if it doesn't already exist
  make_dir(file.path(template_dir, "data"))
  make_dir(file.path(template_dir, "logs"))
  seed_folder <- file.path(template_dir, paste0("template_seed", starting_seed))
  make_dir(dir_path = seed_folder)
  make_dir(dir_path = file.path(seed_folder, "logs"))
  make_dir(dir_path = file.path(seed_folder, "data"))

  # Start log file
  futile.logger::flog.appender(futile.logger::appender.file(file.path(seed_folder, "logs", paste0("seed", starting_seed, ".txt"))))
  futile.logger::flog.info("Start creating new clustering template(s).")
}


#' make_dir
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


#' chooseCenters
#'
#' `chooseCenters()` selects starting centers for the K-means algorithm. All
#' graphs in template_proc_list and template_images_list are sorted by strata, the number of loops
#' and edges. For each stratum, a set number of graphs is selected as a starting
#' cluster center. Previous experiments showed that stratified sampling produces
#' better cluster templates than a simple random sample. Amy Crawford found the
#' numbers hardcoded into numstrat to yield good results when K=40.
#'
#' @param run_number Integer number of current run
#' @param K Integer number of clusters
#' @param template_proc_list List of graphs output by make_proc_list()
#' @param template_images_list List of graphs output by make_images_list()
#' @return List of starting cluster centers
#'
#' @noRd
chooseCenters <- function(run_seed, K, template_proc_list, template_images_list) {
  set.seed(seed = run_seed)

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
  if (length(lvls) > length(numstrat)){
    # Add trailing zeros to make numstrat the same length as lvls
    numstrat <- c(numstrat, rep(0, length(lvls) - length(numstrat)))
  } else if (length(lvls) < length(numstrat)){
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


#' letterKmeansWithOutlier_parallel
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
#' @param num_outliers Fixed value round(.25*length(full_template_images_list))
#' @param num_dist_cores Integer number of cores to use for the distance
#'   calculations in the K-means algorithm. Each iteration of the K-means
#'   algorithm calculates the distance between each input graph and each cluster
#'   center.
#' @param run_number Integer number of current run
#' @return Cluster template
#'
#' @noRd
letterKmeansWithOutlier_parallel <- function(template_proc_list, template_images_list, K, centers, num_path_cuts, max_iters, gamma,
                                             num_outliers, num_dist_cores, run_number) {
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
  db <- c() # Davies-Bouldin Index
  vrc <- c() # Variance ratio criterion
  wcss <- c() # Within-cluster sum of squares
  rmse <- c() # Root mean square error

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
    futile.logger::flog.info("Starting iteration %d.", iters, name = paste0("run", run_number))

    futile.logger::flog.info("Calculating the distances between graphs and cluster centers.", name = paste0("run", run_number))
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

    futile.logger::flog.info("Assigning graphs to clusters.", name = paste0("run", run_number))
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
    futile.logger::flog.info("%d graphs changed clusters.", current_changes, name = paste0("run", run_number))
    futile.logger::flog.info("%f percent of total graphs changed clusters.", current_perc_changes, name = paste0("run", run_number))

    # Performance Measures ----
    # Caclulate the Within-Cluster Sum of Squares
    current_wcss <- within_cluster_sum_of_squares(wcd = current_wcd, cluster = cluster)
    wcss <- c(wcss, current_wcss)
    futile.logger::flog.info("The within-cluster sum of squares is %f.", current_wcss, name = paste0("run", run_number))

    # Calculate the root mean square error
    current_rmse <- root_mean_square_error(wcd = current_wcd, cluster = cluster)
    rmse <- c(rmse, current_rmse)
    futile.logger::flog.info("The root mean square error is %f.", current_rmse, name = paste0("run", run_number))

    # Calculate the Davies-Bouldin Index
    current_db <- davies_bouldin(wcd = current_wcd, cluster = cluster, centers = centers, K = K, num_path_cuts = num_path_cuts)
    db <- c(db, current_db)
    futile.logger::flog.info("The Davies-Bouldin Index is %f.", current_db, name = paste0("run", run_number))

    # Calculate the variance ratio criterion
    current_vrc <- variance_ratio_criterion(wcd = current_wcd, cluster = cluster, centers = centers, K = K, num_path_cuts = num_path_cuts)
    vrc <- c(vrc, current_vrc)
    futile.logger::flog.info("The variance ratio criterion is %f.", current_vrc, name = paste0("run", run_number))

    # Check Stopping Criteria ----
    # Stop if the percent of graphs that changed clusters is <= 3%, if the
    # number of graphs that changed clusters has been constant for 3 consecutive
    # iterations, or if the max number of iterations has been reached
    if (current_perc_changes <= 3) {
      futile.logger::flog.info("Percent of graphs that changed clusters is 3% or less. Stopping K-means algorithm.", name = paste0("run", run_number))
      stop_reason <- "3 percent"
      break
    }
    if ((length(tail(changes, n = 3)) == 3) & (length(unique(tail(changes, n = 3))) == 1)) {
      futile.logger::flog.info("The same number of graphs have changed clusters on the last three iterations. Stopping K-means algorithm.", name = paste0("run", run_number))
      stop_reason <- "flatline"
      break
    }
    if (iters >= max_iters) {
      futile.logger::flog.info("The maximum number of iterations has been reached. Stopping K-means algorithm.", name = paste0("run", run_number))
      stop_reason <- "max iterations"
      break
    }


    # Update Centers Step -------------------------------------------------------------
    futile.logger::flog.info("Calculating new cluster centers.", name = paste0("run", run_number))
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
        centers[[i]] <- meanGraphSet_slowchange(template_images_list[cluster == i][order(current_wcd[cluster == i])], num_path_cuts = num_path_cuts) # meanGraphSet_Kmeans(template_images_list[cluster == i], num_path_cuts=num_path_cuts)
      }
    }

    # store current cluster assignments
    oldCluster <- cluster
  }
  # list docs and writers
  docnames <- sapply(template_images_list, function(x) x$docname)
  writers <- sapply(template_images_list, function(x) x$writer)

  return(list(
    cluster = cluster, centers = centers, K = K, n = n, docnames = docnames, writers = writers, iters = iters, changes = changes, outlierCutoff = outlierCutoff,
    stop_reason = stop_reason, wcd = wcd, wcss = wcss, rmse = rmse, DaviesBouldinIndex = db, VarianceRatioCriterion = vrc
  ))
}


#' meanGraphSet_slowchange
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
meanGraphSet_slowchange <- function(template_images_list, num_path_cuts = 4) {
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
  for (i in 1:length(template_images_list))
  {
    dists[i] <- getGraphDistance(meanGraph1, template_images_list[[i]], isProto1 = TRUE, numPathCuts = num_path_cuts)$matching_weight
  }

  # find the index of the graph that is closest to the mean graph
  if (dists[1] >= quantile(dists, 0)) {
    retindex <- which.min(dists)
  } else {
    retindex <- 1
  }

  # return the graph closest to the mean graph as a protoype
  return(letterToPrototype(template_images_list[[retindex]], numPathCuts = num_path_cuts))
}


#' overall_meanGraph
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
overall_meanGraph <- function(centers, num_path_cuts = 8) {
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
  if (dists[1] >= quantile(dists, 0)) {
    retindex <- which.min(dists)
  } else {
    retindex <- 1
  }

  return(list("overall_center_dists" = dists, overall_center = centers[[retindex]]))
}


#' davies_bouldin
#'
#' davies_bouldin() calculates the Davies-Bouldin Index for the current
#' iteration of the K-means algorithm
#'
#' @param wcd Matrix of within-cluster distances: the distances between each
#'   graph and each cluster center
#' @param cluster Vector of the cluster assignment for each graph
#' @param centers List of cluster centers
#' @param K Integer number of clusters
#' @param num_path_cuts Integer number of sections to cut each graph into for
#'   shape comparison
#' @return The Davies-Bouldin Index
#'
#' @noRd
davies_bouldin <- function(wcd, cluster, centers, K, num_path_cuts) {
  # For each cluster, calculate the average distance between the graphs in that cluster and the cluster center
  s <- rep(0, K)
  for (i in 1:K) {
    s[i] <- mean(wcd[cluster == i])
  }

  # Calculate the distance between each pair of cluster centers. We don't need to calculate the distance between a center and itself, so skip those calculations.
  d <- matrix(NA, nrow = K, ncol = K)
  for (i in 1:K) {
    for (j in 1:K) {
      if (i != j) {
        d[i, j] <- getGraphDistance(centers[[i]], centers[[j]], isProto1 = TRUE, isProto2 = TRUE, numPathCuts = num_path_cuts)$matching_weight
      }
    }
  }

  # Calculate R_ij for all i and j. Again, we don't need to measure the separation between a cluster and itself, so skip those calculations
  R <- matrix(NA, nrow = K, ncol = K)
  for (i in 1:K) {
    for (j in 1:K) {
      if (i != j) {
        R[i, j] <- (s[i] + s[j]) / d[i, j]
      }
    }
  }

  # Make list of clusters that have at least 1 graph
  filled_clusters <- unique(cluster[cluster != -1])

  # Find the max value of R for each filled cluster i
  maxR <- rep(NA, length(filled_clusters))
  for (i in 1:length(filled_clusters)) {
    maxR[i] <- max(R[filled_clusters[i], ], na.rm = TRUE)
  }

  # Calculate the index
  db <- sum(maxR) / length(filled_clusters)

  return(db)
}


#' variance_ratio_criterion
#'
#' variance_ratio_criterion() calculates the varience-ratio criterion for the
#' current iteration of the K-means algorithm
#'
#' @param wcd Matrix of within-cluster distances: the distances between each
#'   graph and each cluster center
#' @param cluster Vector of the cluster assignment for each graph
#' @param centers List of cluster centers
#' @param K Integer number of clusters
#' @param num_path_cuts Integer number of sections to cut each graph into for
#'   shape comparison
#' @return The variance-ratio criterion
#'
#' @noRd
variance_ratio_criterion <- function(wcd, cluster, centers, K, num_path_cuts) {
  # Count the number of graphs in each non-outlier cluster
  ni <- c()
  for (i in 1:K) {
    ni <- c(ni, sum(cluster == i))
  }

  # Get total number of non-outlier graphs
  n <- sum(ni)

  # Estimate the overall mean graph (the center of the centers) and measure distance from all centers to
  # the overall center
  overall <- overall_meanGraph(centers = centers, num_path_cuts = num_path_cuts)

  # Calculate the between-cluster variance: SS_B = (k-1)^(-1) \sum_{i=1}^k n_i \cdot d(m_i, m)^2
  SSb <- sum(ni * overall$overall_center_dists^2) / (K - 1)

  # Calculate the within-cluster variance: SS_W = (n-k)^(-1)\sum_{i=1}^k \sum_{x \in C_i} d(x, m_i)^2
  wcd <- wcd[cluster != -1] # remove graphs in the outlier cluster
  SSw <- sum(wcd^2) / (n - K)

  # Calculate variance-ratio-criterion
  vrc <- SSb / SSw
  return(vrc)
}


#' within_cluster_sum_of_squares
#'
#' within_cluster_sum_of_squares() calculates the the within-cluster sum of squares for the
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


#' root_mean_square_error
#'
#' root_mean_square_error() calculates the the root mean square error for the
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

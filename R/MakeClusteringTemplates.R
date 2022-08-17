make_clustering_templates = function(template_dir,
                                    max_edges = 30, #Maximum number of edges per graph based on plot
                                    starting_seed = 100, 
                                    K = 40, 
                                    num_runs = 1, # number of templates to create
                                    num_cores = 1,
                                    num_dist_cores = 1, # for each template
                                    num_path_cuts = 8, # for distance calculations
                                    max_iters = 1, # how many iterations to let kmeans algorithm run
                                    gamma = 3, # parameter for outliers
                                    num_graphs = 'All'){ # use integer for testing with a subset of graphs or use 'All'
  
  library(handwriter)
  library(tidyverse)
  library(igraph)
  library(reshape2)
  library(gridExtra)
  library(futile.logger)
  library(parallel)
  library(doParallel)
  library(stringr)
  library(foreach)
  library(purrr)
  library(forcats)
  library(mc2d)
  library(gridExtra)
  library(ggrepel)
  library(circular)
  library(tictoc)
  library(lpSolve)
  options(scipen = 999)
  
 
  
  # Make a master rds file that contains all of the graphs from all samples in the dataframe
  # proc_list needed for next step
  proc_list = make_proc_list(template_dir)
  
  # Make table of number of graphs with various numbers of loops and edges
  strata = get_strata(proc_list, template_dir)
  
  # Delete graphs with too many edges
  proc_list = delete_crazy_graphs(proc_list, max_edges, template_dir)
  
  # Make images list, image_list need for next 
  images_list = make_images_list(proc_list, template_dir)
  
  # Make template(s)
  templates = make_templates(num_runs, num_cores, K, num_graphs, num_path_cuts, max_iters, gamma, 
                             num_dist_cores, starting_seed, template_dir)
  
  return(templates)
}


#====================================================
#============== Helper Functions ====================
#====================================================
# Make a list of all graphs (proc_list) from all selected docs and save in
# Stage6_Cluster_Templates folder
make_proc_list = function(template_dir){
  tic = Sys.time()  # start timer
  
  # List files in template directory > data > template_graphs
  df = data.frame(graph_paths =list.files(file.path(template_dir, "data", "template_graphs"), pattern=".rds", full.name=TRUE), stringsAsFactors = FALSE)
  
  proc_list = df$graph_paths %>%
    map(readRDS) %>%
    list_merge()

  # Get the image plot (binarized matrix) of each graph from each handwriting sample
  proc_list = lapply(proc_list, function(x){x$process$letterList = AddLetterImages(x$process$letterList, dim(x$image)); return(x)})
  
  # Get the number of loops and edges in each graph
  proc_list = lapply(proc_list, function(x){x$process$letterList = AddSamplingStrata(x$process$letterList); return(x)})
  
  # For each graph in each handwriting sample, get the locations of the nodes,
  # paths, centroids, and other items with respect to the individual graph
  # instead of the handwriting sample. WARNING: This messes up plotting.
  proc_list = lapply(proc_list, function(x){x$process$letterList = MakeLetterListLetterSpecific(x$process$letterList, dim(x$image)); return(x)}) #THIS MESSES UP PLOTTING!!
  
  # Save to template directory
  saveRDS(proc_list, file.path(template_dir, "data", "proc_list.rds"))
  
  # Save time elapsed to metadata file
  toc = Sys.time()
  elapsed = paste0(round(as.numeric(difftime(time1 = toc, time2 = tic, units = "min")), 3), " minutes")
  message("Processing graphs from training documents to make them ready for template creation: %s", elapsed)
  
  return(proc_list)
}

# Find the number of loops or edges (strata) in each graph in the select docs
# and save in Stage6_Cluster_Templates folder
get_strata = function(proc_list, template_dir){
  tic = Sys.time()  # start timer
  
  metadata_file = file.path(template_dir, "logs", "metadata.txt")
  flog.appender(appender.file(metadata_file), name='metadata')
  flog.info("Starting making a dataframe of the number of graphs with various numbers of loops and edges...", name="metadata")
  
  # Set Max Number of Edges Per Graph -------------------------------------------------
  # Make vectors of document #, letter #, and strata for each graph
  doc0 = letter0 = stratum0 = c()
  for(i in 1:length(proc_list)){
    for(j in 1:length(proc_list[[i]]$process$letterList)){
      doc0 = c(doc0,i)
      letter0 = c(letter0,j)
      stratum0 = c(stratum0, proc_list[[i]]$process$letterList[[j]]$characterFeatures$stratum)
    }
  }
  
  # Make dataframe of strata
  stratum0_fac = factor(stratum0, levels = c("1loop", "2loop", sort(as.numeric(unique(stratum0[!(stratum0 %in% c("1loop", "2loop"))])))))
  stratum_df = data.frame(doc0, letter0, stratum0_fac)
  stratum_table = stratum_df %>% group_by(stratum0_fac) %>% summarize(n=n())
  
  # Save strata to csv file
  flog.info("Saving dataframe to template_dir > data > sample_strata.rds.", name="metadata")
  saveRDS(stratum_table, file.path(template_dir, "data", "sample_strata.rds"))
  
  # Calculate processing time
  toc = Sys.time()
  elapsed = paste0(round(as.numeric(difftime(time1 = toc, time2 = tic, units = "min")), 3), " minutes")
  metadata_file = file.path(template_dir, "logs", "metadata.txt")
  flog.appender(appender.file(metadata_file), name='metadata')
  flog.info("Creating and saving strata dataframe: %s", elapsed, name="metadata")
  flog.info("Strata dataframe saved to template_dir > data > sample_strata.rds.", name="metadata")
  
  return(stratum_table)
  
}

# Delete graphs with too many edges from the list of all graphs (proc_list)
delete_crazy_graphs = function(proc_list, max_edges, template_dir){
  
  tic = Sys.time()  # start timer
  
  metadata_file = file.path(template_dir, "logs", "metadata.txt")
  flog.appender(appender.file(metadata_file), name='metadata')
  flog.info("Making dataframe of strata...", name="metadata")
  # Make vectors of document #, letter #, and strata for each graph
  doc0 = letter0 = stratum0 = c()
  for(i in 1:length(proc_list)){
    for(j in 1:length(proc_list[[i]]$process$letterList)){
      doc0 = c(doc0,i)
      letter0 = c(letter0,j)
      stratum0 = c(stratum0, proc_list[[i]]$process$letterList[[j]]$characterFeatures$stratum)
    }
  }
  
  # Make dataframe of strata
  stratum0_fac = factor(stratum0, levels = c("1loop", "2loop", sort(as.numeric(unique(stratum0[!(stratum0 %in% c("1loop", "2loop"))])))))
  stratum_df = data.frame(doc0, letter0, stratum0_fac)
  
  # Delete graphs that have more than max edges from proc_list
  flog.info("Deleting graphs with more than the max number of edges...", name="metadata")
  ok_edges = sort(as.numeric(unique(stratum0[!(stratum0 %in% c("1loop", "2loop"))])))
  num_delete = sum(ok_edges > max_edges)
  flog.info("%d graphs deleted...", num_delete, name="metadata")
  ok_edges = ok_edges[ok_edges <= max_edges]
  ok_edges = c('1loop', '2loop', ok_edges)
  ok_df = stratum_df %>% filter(stratum0 %in% ok_edges)
  for (i in 1:length(proc_list)){
    # Find indices of graphs in document i with fewer than max_edges edges 
    keep_graphs = ok_df %>% 
      filter(doc0==i)
    keep_graphs = keep_graphs$letter
    
    # Only keep graphs in doc i with fewer than max_edges edges
    proc_list[[i]]$process$letterList = proc_list[[i]]$process$letterList[keep_graphs]
  }
  
  # Save to template directory
  flog.info("Saving updated graph list to template_dir > data > proc_list.rds", name="metadata")
  saveRDS(proc_list, file.path(template_dir, "data", "proc_list.rds"))
  
  # Calculate processing time
  toc = Sys.time()  # stop timer
  elapsed = paste0(round(as.numeric(difftime(time1 = toc, time2 = tic, units = "min")), 3), " minutes")
  flog.info("Deleted graphs with more than %d edges: %s", max_edges, elapsed, name="metadata")
  
  return(proc_list)
}

#make_images_list
make_images_list = function(proc_list, template_dir){
  
  tic = Sys.time()  # start timer
  
  metadata_file = file.path(template_dir, "logs", "metadata.txt")
  flog.appender(appender.file(metadata_file), name='metadata')
  flog.info("Processing the image (matrix) for each graph in proc_list...", name="metadata")
  
  # For each graph, find the locations (column and row numbers) relative to the bottom left corner of the graph image.
  images_list = NULL
  for(i in 1:length(proc_list))
  {
    images_list = c(images_list, lapply(proc_list[[i]]$process$letterList, function(x){centeredImage(x)}))
  }
  
  # For each graph, find the locations (column and row numbers) relative to centroid of the graph image.
  images_list = lapply(images_list, function(x){
    x$nodesrc = cbind(((x$nodes-1) %/% dim(x$image)[1]) + 1, dim(x$image)[1] - ((x$nodes-1) %% dim(x$image)[1]))
    x$nodesrc = x$nodesrc - matrix(rep(x$centroid, each = dim(x$nodesrc)[1]), ncol = 2)
    x$pathEndsrc = lapply(x$allPaths, function(z){cbind(((z[c(1,length(z))]-1) %/% dim(x$image)[1]) + 1, dim(x$image)[1] - ((z[c(1,length(z))]-1) %% dim(x$image)[1]))})
    x$pathEndsrc = lapply(x$pathEndsrc, function(z){z - matrix(rep(x$centroid, each = 2), ncol = 2)})
    return(x)
  })
  
  flog.info("Saving list of images to template_dir > data > images_list.rds", name="metadata")
  saveRDS(images_list, file.path(template_dir, "data", "images_list.rds"))
  
  # Calculate processing time
  toc = Sys.time()  # stop timer
  elapsed = paste0(round(as.numeric(difftime(time1 = toc, time2 = tic, units = "min")), 3), " minutes")
  flog.info("Saved images list to template_dir > data > images_list.rds: %s", elapsed, name="metadata")
  
  return(images_list)
  
}

#make_templates
make_templates = function(num_runs, num_cores, K, num_graphs, num_path_cuts, max_iters, gamma, num_dist_cores, starting_seed, template_dir){
  
  # Setup ----
  data = do_setup(template_dir = template_dir, starting_seed = starting_seed)
  proc_list = data$proc_list
  full_images_list = data$images_list
  seed_folder = data$seed_folder
  rm(data)
  
  # Set outliers parameter
  num_outliers=round(.25*length(full_images_list))
  
  # Log parameters ---- 
  flog.info("Starting the k-means clustering algorithm with... \n num_runs=%d \n num_cores=%d \n num_dist_cores=%d \n K=%d \n num_path_cuts=%d \n max_iters=%d \n gamma=%f \n (max) num_outliers=%d \n starting_seed=%d", 
            num_runs, num_cores, num_dist_cores, K, num_path_cuts, max_iters, gamma, num_outliers, starting_seed)
  
  # Run main function ----
  handwriterTemplates=runLetterKmeansParallel(num_runs, num_cores, full_images_list, proc_list, num_graphs, K, num_path_cuts, 
                                              max_iters, gamma, num_outliers, num_dist_cores, starting_seed, template_dir, seed_folder)
  
  # Save template(s) in a single file on the server
  flog.info("Savinging template(s)...")
  saveRDS(handwriterTemplates, file.path(seed_folder, "data", "all_templates.rds"))
  
  return(handwriterTemplates)
  
}

#do_setup
do_setup = function(template_dir, starting_seed){
  
  # Create subfolder in template_dir if it doesn't already exist
  seed_folder=file.path(template_dir, paste0("template_seed", starting_seed))
  make_dir(dir_path=seed_folder)
  make_dir(dir_path=file.path(seed_folder, "logs"))
  make_dir(dir_path=file.path(seed_folder, "data"))
  
  # Start log file
  flog.appender(appender.file(file.path(seed_folder, "logs", paste0("seed", starting_seed, ".txt"))))
  flog.info("Start creating new clustering template(s).")
  
  # Load proc_list and imagesList ------------------------------------------
  flog.info("Loading training graphs from proc_list and imagesList.RDS")
  proc_list=readRDS(file.path(template_dir,"data","proc_list.RDS"))
  imagesList=readRDS(file.path(template_dir,"data","images_list.RDS"))
  data = list("proc_list" = proc_list, "images_list" = imagesList, "seed_folder" = seed_folder)
  return(data)
  
}

make_dir = function(dir_path){
  if (!dir.exists(dir_path)){
    dir.create(dir_path)
  }
}


#RunKLetterMeans
# runs loop in parallel where each iteration produces a clustering template
runLetterKmeansParallel=function(num_runs, num_cores, full_images_list, proc_list, num_graphs, K, num_path_cuts, 
                                 max_iters, gamma, num_outliers, num_dist_cores, starting_seed, template_dir, seed_folder){ 
  # Choose how many graphs to use to create the template(s)
  if (num_graphs == 'All'){
    images_list=full_images_list
  } else {
    images_list=full_images_list[1:num_graphs]
  }

  doParallel::registerDoParallel(num_cores)
  resList=foreach(i=1:num_runs, 
                  .export = c('make_dir', 'chooseCenters', 'runAndSaveKmeans', 'letterToPrototype','i_to_x',
                              'letterKmeansWithOutlier_parallel', 'within_cluster_sum_of_squares', 'root_mean_square_error', 'davies_bouldin', 'variance_ratio_criterion', 'overall_meanGraph'), 
                  .packages = c('futile.logger', 'tidyr', 'dplyr', 'purrr', 'handwriter', 'parallel', 'doParallel', 'lpSolve')) %dopar% {
    
    .GlobalEnv$letterKmeansWithOutlier_parallel <- letterKmeansWithOutlier_parallel                
    .GlobalEnv$within_cluster_sum_of_squares <- within_cluster_sum_of_squares
    .GlobalEnv$root_mean_square_error <- root_mean_square_error
    .GlobalEnv$davies_bouldin <- davies_bouldin
    .GlobalEnv$variance_ratio_criterion <- variance_ratio_criterion
    .GlobalEnv$overall_meanGraph <- overall_meanGraph
    .GlobalEnv$letterToPrototype <- letterToPrototype
    
    # Add i to the starting seed
    run_seed=starting_seed+i-1
    
    # Template name
    template_name=paste0("seed", run_seed, "_run", i)
    
    # Create folder for run and subfolders for data and logs
    run_folder = file.path(seed_folder, template_name)
    make_dir(dir_path=run_folder)
    make_dir(dir_path=file.path(run_folder, "logs"))
    make_dir(dir_path=file.path(run_folder, "data"))
    
    # Start new log file for run i
    flog.appender(appender.file(file.path(run_folder, "logs", paste0(template_name, ".txt"))), name=paste0("run", i))
    flog.info("Start creating template %d", i, ".", name=paste0("run", i))
    
    # Choose cluster centers. NOTE: Even if you are testing the code on a small number of 
    # graphs, you need to select centers from the full list of graphs.
    flog.info("Choosing starting cluster centers.", name=paste0("run", i))
    centers=chooseCenters(run_seed=run_seed, K=K, proc_list=proc_list, images_list=full_images_list)
    
    # Run Kmeans
    template=runAndSaveKmeans(i, images_list, K, centers, num_path_cuts, max_iters, gamma, 
                              num_outliers, num_dist_cores, run_seed, run_folder, template_name)
  }
  return(resList)
}


# Helper function - calls another function to create a clustering template, then saves the template
runAndSaveKmeans=function(run_number, images_list, K, centers, num_path_cuts, max_iters, gamma, 
                          num_outliers, num_dist_cores, run_seed, run_folder, template_name)
{
  template=letterKmeansWithOutlier_parallel(run_number=run_number, images_list=images_list, K=K, 
                                            num_path_cuts=num_path_cuts, max_iters=max_iters, 
                                            gamma=gamma, num_outliers=num_outliers, centers=centers, 
                                            num_dist_cores=num_dist_cores)
  flog.info("Saving template %d .", run_number, name=paste0("run", run_number))
  saveRDS(template, file=file.path(run_folder, "data", paste0(template_name, ".rds")))
  return(template)
}


# Starting Centers --------------------------------------------------------
chooseCenters = function(run_seed, K, proc_list, images_list) {
  set.seed(seed=run_seed)
  
  # Make vectors of document #, letter #, and strata for each graph
  doc=letter=stratum_a=c()
  for(i in 1:length(proc_list)){
    for(j in 1:length(proc_list[[i]]$process$letterList)){
      doc=c(doc,i)
      letter=c(letter,j)
      stratum_a=c(stratum_a, proc_list[[i]]$process$letterList[[j]]$characterFeatures$stratum)
    }
  }
  
  lvls=c("1loop", "2loop", sort(as.numeric(unique(stratum_a[!(stratum_a %in% c("1loop", "2loop"))]))))
  # Select graphs as starting cluster centers by randomly selecting 5 graphs with 1 loop, 2 graphs with 2 loops,
  # 5 graphs with 1 edge, 6 graphs with 2 edges, and so on. NOTE: numstrat must sum to # of
  # clusters K.
  numstrat=c(5, 2, 5, 6, 5, 3, 2, 2, 2, rep(1,8))
  # Adjust numstrat to be the same length as lvls by either adding or
  # subtracting trailing zeros on the right. 
  numstrat=c(numstrat, rep(0, length(lvls) - length(numstrat)))
  
  samplingdf=data.frame(doc=doc, letter=letter, stratum=stratum_a, ind=1:length(stratum_a))
  samplingdf=samplingdf %>%
    mutate(stratumfac=factor(stratum, levels=lvls)) %>%
    group_by(stratumfac) %>% 
    nest() %>%            
    ungroup() %>% 
    arrange(stratumfac) %>%
    mutate(n=numstrat) %>% 
    mutate(samp=map2(data, n, sample_n)) %>% 
    select(-data) %>%
    unnest(samp)
  
  # Reformat starting cluster centers as prototype graphs
  centerstarts=replicate(K, list())
  for(k in 1:K){
    centerstarts[[k]] = letterToPrototype(images_list[[samplingdf$ind[k]]], numPathCuts=8)
  }
  
  return(centerstarts)
}


# Kmeans ------------------------------------------------------------------
letterKmeansWithOutlier_parallel = function(images_list, K, centers, num_path_cuts, max_iters, gamma, num_outliers, num_dist_cores, run_number)
{ 
  get('within_cluster_sum_of_squares')
  
  # Initialize
  n=length(images_list)
  oldCluster=rep(0, n)  # previous cluster assignment for each graph
  cluster=rep(NA, n)  # current cluster assignment for each graph
  iters=0 
  dists=matrix(NA, ncol=K, nrow=n)  # distance between each graph and each cluster center
  wcd=matrix(NA, nrow=max_iters, ncol=n)  # within-cluster distances
  centerMoved=rep(TRUE, K)  # whether each cluster lost or grained graphs during current iteration
  centerMovedOld=rep(TRUE, K)  # whether each cluster lost or grained graphs on last iteration
  oldCenters=centers
  changes=c()  # number of graphs that changed clusters on each iteration
  db=c()  # Davies-Bouldin Index
  vrc=c()  # Variance ratio criterion
  wcss=c()  # Within-cluster sum of squares
  rmse=c()  # Root mean square error
  
  # Initial settings for outliers
  current_outlierCutoff=Inf
  outlierCutoff=c()  # save outlier cutoff on each iteration
  outliers=NULL
  potentialOutliers=NULL
  
  ## ADDING PARALLEL
  doParallel::registerDoParallel(num_dist_cores)
  vecoflengthj=rep(0, K)
  
  while(TRUE)
  { 
    # Cluster Assignment Step ---------------------------------------------------------
    iters=iters + 1
    flog.info("Starting iteration %d.", iters, name=paste0("run", run_number))
    
    flog.info("Calculating the distances between graphs and cluster centers.", name=paste0("run", run_number))
    # Calculate the distance between each graph and each cluster center. If the cluster center didn't change, the distances for that
    # cluster don't need to be recalculated
    listoflengthi=foreach(i=1:length(images_list), .export=c('getGraphDistance'), .packages = c('lpSolve')) %dopar% { # for each graph i
      for(j in 1:K)  # for each cluster j
      {
        if(centerMoved[j] | centerMovedOld[j])  # if cluster j's center changed
        { # calculate the distance between graph i and cluster j
          vecoflengthj[j]=getGraphDistance(images_list[[i]], centers[[j]], isProto2=TRUE, numPathCuts = num_path_cuts)$matching_weight
        }
        else
          vecoflengthj[j]=dists[i,j]  # distance didn't change from last iteration
      }
      return(vecoflengthj)
    }
    
    flog.info("Assigning graphs to clusters.", name=paste0("run", run_number))
    # put distances between graphs and cluster centers in a matrix
    dists=matrix(unlist(listoflengthi), nrow=length(images_list), ncol=K, byrow=TRUE)
    
    # assign each graph to the closet cluster
    cluster=apply(dists, 1, which.min)
    
    # get the within-cluster distances 
    current_wcd=apply(dists, 1, min)
    
    # find graphs that are further from their assigned cluster than current_outlierCutoff. NOTE: no outliers are assigned on the first iteration
    potentialOutliers=which(current_wcd > current_outlierCutoff)
    
    # Sort the potential outliers by distance from their respective cluster centers. If there are more potential outliers than num_outliers
    # only keep the outliers furthest from their clusters
    outliers=potentialOutliers[rank(-1*current_wcd[current_wcd > current_outlierCutoff]) < num_outliers]
    
    # assign outliers to the outlier cluster
    cluster[outliers]=-1
    
    # If a cluster is empty or only has one graph, reassign that graph (if any) to the outlier cluster.
    # Then find the non-outlier graph that is furthest from its cluster center and reassign it to the 
    # now empty cluster
    for(i in 1:K){
      if(sum(cluster == i) < 2){
        cluster[which(cluster == i)]=-1
        cluster[cluster != -1][which.max(current_wcd[cluster != -1])]=i
      }
    }
    
    # Add current outlier cutoff to list
    outlierCutoff = c(outlierCutoff, current_outlierCutoff)
    
    # Calculate the new outlierCutoff as 
    # gamma*(current_wcd of non-outlier clusters)/(total # graphs - # outliers)
    current_outlierCutoff=gamma*sum(current_wcd[cluster > 0])/(n-sum(cluster < 0))
    
    # Recalculate the within cluster distances
    for (i in 1:n){
      if (cluster[i] == -1){
        current_wcd[i]=NA
      } else {
        current_wcd[i]=dists[i, cluster[i]]
      }
    }
    wcd[iters,]=current_wcd 
    
    # Record number and percent of graphs that changed clusters
    current_changes=sum(cluster != oldCluster)
    changes=c(changes, current_changes)
    current_perc_changes=100*current_changes/n
    flog.info("%d graphs changed clusters.", current_changes, name=paste0("run", run_number))
    flog.info("%f percent of total graphs changed clusters.", current_perc_changes, name=paste0("run", run_number))
    
    
    # Performance Measures ----------------------------------------------------
    # Caclulate the Within-Cluster Sum of Squares
    current_wcss = within_cluster_sum_of_squares(wcd=current_wcd, cluster=cluster)
    wcss = c(wcss, current_wcss)
    flog.info("The within-cluster sum of squares is %f.", current_wcss, name=paste0("run", run_number))
    
    # Calculate the root mean square error
    current_rmse = root_mean_square_error(wcd=current_wcd, cluster=cluster)
    rmse = c(rmse, current_rmse)
    flog.info("The root mean square error is %f.", current_rmse, name=paste0("run", run_number))
    
    # Calculate the Davies-Bouldin Index
    current_db=davies_bouldin(wcd=current_wcd, cluster=cluster, centers=centers, K=K, num_path_cuts=num_path_cuts)
    db=c(db, current_db)
    flog.info("The Davies-Bouldin Index is %f.", current_db, name=paste0("run", run_number))
    
    # Calculate the variance ratio criterion
    current_vrc=variance_ratio_criterion(wcd=current_wcd, cluster=cluster, centers=centers, K=K, num_path_cuts=num_path_cuts)
    vrc=c(vrc, current_vrc)
    flog.info("The variance ratio criterion is %f.", current_vrc, name=paste0("run", run_number))
    
    # Check Stopping Criteria -------------------------------------------------
    # Stop if the percent of graphs that changed clusters is <= 3%, if the
    # number of graphs that changed clusters has been constant for 3 consecutive
    # iterations, or if the max number of iterations has been reached
    if (current_perc_changes <=3){
      flog.info("Percent of graphs that changed clusters is 3% or less. Stopping K-means algorithm.", name=paste0("run", run_number))
      stop_reason = "3 percent"
      break
    }
    if ((length(tail(changes, n=3))==3) & (length(unique(tail(changes, n=3)))==1)){
      flog.info("The same number of graphs have changed clusters on the last three iterations. Stopping K-means algorithm.", name=paste0("run", run_number))
      stop_reason = "flatline"
      break
    }
    if(iters >= max_iters){
      flog.info("The maximum number of iterations has been reached. Stopping K-means algorithm.", name=paste0("run", run_number))
      stop_reason = "max iterations"
      break
    }
    
    
    # Update Centers Step -------------------------------------------------------------
    flog.info("Calculating new cluster centers.", name=paste0("run", run_number))
    # Record whether each graph changed clusters
    whichChanged=!(oldCluster == cluster)
    
    # For each cluster, record whether any of its graphs left or joined the cluster
    centerMovedOld=centerMoved  
    centerMoved=rep(FALSE, K)
    for(i in 1:K)  
    {
      if(any(whichChanged[cluster == i]))
        centerMoved[i]=TRUE
    }
    
    # For each cluster, if the cluster changed graphs, calculate the new cluster center
    for(i in 1:K)
    {
      if(centerMoved[i])  
      { # call the mean graph function with the graphs in cluster i sorted by distance, from closest to furthest) to cluster i's center 
        centers[[i]]=meanGraphSet_slowchange(images_list[cluster == i][order(current_wcd[cluster == i])], num_path_cuts=num_path_cuts) #meanGraphSet_Kmeans(images_list[cluster == i], num_path_cuts=num_path_cuts)
      }
    }
    
    # store current cluster assignments
    oldCluster=cluster
  }
  return(list(cluster=cluster, centers=centers, K=K, n=n, iters=iters, changes=changes, outlierCutoff = outlierCutoff,
              stop_reason=stop_reason, wcd=wcd, wcss=wcss, rmse=rmse, DaviesBouldinIndex=db, VarianceRatioCriterion=vrc))
}


meanGraphSet_slowchange=function(images_list, num_path_cuts=4)
{
  # indices=sample.int(length(images_list)) # adds graphs to mean calculations in a random order
  indices=1:length(images_list) # adds graphs in order
  if(length(images_list) < 1) stop("Please specify more than 0 graphs to mean.")
  
  # initialize
  dists=rep(0, length(images_list))
  
  # convert the first graph in the list to a prototype
  meanGraph1=letterToPrototype(images_list[[indices[1]]], numPathCuts=num_path_cuts)
  
  # add graphs one-by-one to the mean graph calculation
  if(length(images_list) > 1)
  {
    for(i in 2:length(images_list))
    {
      meanGraph1=weightedMeanGraphs(images_list[[indices[i]]], meanGraph1, 1/i, isProto2=TRUE, numPathCuts=num_path_cuts)
    }
  }
  
  # calculate the distance between the new mean graph and each graph in the cluster
  for(i in 1:length(images_list))
  {
    dists[i]=getGraphDistance(meanGraph1, images_list[[i]], isProto1=TRUE, numPathCuts=num_path_cuts)$matching_weight
  }
  
  # find the index of the graph that is closest to the mean graph
  if(dists[1] >= quantile(dists, 0)){
    retindex=which.min(dists)
  } else {
    retindex=1
  }
  
  # return the graph closest to the mean graph as a protoype
  return(letterToPrototype(images_list[[retindex]], numPathCuts=num_path_cuts))
}


overall_meanGraph=function(centers, num_path_cuts=8)
{
  indices=1:length(centers) # adds graphs in order
  if(length(centers) < 1) stop("Please specify more than 0 graphs to mean.")
  
  # initialize
  dists=rep(0, length(centers))
  meanGraph1=centers[[1]]
  
  # add centers one-by-one to the mean graph calculation
  if(length(centers) > 1){
    for(i in 2:length(centers)){
      meanGraph1=weightedMeanGraphs(centers[[indices[i]]], meanGraph1, 1/i, isProto1=TRUE, isProto2=TRUE, numPathCuts=num_path_cuts)
    }
  }
  
  # calculate the distance between the new mean graph and each center
  for(i in 1:length(centers))
  {
    dists[i]=getGraphDistance(meanGraph1, centers[[i]], isProto1=TRUE, isProto2=TRUE, numPathCuts=num_path_cuts)$matching_weight
  }
  
  # find the index of the graph that is closest to the mean graph
  if(dists[1] >= quantile(dists, 0)){
    retindex=which.min(dists)
  } else {
    retindex=1
  }
  
  return(list('overall_center_dists'=dists, overall_center=centers[[retindex]]))
}

#Performance
# Davies-Bouldin Index ----------------------------------------------------
davies_bouldin = function(wcd, cluster, centers, K, num_path_cuts){
  # For each cluster, calculate the average distance between the graphs in that cluster and the cluster center
  s = rep(0, K)
  for (i in 1:K){
    s[i] = mean(wcd[cluster==i])
  }
  
  # Calculate the distance between each pair of cluster centers. We don't need to calculate the distance between a center and itself, so skip those calculations.
  d = matrix(NA, nrow=K, ncol=K)
  for (i in 1:K){
    for (j in 1:K){
      if (i != j){
        d[i,j] = getGraphDistance(centers[[i]], centers[[j]], isProto1 = TRUE, isProto2 = TRUE, numPathCuts = num_path_cuts)$matching_weight
      }
    }
  }
  
  # Calculate R_ij for all i and j. Again, we don't need to measure the separation between a cluster and itself, so skip those calculations
  R = matrix(NA, nrow=K, ncol=K)
  for (i in 1:K){
    for (j in 1:K){
      if(i!=j){
        R[i,j] = (s[i] + s[j])/d[i,j]
      }
    }
  }
  
  # Make list of clusters that have at least 1 graph
  filled_clusters = unique(cluster[cluster!=-1])
  
  # Find the max value of R for each filled cluster i
  maxR = rep(NA,length(filled_clusters))
  for (i in 1:length(filled_clusters)){
    maxR[i] = max(R[filled_clusters[i],], na.rm=TRUE)
  }
  
  # Calculate the index
  db = sum(maxR)/length(filled_clusters)
  
  return(db)
}


# Variance Ratio Criterion ------------------------------------------------

variance_ratio_criterion = function(wcd, cluster, centers, K, num_path_cuts){
  # Count the number of graphs in each non-outlier cluster
  ni = c()
  for (i in 1:K){
    ni = c(ni, sum(cluster==i))
  }
  
  # Get total number of non-outlier graphs
  n = sum(ni)
  
  # Estimate the overall mean graph (the center of the centers) and measure distance from all centers to
  # the overall center
  overall = overall_meanGraph(centers = centers, num_path_cuts = num_path_cuts)
  
  # Calculate the between-cluster variance: SS_B = (k-1)^(-1) \sum_{i=1}^k n_i \cdot d(m_i, m)^2
  SSb = sum(ni*overall$overall_center_dists^2)/(K-1)
  
  # Calculate the within-cluster variance: SS_W = (n-k)^(-1)\sum_{i=1}^k \sum_{x \in C_i} d(x, m_i)^2
  wcd = wcd[cluster != -1]  # remove graphs in the outlier cluster
  SSw = sum(wcd^2)/(n - K)
  
  # Calculate variance-ratio-criterion
  vrc = SSb/SSw
  return(vrc)
}


# Within-Cluster Sum of Squares -------------------------------------------

within_cluster_sum_of_squares = function(wcd, cluster){
  # Get within cluster distances of non-outlier clusters
  wcd = wcd[cluster != -1]
  
  # Calculate within-cluster sum of squares
  wcss = sum(wcd^2)
  
  return(wcss)
}



# Root Mean Square Error --------------------------------------------------

root_mean_square_error = function(wcd, cluster){
  # Get within cluster distances of non-outlier clusters
  wcd = wcd[cluster != -1]
  
  # Get number of non-outlier graphs
  n = length(wcd)
  
  # Calculate within-cluster sum of squares
  wcss = sum(wcd^2)
  
  # Calculate the root means square error
  rmse = sqrt(wcss/n)
  
  return(rmse)
}

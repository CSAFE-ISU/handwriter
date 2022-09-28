# example data for handwriter pipeline 

# Requires: extracted graphs from all data that will be
# used. Place graph rds files in template_dir > data > template_graphs, template_dir > data > model_graphs,
# and template_dir > data > questioned_graphs folders.


main_dir <- "/Users/stephanie/Documents/non_version_control/CSAFE_test_template"


# Cluster Template --------------------------------------------------------
# create cluster template
# template <- make_clustering_templates(template_dir = main_dir, starting_seed = 100, K = 5, num_dist_cores = 4, max_iters = 3, num_graphs = 1000, num_runs=1)
# template <- readRDS(file.path(main_dir, "template_seed100", "data", "all_templates.rds"))
# example_template <- template
# usethis::use_data(example_template)

# Model Training Data -----------------------------------------------------
# Cluster Assignments
# model_proc_list <- get_clusterassignment(clustertemplate = template[[1]], input_dir = file.path(main_dir, "data", "model_graphs"))
# saveRDS(model_proc_list, file.path(main_dir, "template_seed100", "seed100_run1", "data", "model_proc_list.rds"))
# model_proc_list <- readRDS(file.path(main_dir, "template_seed100", "seed100_run1", "data", "model_proc_list.rds"))

# Example data
# example_data <- list()
# for (i in 1:length(model_proc_list)){
#   example_doc <- list()
#   example_doc$docname <- model_proc_list[[i]]$docname
#   
#   # graph-level features
#   example_doc$process$letterList <- list()
#   for (j in 1:length(model_proc_list[[i]]$process$letterList)){
#     temp_letter <- list()
#     temp_letter$cluster <- model_proc_list[[i]]$proces$letterList[[j]]$cluster
#     temp_letter$characterFeatures$slope <- model_proc_list[[i]]$proces$letterList[[j]]$characterFeatures$slope
#     temp_letter$characterFeatures$xvar <- model_proc_list[[i]]$proces$letterList[[j]]$characterFeatures$xvar
#     temp_letter$characterFeatures$yvar <- model_proc_list[[i]]$proces$letterList[[j]]$characterFeatures$yvar
#     temp_letter$characterFeatures$covar <- model_proc_list[[i]]$proces$letterList[[j]]$characterFeatures$covar
#     example_doc$process$letterList <- append(example_doc$process$letterList, list(temp_letter))
#   }
#   example_data <- append(example_data, list(example_doc))
# }
# 
# example_model_proc_list <- example_data
# usethis::use_data(example_model_proc_list, overwrite = TRUE)

# Format training data for rjags
# USER INPUT: location of writer and doc in file names
writer_indices = c(2,5)
doc_indices = c(7,18)

model_data <- format_model_data(example_model_proc_list, writer_indices, doc_indices,  a = 2, b = 0.25, c = 2, d = 2, e = 0.5)
# saveRDS(model_data, file = file.path(main_dir, "template_seed100", "seed100_run1", "data", "model_data.rds"))

# save for example data
# example_model_training_data <- model_data
# usethis::use_data(example_model_training_data, overwrite = TRUE)



# Fit Model ---------------------------------------------------------------
# usethis::use_data(model_wrapped_cauchy)

draws <- fit_model(model_training_data = example_model_training_data, num_iters = 4000)
draws <- drop_burnin(draws, burn_in = 1000)


# questioned documents ----------------------------------------------------
# get cluster assignments
template <- readRDS(file.path(main_dir, "template_seed100", "data", "all_templates.rds"))
questioned_proc_list <- get_clusterassignment(clustertemplate = template[[1]], input_dir = file.path(main_dir, "data", "questioned_graphs"))
saveRDS(questioned_proc_list, file.path(main_dir, "template_seed100", "seed100_run1", "data", "questioned_proc_list.rds"))
questioned_proc_list <- readRDS(file.path(main_dir, "template_seed100", "seed100_run1", "data", "questioned_proc_list.rds"))

# Example data
# example_data <- list()
# for (i in 1:length(questioned_proc_list)){
#   example_doc <- list()
#   example_doc$docname <- questioned_proc_list[[i]]$docname
# 
#   # graph-level features
#   example_doc$process$letterList <- list()
#   for (j in 1:length(questioned_proc_list[[i]]$process$letterList)){
#     temp_letter <- list()
#     temp_letter$cluster <- questioned_proc_list[[i]]$proces$letterList[[j]]$cluster
#     temp_letter$characterFeatures$slope <- questioned_proc_list[[i]]$proces$letterList[[j]]$characterFeatures$slope
#     temp_letter$characterFeatures$xvar <- questioned_proc_list[[i]]$proces$letterList[[j]]$characterFeatures$xvar
#     temp_letter$characterFeatures$yvar <- questioned_proc_list[[i]]$proces$letterList[[j]]$characterFeatures$yvar
#     temp_letter$characterFeatures$covar <- questioned_proc_list[[i]]$proces$letterList[[j]]$characterFeatures$covar
#     example_doc$process$letterList <- append(example_doc$process$letterList, list(temp_letter))
#   }
#   example_data <- append(example_data, list(example_doc))
# }

# example_questioned_proc_list <- example_data
# usethis::use_data(example_questioned_proc_list, overwrite = TRUE)

# questioned_data <- format_questioned_data(example_questioned_proc_list, writer_indices, doc_indices)
# example_questioned_data <- questioned_data
# usethis::use_data(example_questioned_data)

analysis <- analyze_questioned_documents(example_model_training_data, draws, example_questioned_data, num_cores = 4)
example_analysis <- analysis
usethis::use_data(example_analysis)


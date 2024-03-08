## code to prepare `example_data`

# The functions in this script create `example_cluster_template`,
# `example_model_1chain`, and `example_analysis_1chain`, and save them in the
# data folder as rda files.

# Creating these data objects requires saving processed handwriting files, template helper files,
# and other assorted files to a folder on the local computer. Some of these files will be used 
# in tests so create the new example data in tests > testthat > fixtures > template.

# All of the functions below are set to overwrite example data files currently in the data folder.


# helper functions ----
make_example_template <- function(main_dir, centers_seed, graphs_seed) {
  # create folder if it doesn't already exist
  if (!dir.exists(main_dir)){dir.create(main_dir)}
  
  template_docs <- system.file("extdata/example_images/template_docs", 
                                     package = "handwriter")
  
  example_cluster_template <- make_clustering_templates(template_dir = main_dir,
                                                        template_images_dir = template_docs,
                                                        writer_indices = c(2,5),
                                                        max_edges = 30,
                                                        K = 10,
                                                        num_dist_cores = 2,
                                                        max_iters = 3,
                                                        num_graphs = 1000,
                                                        centers_seed = centers_seed,
                                                        graphs_seed = graphs_seed)
  
  # save to data folder
  usethis::use_data(example_cluster_template, overwrite = TRUE)
}

make_example_model <- function(main_dir){
  model_docs <- system.file("extdata/example_images/model_docs", 
                                  package = "handwriter")
  example_model_1chain <- fit_model(template_dir = main_dir, 
                                    model_images_dir = model_docs,
                                    num_iters = 200, 
                                    num_chains = 1, 
                                    num_cores = 5,
                                    writer_indices = c(2,5), 
                                    doc_indices = c(7,18))
  # save to data folder
  usethis::use_data(example_model_1chain, overwrite = TRUE)
}


make_example_analyses <- function(main_dir, num_cores = 5) {
  questioned_docs <- system.file("extdata/example_images/questioned_docs", 
                                       package = "handwriter")
  
  example_analysis_1chain <- analyze_questioned_documents(template_dir = main_dir, 
                                                          questioned_images_dir = questioned_docs, 
                                                          model = example_model_1chain, 
                                                          num_cores = num_cores,
                                                          writer_indices = c(2,5), 
                                                          doc_indices = c(7,18))
  
  usethis::use_data(example_analysis_1chain, overwrite = TRUE)
}

# create example data ----
# build template and model in tests folder so that it can be used for testing
main_dir <- testthat::test_path("fixtures", "template")

# choose starting seed and run number 
# NOTE: I chose these seeds because they result in a single chain
# model that has high accuracy, 0.95, on the test documents
centers_seed <- 100
graphs_seed <- 104

# make example template
make_example_template(main_dir, centers_seed, graphs_seed)

# make example models using the new example template
devtools::load_all()
make_example_model(main_dir)

# make example analyses using the new example models
devtools::load_all()
make_example_analyses(main_dir)

# delete template and model files not needed for tests ----
# analysis.rds is now saved as example_analysis_1chain
file.remove(testthat::test_path("fixtures", "template", "data", "analysis.rds"))  

# model.rds is now saved as example_model_1chain
file.remove(testthat::test_path("fixtures", "template", "data", "model.rds"))  

# only template.rds is used in the tests so other template files can be deleted
unlink(testthat::test_path("fixtures", "template", "data", "template_graphs"), recursive = TRUE) 
file.remove(testthat::test_path("fixtures", "template", "data", "template_images_list.rds")) 
file.remove(testthat::test_path("fixtures", "template", "data", "template_proc_list.rds")) 
file.remove(testthat::test_path("fixtures", "template", "data", "template_strata.rds")) 

# log files aren't used in the tests
unlink(testthat::test_path("fixtures", "template", "logs"), recursive = TRUE) 

# delete model_clusters and questioned_clusters folders. These folders are
# simply back-up for model_clusters.rds and questioned_clusters.rds.
unlink(testthat::test_path("fixtures", "template", "data", "model_clusters"), recursive = TRUE) 
unlink(testthat::test_path("fixtures", "template", "data", "questioned_clusters"), recursive = TRUE) 

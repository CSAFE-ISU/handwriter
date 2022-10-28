# choose main directory ----
# path to main directory
main_dir <- "/Users/stephanie/Documents/non_version_control/CSAFE_example_data_new"

# choose starting seed and run number
seed <- 100


make_example_template <- function(main_dir, seed) {
  # create folder if it doesn't already exist
  if (!dir.exists(main_dir)){dir.create(main_dir)}
  
  template_images_dir <- system.file("extdata/example_images/template_training_images", 
                                     package = "handwriter")
  
  example_cluster_template <- make_clustering_templates(template_dir = main_dir,
                                                        template_images_dir = template_images_dir,
                                                        writer_indices = c(2,5),
                                                        max_edges = 30,
                                                        K = 10,
                                                        num_dist_cores = 2,
                                                        max_iters = 3,
                                                        num_graphs = 1000,
                                                        seed = seed)
  
  usethis::use_data(example_cluster_template, overwrite = TRUE)
}


make_example_models <- function(main_dir){
  model_images_dir <- system.file("extdata/example_images/model_training_images", 
                                  package = "handwriter")
  example_model_1chain <- fit_model(template_dir = main_dir, 
                     model_images_dir = model_images_dir,
                     num_iters = 100, 
                     num_chains = 1, 
                     writer_indices = c(2,5), 
                     doc_indices = c(7,18))
  usethis::use_data(example_model_1chain, overwrite = TRUE)
  
  example_model_2chains <- fit_model(template_dir = main_dir, 
                                    model_images_dir = model_images_dir,
                                    num_iters = 100, 
                                    num_chains = 1, 
                                    writer_indices = c(2,5), 
                                    doc_indices = c(7,18))
  usethis::use_data(example_model_2chains, overwrite = TRUE)
}


make_example_analyses <- function(main_dir, num_cores = 5) {
  questioned_images_dir <- system.file("extdata/example_images/questioned_images", 
                                       package = "handwriter")
  
  example_analysis_1chain <- analyze_questioned_documents(template_dir = main_dir, 
                                           questioned_images_dir = questioned_images_dir, 
                                           model = example_model_1chain, 
                                           num_cores = num_cores)
  
  usethis::use_data(example_analysis_1chain, overwrite = TRUE)
  
  example_analysis_2chains <- analyze_questioned_documents(template_dir = main_dir, 
                                                          questioned_images_dir = questioned_images_dir, 
                                                          model = example_model_2chains, 
                                                          num_cores = num_cores)
  
  usethis::use_data(example_analysis_2chains, overwrite = TRUE)
}


make_example_template(main_dir, seed)
make_example_models(main_dir)
make_example_analyses(main_dir)

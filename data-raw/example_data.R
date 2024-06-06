## code to prepare `example_data`

# The functions in this script create `example_cluster_template`,
# `example_model`, and `example_analysis`, and save them in the
# data folder as rda files.

# Creating these data objects requires saving processed handwriting files, template helper files,
# and other assorted files to a folder on the local computer.

# All of the functions below are set to overwrite example data files currently in the data folder.


# helper functions ----
make_example_template <- function(main_dir, centers_seed) {
  # create folder if it doesn't already exist
  if (!dir.exists(main_dir)){dir.create(main_dir)}
  
  template_docs <- file.path(main_dir, 'data', 'template_docs')
  
  example_cluster_template <- make_clustering_template(main_dir = main_dir,
                                                       template_docs = template_docs,
                                                       writer_indices = c(2,5),
                                                       K = 10,
                                                       num_dist_cores = 2,
                                                       max_iters = 3,
                                                       centers_seed = centers_seed)
  
  # save to data folder
  usethis::use_data(example_cluster_template, overwrite = TRUE)
}

make_example_model <- function(main_dir){
  model_docs <- file.path(main_dir, 'data', 'model_docs')
  
  example_model <- fit_model(main_dir = main_dir, 
                             model_docs = model_docs,
                             num_iters = 200, 
                             num_chains = 1, 
                             num_cores = 5,
                             writer_indices = c(2,5), 
                             doc_indices = c(7,18))
  # save to data folder
  usethis::use_data(example_model, overwrite = TRUE)
}


make_example_analyses <- function(main_dir, num_cores = 5) {
  questioned_docs <- file.path(main_dir, 'data', 'questioned_docs')
  
  example_analysis <- analyze_questioned_documents(main_dir = main_dir, 
                                                   questioned_docs = questioned_docs, 
                                                   model = example_model, 
                                                   num_cores = num_cores,
                                                   writer_indices = c(2,5), 
                                                   doc_indices = c(7,18))
  
  usethis::use_data(example_analysis, overwrite = TRUE)
}

# create example data ----
# build template and model in tests folder so that it can be used for testing
main_dir <- file.path('examples', 'example_template')

# choose starting seed
centers_seed <- 100

# make example template
make_example_template(main_dir, centers_seed)

# make example models using the new example template
devtools::load_all()
make_example_model(main_dir)

# make example analyses using the new example models
devtools::load_all()
make_example_analyses(main_dir)

# manually delete all files from examples > example_template > data except the 3 docs folders


# handwriter pipeline using example data stored in the package

# process handwriting ----
main_dir <- "/Users/stephanie/Documents/non_version_control/CSAFE_toy_pipeline"

# process clustering template training images
template_images_dir <- system.file("extdata/example_images/template_training_images", package = "handwriter")
template_graphs_dir <- file.path(main_dir, "data", "template_graphs")
process_batch_dir(image_batch = template_images_dir,
                  batch_output_dir = template_graphs_dir,
                  transform_output = 'document')

# process model training images
model_images_dir <- system.file("extdata/example_images/model_training_images", package = "handwriter")
model_graphs_dir <- file.path(main_dir, "data", "model_graphs")
process_batch_dir(image_batch = model_images_dir,
                  batch_output_dir = model_graphs_dir,
                  transform_output = 'document')

# process questioned images
questioned_images_dir <- system.file("extdata/example_images/questioned_images", package = "handwriter")
questioned_graphs_dir <- file.path(main_dir, "data", "questioned_graphs")
process_batch_dir(image_batch = questioned_images_dir,
                  batch_output_dir = questioned_graphs_dir,
                  transform_output = 'document')

# create clustering template ----
template <- make_clustering_templates(template_dir = main_dir,
                                      K = 5,
                                      num_dist_cores = 4,
                                      max_iters = 3,
                                      num_graphs = 1000,
                                      num_runs = 1,
                                      starting_seed = 200)


# fit model ----
m_proc_list <- get_clusterassignment(clustertemplate = template[[1]], input_dir = model_graphs_dir)
md <- format_model_data(proc_list=m_proc_list, 
                        writer_indices=c(2,5), 
                        doc_indices=c(7,18), 
                        a=2, b=0.25, c=2, d=2, e=0.5)
draws <- fit_model(md, num_iters = 4000)
draws <- drop_burnin(draws, 1000)

# questioned documents ----
q_proc_list <- get_clusterassignment(clustertemplate = template[[1]], input_dir = questioned_graphs_dir)
qd <- format_questioned_data(proc_list = example_questioned_proc_list,
                             writer_indices=c(2,5), 
                             doc_indices=c(7,18))
analysis <- analyze_questioned_documents(md, draws, qd, num_cores = 4)

plot_posterior_probabilities(analysis)





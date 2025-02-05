# example_cluster_template and 1 QD ---------------------------------------

main_dir <- testthat::test_path("fixtures", "temp1qd")

# make template identical to example_cluster_template
make_clustering_template(main_dir = main_dir,
                         template_docs = file.path(main_dir, 'data', 'template_docs'),
                         writer_indices = c(1, 5),
                         K = 5,
                         num_dist_cores = 1,
                         max_iters = 3,
                         centers_seed = 100)


# make model with same settings as example_model. Even if we set the seed the
# models will not be identical.
model <- fit_model(main_dir = main_dir,
                   model_docs = file.path(main_dir, 'data', 'model_docs'),
                   num_iters = 200,
                   num_chains = 1,
                   num_cores = 1,
                   writer_indices = c(1, 5),
                   doc_indices = c(7, 18))

analysis <- analyze_questioned_documents(main_dir = main_dir,
                                         questioned_docs = file.path(main_dir, "data", "questioned_docs"),
                                         model = model,
                                         num_cores = 2,
                                         writer_indices = c(1, 5),
                                         doc_indices = c(7, 18))

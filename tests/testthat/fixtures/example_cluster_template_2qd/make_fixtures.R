# example_cluster_template and 2 QD ---------------------------------------

# copied model_clusters, model_clusters.rds, model_docs, model_graphs,
# model.rds, template_docs, template_graphs, and template.rds from fixtures >
# example_cluster_template_1qd

main_dir <- testthat::test_path("fixtures", "example_cluster_template_2qd")

analysis <- analyze_questioned_documents(main_dir = main_dir,
                                         questioned_docs = file.path(main_dir, "data", "questioned_docs"),
                                         model = model,
                                         num_cores = 2,
                                         writer_indices = c(1, 5),
                                         doc_indices = c(7, 18))

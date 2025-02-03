devtools::load_all()

main_dir <- testthat::test_path("fixtures", "template")

actual <- make_clustering_template(main_dir = main_dir,
                                   template_docs = file.path(main_dir, "data", "template_docs"),
                                   writer_indices = c(1, 5),
                                   K = 5,
                                   num_dist_cores = 1,
                                   max_iters = 3,
                                   centers_seed = 100)

actual <- fit_model(main_dir = main_dir,
                    model_docs = file.path(main_dir, "data", "model_docs"),
                    num_iters = 200,
                    num_chains = 1,
                    num_cores = 1,
                    writer_indices = c(1, 5),
                    doc_indices = c(7, 18))

actual <- analyze_questioned_documents(main_dir = main_dir,
                                       questioned_docs = file.path(main_dir, 'data', 'questioned_docs'),
                                       model = example_model,
                                       num_cores = 1,
                                       writer_indices = c(1, 5),
                                       doc_indices = c(7, 18))

# Fixtures for format_template_data() ----

# Template data with outliers
template_data_w_outliers <- format_template_data(template = example_cluster_template)
saveRDS(template_data_w_outliers, testthat::test_path("fixtures", "template", "template_data_w_outliers.rds"))

# Template data without outliers
no_outliers <- example_cluster_template
not_outliers <- no_outliers$cluster != -1
no_outliers$cluster <- no_outliers$cluster[not_outliers]
no_outliers$writers <- no_outliers$writers[not_outliers]
no_outliers$doc <- no_outliers$doc[not_outliers]
template_data_wo_outliers <- format_template_data(template = no_outliers)
saveRDS(template_data_wo_outliers, testthat::test_path("fixtures", "template", "template_data_wo_outliers.rds"))

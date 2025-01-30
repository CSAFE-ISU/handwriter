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

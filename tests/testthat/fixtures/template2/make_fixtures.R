devtools::load_all()

main_dir <- testthat::test_path("fixtures", "template2")

# save template in folder
saveRDS(example_cluster_template, testthat::test_path("fixtures", "template2", "data", "template.rds"))

actual <- analyze_questioned_documents(main_dir = main_dir,
                                       questioned_docs = file.path(main_dir, 'data', 'questioned_docs'),
                                       model = example_model,
                                       num_cores = 1,
                                       writer_indices = c(1, 5),
                                       doc_indices = c(7, 18))

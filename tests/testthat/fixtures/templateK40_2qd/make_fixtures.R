# templateK40 with 2 QD ---------------------------------------------------

# Copied model_docs and questioned_docs from fixtures >
# example_cluster_template_2qd

main_dir <- testthat::test_path("fixtures", "templateK40_2qd")

# save templateK40 in folder
saveRDS(templateK40, file.path(main_dir, "data", "template.rds"))

# fit model
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

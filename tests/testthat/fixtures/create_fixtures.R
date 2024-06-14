# Create a model and analysis with writer IDs that contain letters and numbers

main_dir <- testthat::test_path('fixtures', 'template')
model_docs <- file.path(main_dir, 'data', 'model_docs')
questioned_docs <- file.path(main_dir, 'data', 'questioned_docs')

# save example template in fixtures folder
saveRDS(example_cluster_template, file.path(main_dir, 'data', 'template.rds'))

example_model <- fit_model(main_dir = main_dir, 
                           model_docs = model_docs,
                           num_iters = 200, 
                           num_chains = 1, 
                           num_cores = 5,
                           writer_indices = c(1, 5), 
                           doc_indices = c(7, 18))

example_analysis <- analyze_questioned_documents(main_dir = main_dir, 
                                                 questioned_docs = questioned_docs, 
                                                 model = example_model, 
                                                 num_cores = 5,
                                                 writer_indices = c(1, 5), 
                                                 doc_indices = c(7, 18))

# Delete the unnecessary items from tests > testthat > fixtures > template > data
files_to_delete <- c('template.rds')
sapply(files_to_delete, function(x) {if (file.exists(file.path(main_dir, 'data', x))){file.remove(file.path(main_dir, 'data', x))}})
dirs_to_delete <- c('model_clusters', 'model_graphs', 'questioned_clusters', 'questioned_graphs')
sapply(dirs_to_delete, function(x) unlink(file.path(main_dir, 'data', x), recursive = TRUE))

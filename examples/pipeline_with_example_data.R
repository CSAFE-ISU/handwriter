# handwriter pipeline using example data stored in the package


main_dir <- "/Volumes/research/csafe-handwriting/Data_Processing/Stage6_Cluster_Templates/test_template"

# Fit Model ---------------------------------------------------------------
# Format training data for rjags
writer_indices = c(2,5)
doc_indices = c(7,18)
model_data <- format_model_data(example_model_proc_list, writer_indices, doc_indices,  a = 2, b = 0.25, c = 2, d = 2, e = 0.5)

# fit model to training data
draws <- fit_model(model_training_data = example_model_training_data, num_iters = 4000)
draws <- drop_burnin(draws, burn_in = 1000)


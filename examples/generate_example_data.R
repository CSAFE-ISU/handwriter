# choose main directory ----
# path to main directory
main_dir <- "/Users/stephanie/Documents/non_version_control/CSAFE_example_data"

# choose starting seed and run number
start_seed <- 100
run <- 1


make_example_template <- function(main_dir, run, start_seed) {
  # create folder if it doesn't already exist
  if (!dir.exists(main_dir)){dir.create(main_dir)}
  
  # create cluster template ----
  # get the path to the handwriter image directory
  template_images_dir <- system.file("extdata/example_images/template_training_images", package = "handwriter")
  
  # choose where to save the processed 
  template_graphs_dir <- file.path(main_dir, "data", "template_graphs")
  
  # process the handwriting
  process_batch_dir(input_dir = template_images_dir,
                    output_dir = template_graphs_dir,
                    transform_output = 'document')
  
  example_cluster_template <- make_clustering_templates(template_dir = main_dir,
                                                        writer_indices = c(2,5),
                                                        K = 10,
                                                        num_dist_cores = 4,
                                                        max_iters = 3,
                                                        num_graphs = 1000,
                                                        num_runs = run,
                                                        starting_seed = start_seed)
  
  usethis::use_data(example_cluster_template, overwrite = TRUE)
}


make_example_model_clusters <- function(main_dir, start_seed, run) {
  # get the path to the handwriter image directory
  model_images_dir <- system.file("extdata/example_images/model_training_images", 
                                  package = "handwriter")
  
  # choose where to save the processed handwriting
  model_graphs_dir <- file.path(main_dir, 
                                paste0("template_seed", start_seed),
                                paste0("seed", start_seed + run - 1, "_run", run),
                                "data", 
                                "model_graphs")
  
  # process the handwriting
  process_batch_dir(image_batch = model_images_dir,
                    batch_output_dir = model_graphs_dir,
                    transform_output = 'document')
  
  # assign model graphs to clusters
  example_model_clusters <- get_clusterassignment(clustertemplate = example_cluster_template[[1]],
                                                  input_dir = model_graphs_dir)
  usethis::use_data(example_model_clusters, overwrite = TRUE)
}


make_example_model_data <- function() {
  example_model_data <- format_model_data(model_proc_list=example_model_clusters, 
                                          writer_indices=c(2,5), 
                                          doc_indices=c(7,18), 
                                          a=2, b=0.25, c=2, d=2, e=0.5)
  usethis::use_data(example_model_data, overwrite = TRUE)
}

make_rjags_model_wrapped_cauchy <- function() {
  model_wrapped_cauchy <- "
model {
  for(letter in 1:numletters){  
    # numletters = num unique letters with measurements
    # use zeros trick to sample from wrapped-Cauchy distribution
    nll_datamodel[letter] = -log( (1-pow(tau[letterwriter[letter], lettercluster[letter]],2)) / (2*pi_constant*(1+pow(tau[letterwriter[letter], lettercluster[letter]],2)-2*tau[letterwriter[letter], lettercluster[letter]]*cos(pc_wrapped[letter]-mu[letterwriter[letter], lettercluster[letter]]))) ) + C
    zero_vec[letter] ~ dpois( nll_datamodel[letter] )
  }

  # Priors for wrapped cauchy
  for(g in 1:Gsmall){
    # g = cluster
    gamma[g] ~ dgamma(a, b)
    eta[g] ~ dunif(0,2*pi_constant)
    for(w in 1:W){
      # W = num unique writers 
      # use zeros trick to sample from wrapped-Cauchy distribution
      mu[w,g]  ~ dunif(0,2*pi_constant)
      nld_locationparam[w,g] = -log( (1-pow(e,2)) / (2*pi_constant*(1+pow(e,2)-2*e*cos(mu[w,g]-eta[g]))) ) + C
      zero_mat[w,g] ~ dpois(nld_locationparam[w,g])
      tau[w,g] ~ dbeta(c,d)
    }
  }

  for (w in 1:W) {  
    # w = writer
    pi[w,1:G] ~ ddirch(gamma[1:G] + 0.001)
  }

  for(d in 1:D) {                                       
    # d = document
    Y[d,1:G] ~ dmulti(pi[docwriter[d],1:G], docN[d])
  }

  # other values
  C = 30   # for the zeros trick
  pi_constant = 3.14159
  pi_1 = -pi_constant
}"
  usethis::use_data(model_wrapped_cauchy, overwrite = TRUE)
}


make_example_models <- function(){
  example_model_1chain <- fit_model(model_data = example_model_data,
                                     num_iters = 50,
                                     num_chains = 1)
  usethis::use_data(example_model_1chain, overwrite = TRUE)
  
  example_model_2chains <- fit_model(model_data = example_model_data,
                             num_iters = 50,
                             num_chains = 2)
  usethis::use_data(example_model_2chains, overwrite = TRUE)
}


make_example_questioned_clusters <- function(main_dir) {
  # get the path to the handwriter image directory
  questioned_images_dir <- system.file("extdata/example_images/questioned_images", 
                                       package = "handwriter")
  
  # choose where to save the processed handwriting
  questioned_graphs_dir <- file.path(main_dir, "data", "questioned_graphs")
  
  # process the handwriting
  process_batch_dir(image_batch = questioned_images_dir,
                    batch_output_dir = questioned_graphs_dir,
                    transform_output = 'document')
  
  # get cluster assignments
  example_questioned_clusters <- get_clusterassignment(clustertemplate = example_cluster_template[[1]], 
                                                       input_dir = questioned_graphs_dir)
  usethis::use_data(example_questioned_clusters)
}


make_example_questioned_data <- function() {
  example_questioned_data <- format_questioned_data(formatted_model_data = example_model_data,
                                                    questioned_proc_list = example_questioned_clusters,
                                                    writer_indices=c(2,5), 
                                                    doc_indices=c(7,18))
  usethis::use_data(example_questioned_data, overwrite = TRUE)
}


make_example_analysis <- function(num_cores) {
  # analyze questioned documents
  example_analysis <- analyze_questioned_documents(model_data = example_model_data, 
                                                   model = example_model_1chain, 
                                                   questioned_data = example_questioned_data, 
                                                   num_cores = num_cores)
  usethis::use_data(example_analysis, overwrite = TRUE)
}

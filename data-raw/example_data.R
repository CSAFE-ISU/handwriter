## code to prepare `example_data`

# The functions in this script create `example_cluster_template`,
# `model_wrapped_cauchy`, `example_model_1chain`, `example_model_2chains`,
# `example_analysis_1chain`, and `example_analysis_2chains` and save them in the
# data folder as rda files.

# Creating these data objects requires saving processed handwriting files, template helper files,
# and other assorted files to a folder on the local computer. Specify which folder you would 
# like to use for this task with `main_dir`. The `main_dir` folder may be deleted after the 
# example data has been saved to the data folder.

# All of the functions below are set to overwrite example data files currently in the data folder.

# choose main directory ----
# path to main directory
main_dir <- "/Users/stephanie/Documents/non_version_control/CSAFE_example_data_new"

# choose starting seed and run number
seed <- 100


make_example_template <- function(main_dir, seed) {
  # create folder if it doesn't already exist
  if (!dir.exists(main_dir)){dir.create(main_dir)}
  
  template_images_dir <- system.file("extdata/example_images/template_training_images", 
                                     package = "handwriter")
  
  example_cluster_template <- make_clustering_templates(template_dir = main_dir,
                                                        template_images_dir = template_images_dir,
                                                        writer_indices = c(2,5),
                                                        max_edges = 30,
                                                        K = 10,
                                                        num_dist_cores = 2,
                                                        max_iters = 3,
                                                        num_graphs = 1000,
                                                        seed = seed)
  
  usethis::use_data(example_cluster_template, overwrite = TRUE)
}

make_model_wrapped_cauchy <- function(){
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


make_example_models <- function(main_dir){
  model_images_dir <- system.file("extdata/example_images/model_training_images", 
                                  package = "handwriter")
  example_model_1chain <- fit_model(template_dir = main_dir, 
                                    model_images_dir = model_images_dir,
                                    num_iters = 100, 
                                    num_chains = 1, 
                                    writer_indices = c(2,5), 
                                    doc_indices = c(7,18))
  usethis::use_data(example_model_1chain, overwrite = TRUE)
  
  example_model_2chains <- fit_model(template_dir = main_dir, 
                                     model_images_dir = model_images_dir,
                                     num_iters = 100, 
                                     num_chains = 2, 
                                     writer_indices = c(2,5), 
                                     doc_indices = c(7,18))
  usethis::use_data(example_model_2chains, overwrite = TRUE)
}


make_example_analyses <- function(main_dir, num_cores = 5) {
  questioned_images_dir <- system.file("extdata/example_images/questioned_images", 
                                       package = "handwriter")
  
  example_analysis_1chain <- analyze_questioned_documents(template_dir = main_dir, 
                                                          questioned_images_dir = questioned_images_dir, 
                                                          model = example_model_1chain, 
                                                          num_cores = num_cores)
  
  usethis::use_data(example_analysis_1chain, overwrite = TRUE)
  
  example_analysis_2chains <- analyze_questioned_documents(template_dir = main_dir, 
                                                           questioned_images_dir = questioned_images_dir, 
                                                           model = example_model_2chains, 
                                                           num_cores = num_cores)
  
  usethis::use_data(example_analysis_2chains, overwrite = TRUE)
}


make_example_template(main_dir, seed)
make_model_wrapped_cauchy()
make_example_models(main_dir)
make_example_analyses(main_dir)

# EXPORTED ----------------------------------------------------------------


#' fit_model
#'
#' `fit_model()` fits a Bayesian hierarchical model to `model_training_data` and
#' draws samples from the model with MCMC.
#'
#' @param template_dir A directory that contains a cluster template created by
#'   [`make_clustering_templates`]
#' @param model_images_dir A directory containing model training documents
#' @param num_iters An integer number of iterations of MCMC.
#' @param num_chains An integer number of chains to use.
#' @param num_cores An integer number of cores to use for parallel processing
#'   clustering assignments. The model fitting is not done in parallel.
#' @param writer_indices A vector of the start and stop character of the writer
#'   ID in the model training file names. E.g., if the file names are
#'   writer0195_doc1, writer0210_doc1, writer0033_doc1 then writer_indices is
#'   'c(7,10)'.
#' @param doc_indices A vector of the start and stop character of the "document
#'   name" in the model training file names. This is used to distinguish between
#'   two documents written by the same writer. E.g., if the file names are
#'   writer0195_doc1, writer0195_doc2, writer0033_doc1, writer0033_doc2 then
#'   doc_indices are 'c(12,15)'.
#' @param a The shape parameter for the Gamma distribution in the hierarchical
#'   model
#' @param b The rate parameter for the Gamma distribution in the hierarchical
#'   model
#' @param c The first shape parameter for the Beta distribution in the
#'   hierarchical model
#' @param d The second shape parameter for the Beta distribution in the
#'   hierarchical model
#' @param e The scale parameter for the hyper prior for mu in the hierarchical
#'   model
#' @return A list of training data used to fit the model and the fitted model
#'
#' @examples
#' \dontrun{
#' template_dir <- "/path/to/template_directory"
#' model_images_dir <- system.file("extdata/example_images/model_training_images",
#'   package = "handwriter"
#' )
#' questioned_images_dir <- system.file("extdata/example_images/questioned_images",
#'   package = "handwriter"
#' )
#'
#' model <- fit_model(
#'   template_dir = template_dir,
#'   model_images_dir = model_images_dir,
#'   num_iters = 100,
#'   num_chains = 1,
#'   num_cores = 2,
#'   writer_indices = c(2, 5),
#'   doc_indices = c(7, 18)
#' )
#'
#' model <- drop_burnin(model = model, burn_in = 25)
#'
#' analysis <- analyze_questioned_documents(
#'   template_dir = template_dir,
#'   questioned_images_dir = questioned_images_dir,
#'   model = model,
#'   num_cores = 2
#' )
#' analysis$posterior_probabilities
#' }
#'
#' @keywords model
#'
#' @export
#' @md
fit_model <- function(template_dir,
                      model_images_dir,
                      num_iters,
                      num_chains = 1,
                      num_cores,
                      writer_indices,
                      doc_indices,
                      a = 2,
                      b = 0.25,
                      c = 2,
                      d = 2,
                      e = 0.5) {
  # process model training documents and save in template_dir > data > model_graphs
  message("Processing model documents...")
  process_batch_dir(
    input_dir = model_images_dir,
    output_dir = file.path(template_dir, "data", "model_graphs"),
    return_result = FALSE,
    transform_output = "document"
  )

  # get cluster assignments
  message("Getting cluster assignments for model documents...")
  model_clusters <- get_clusterassignment(
    template_dir = template_dir,
    input_type = "model",
    writer_indices = writer_indices,
    doc_indices = doc_indices,
    num_cores = num_cores
  )

  # format model data
  message("Getting the model data ready for RJAGS...")
  model_data <- format_model_data(
    model_clusters = model_clusters,
    writer_indices = writer_indices,
    doc_indices = doc_indices,
    a = a,
    b = b,
    c = c,
    d = d,
    e = e
  )

  rjags_data <- model_data$rjags_data

  # fit model with rjags
  message("Fitting the model with RJAGS")
  model <-
    rjags::jags.model(textConnection(model_wrapped_cauchy),
      data = rjags_data,
      n.chains = num_chains
    )

  # mcmc draws
  model <-
    rjags::coda.samples(model, c("pi", "gamma", "mu", "tau", "eta"), n.iter = num_iters)

  model <- list(
    "fitted_model" = model,
    "rjags_data" = model_data$rjags_data,
    "graph_measurements" = model_data$graph_measurements,
    "cluster_fill_counts" = model_data$cluster_fill_counts
  )
  saveRDS(model, file.path(template_dir, "data", "model.rds"))

  return(model)
}


#' drop_burnin
#'
#' `drop_burnin()` removes the burn-in from the MCMC draws.
#'
#' @param model A list of MCMC draws from a model fit with [`fit_model()`].
#' @param burn_in An integer number of starting iterations to drop from each MCMC chain.
#' @return A list of data frames of MCMC draws with burn-in dropped.
#'
#' @examples
#' model <- drop_burnin(model = example_model_1chain, burn_in = 25)
#' plot_trace(variable = "mu[1,2]", model = example_model_1chain)
#'
#' model <- drop_burnin(model = example_model_2chains, burn_in = 25)
#' plot_trace(variable = "mu[1,2]", model = example_model_2chains)
#'
#' @keywords model
#'
#' @export
#' @md
drop_burnin <- function(model, burn_in) {
  model$fitted_model <-
    lapply(model$fitted_model, function(x) {
      coda::as.mcmc(x[(burn_in + 1):coda::niter(x), ])
    })
  return(model)
}


#' about_variable
#'
#' `about_variable()` returns information about the model variable.
#'
#' @param variable A variable in the fitted model output by [`fit_model`]
#' @param model A fitted model created by [`fit_model`]
#' @return Text that explains the variable
#'
#' @examples
#' about_variable(
#'   variable = "mu[1,2]",
#'   model = example_model_1chain
#' )
#' about_variable(
#'   variable = "gamma[5]",
#'   model = example_model_2chains
#' )
#'
#' @keywords model
#'
#' @export
#' @md
about_variable <- function(variable, model) {
  all_vars <-
    names(as.data.frame(coda::as.mcmc(model$fitted_model[[1]])))
  if (!(variable %in% all_vars)) {
    stop(paste("The input variable", variable, "is not in the model."))
  }

  if (stringr::str_detect(variable, ",")) {
    # get variable, writer, and cluster
    split <- strsplit(variable, "\\[|,|\\]")
    var <- split[[1]][1]
    writer <- as.integer(split[[1]][2])
    cluster <- split[[1]][3]

    # pick text
    about <- switch(var,
      pi = "Pi is the cluster fill probability for writer w and cluster g",
      mu = "Mu is the location parameter of a wrapped-Cauchy distribution for writer w and cluster g",
      tau = "Tau is the scale parameter of a wrapped-Cauchy distribution for writer w and cluster g"
    )

    # replace writer
    writers <- unique(model$cluster_fill_counts$writer)
    about <-
      stringr::str_replace(about, " w ", paste0(" ID ", writers[writer], " "))

    # replace cluster
    about <- stringr::str_replace(about, " g", paste0(" ", cluster))
  } else {
    split <- strsplit(variable, "\\[|\\]")
    var <- split[[1]][1]
    cluster <- split[[1]][2]

    # pick text
    about <- switch(var,
      gamma = "Gamma is the mean cluster fill probability across all writers for cluster g",
      eta = "Eta is the mean, or the location parameter, of the hyper prior for mu for cluster g"
    )

    # replace cluster
    about <- stringr::str_replace(about, " g", paste0(" ", cluster))
  }
  return(about)
}


# NOT EXPORTED ------------------------------------------------------------

#' format_draws
#'
#' `format_draws()` formats the coda samples output by [`fit_model()`] into a
#' more convenient matrix.
#'
#' @param model A fitted model created by [`fit_model()`]
#' @return MCMC draws formatted into a list of dataframes
#'
#' @noRd
format_draws <- function(model) {
  # convert mcmc object to list of data frames
  draws_to_dataframe <- function(model_chain) {
    draws <- as.data.frame(coda::as.mcmc(model_chain))
    draws <- list(
      pis = draws[, grep(x = colnames(draws), pattern = "pi")],
      mus = draws[, grep(x = colnames(draws), pattern = "mu")],
      gammas = draws[, grep(x = colnames(draws), pattern = "gamma")],
      taus = draws[, grep(x = colnames(draws), pattern = "tau")],
      etas = draws[, grep(x = colnames(draws), pattern = "^eta")]
    )
    return(draws)
  }

  # make data frames for each chain and each variable group
  draws_list <-
    lapply(model$fitted_model, function(x) {
      draws_to_dataframe(x)
    })

  # combine data frames from different chains by variable group
  if (length(draws_list) > 1) {
    vars <- c("pis", "mus", "gammas", "taus", "etas")
    draws <- list()
    for (i in 1:length(vars)) {
      temp <- draws_list[[1]][[vars[i]]]
      for (j in 2:length(draws_list)) {
        temp <- rbind(temp, draws_list[[j]][[vars[i]]])
      }
      draws[[vars[i]]] <- temp
    }
  } else {
    draws <- draws_list[[1]]
  }

  return(draws)
}


#' calculate_wc_likelihood
#'
#' Calculate the Wrapped Cauchy likelihood function for x, mu, and tau.
#'
#' @param x Value
#' @param mu The location parameter
#' @param tau The scale parameter
#'
#' @return Value
#'
#' @noRd
calculate_wc_likelihood <- function(x, mu, tau) {
  (1 - tau^2) / (2 * pi * (1 + tau^2 - 2 * tau * cos(x - mu)))
}

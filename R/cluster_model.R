# The handwriter R package performs writership analysis of handwritten documents. 
# Copyright (C) 2021 Iowa State University of Science and Technology on behalf of its Center for Statistics and Applications in Forensic Evidence
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


# EXPORTED ----------------------------------------------------------------


#' Fit Model
#'
#' `fit_model()` fits a Bayesian hierarchical model to the model training data
#' in `model_docs` and draws samples from the model as Markov Chain Monte
#' Carlo (MCMC) estimates.
#'
#' @param main_dir A directory that contains a cluster template created by
#'   [`make_clustering_template()`]
#' @param model_docs A directory containing model training documents
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
#' main_dir <- "/path/to/main_dir"
#' model_docs <- "path/to/model_training_docs"
#' questioned_docs <- "path/to/questioned_docs"
#'
#' model <- fit_model(
#'   main_dir = main_dir,
#'   model_docs = model_docs,
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
#'   main_dir = main_dir,
#'   questioned_docs = questioned_docs,
#'   model = model,
#'   num_cores = 2
#' )
#' analysis$posterior_probabilities
#' }
#'
#' @export
#' @md
fit_model <- function(main_dir,
                      model_docs,
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
  # process model training documents and save in main_dir > data > model_graphs
  message("Processing model documents...")
  process_batch_dir(
    input_dir = model_docs,
    output_dir = file.path(main_dir, "data", "model_graphs")
  )

  # get cluster assignments
  message("Getting cluster assignments for model documents...")
  model_clusters <- get_clusterassignment(
    main_dir = main_dir,
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
  
  model_wrapped_cauchy <- "model {
    for(letter in 1:numletters){
      # numletters = num unique letters with measurements
      # use zeros trick to sample from wrapped-Cauchy distribution
      nll_datamodel[letter] = -log( (1-pow(tau[letterwriter[letter], lettercluster[letter]],2)) / (2*pi_constant*(1+pow(tau[letterwriter[letter], lettercluster[letter]],2)-2*tau[letterwriter[letter], lettercluster[letter]]*cos(pc_wrapped[letter]-mu[letterwriter[letter], lettercluster[letter]]))) ) + C
      zero_vec[letter] ~ dpois( nll_datamodel[letter] )
    }
    
    # Priors for wrapped cauchy
    for(g in 1:Gsmall){
      # g = cluster
      gamma[g] ~ dgamma(a, b) T(0.1,)
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
  saveRDS(model, file.path(main_dir, "data", "model.rds"))

  return(model)
}


#' Drop Burn-In
#'
#' `drop_burnin()` removes the burn-in from the Markov Chain Monte Carlo (MCMC) draws.
#'
#' @param model A list of MCMC draws from a model fit with [`fit_model()`].
#' @param burn_in An integer number of starting iterations to drop from each MCMC chain.
#' @return A list of data frames of MCMC draws with burn-in dropped.
#'
#' @examples
#' model <- drop_burnin(model = example_model, burn_in = 25)
#' plot_trace(variable = "mu[1,2]", model = example_model)
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


#' About Varialbe
#'
#' `about_variable()` returns information about the model variable.
#'
#' @param variable A variable in the fitted model output by [`fit_model()`]
#' @param model A fitted model created by [`fit_model()`]
#' @return Text that explains the variable
#'
#' @examples
#' about_variable(
#'   variable = "mu[1,2]",
#'   model = example_model
#' )
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

#' Get Credible Intervals
#'
#' In a model created with [`fit_model()`] the pi parameters are the estimate of
#' the true cluster fill count for a particular writer and cluster. The function
#' `get_credible_intervals()` calculates the credible intervals of the pi
#' parameters for each writer in the model.
#'
#' @param model A model output by [`fit_model()`]
#' @param interval_min The lower bound for the credible interval. The number
#'   must be between 0 and 1.
#' @param interval_max The upper bound for the credible interval. The number
#'   must be greater than `interval_min` and must be less than 1.
#' @return A list of data frames. Each data frame lists the credible intervals for a single writer.
#'
#' @examples
#' get_credible_intervals(model=example_model)
#' get_credible_intervals(model=example_model, interval_min=0.05, interval_max=0.95)
#'
#' @export
#' @md
get_credible_intervals <- function(model, interval_min = 0.05, interval_max = 0.95){
  pis <- get_pi_dataframes(model)
  writerIDs <- levels(model$rjags_data$docwriter)
  ci <- lapply(1:model$rjags_data$W, function(i) {get_credible_intervals_for_writer(
    model = model,
    writer = i,
    writerID = writerIDs[i],
    writer_pis = pis[[i]],
    interval_min = interval_min,
    interval_max = interval_max)})
  return(ci)
}

# Internal Functions ------------------------------------------------------


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


#' get_pi_dataframes
#'
#' Reformat the pi parameters from the model in individual data frames for each writer.
#'
#' @param model A model fitted with fit_model().
#'
#' @return A list of data frames
#'
#' @noRd
get_pi_dataframes <- function(model) {
  model$fitted_model <- format_draws(model = model)
  
  # add iters and writer columns
  niter <- nrow(model$fitted_model$pis)
  flat_pi <- cbind(iters = 1:niter, writer = NA, model$fitted_model$pis)
  
  # get a data frame of pis for a specific writer
  get_writer_pis <- function(flat_pi, writer){
    # select writer's columns in flat_pi
    writer_cols <- colnames(flat_pi)[grepl(paste0("pi\\[", writer, ","), colnames(flat_pi))]
    writer_pis <- flat_pi[,c("iters", "writer", writer_cols)]
    # add writer to data frame
    writer_pis$writer <- writer
    # rename columns so all writers have same column names
    colnames(writer_pis) <- c("iter", "writer", paste0("cluster_", 1:model$rjags_data$Gsmall))
    return(writer_pis)
  }
  writers <- 1:model$rjags_data$W
  pis <- lapply(writers, function(w) get_writer_pis(flat_pi, w))
  
  return(pis)
}

#' get_credible_intervals_for_writer
#'
#' Calculate the median and credible intervals for the pi parameters for a writer
#' 
#' @param model A model created with `fit_model`
#' @param writer The model sequentially numbers the writers. `writer` is the sequential writer number.
#' @param writerID The writer ID
#' @param writer_pis The formatted data frame of pi parameters for the writer created by `get_pi_dataframes`
#' @param interval_min The lower bound of the credible interval
#' @param interval_max The upper bound of the credible interval
#' @return A data frame
#'
#' @noRd
get_credible_intervals_for_writer <- function(model, 
                                              writer, 
                                              writerID, 
                                              writer_pis, 
                                              interval_min=0.025, 
                                              interval_max=0.975){
  
  df <- sapply(writer_pis[,-which(names(writer_pis) %in% c("iter", "writer"))], 
               function(x) stats::quantile(x,  probs = c(interval_min, 0.5, interval_max)), USE.NAMES = FALSE)
  df <- data.frame(quantile = row.names(df), df)
  df$writer <- writerID
  rownames(df) <- NULL
  return(df)
}

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
#' template_dir <- /path/to/template_directory
#' model_images_dir <- system.file("extdata/example_images/model_training_images",
#'   package = "handwriter")
#' questioned_images_dir <- system.file("extdata/example_images/questioned_images",
#'   package = "handwriter")
#'
#' model <- fit_model(
#'   template_dir = template_dir
#'   model_images_dir = model_images_dir,
#'   num_iters = 100,
#'   num_chains = 1,
#'   num_cores = 2,
#'   writer_indices = c(2,5),
#'   doc_indices = c(7,18)
#' )
#'
#' model <- drop_burnin(model = model, burn_in = 25)
#'
#' analysis <- analyze_questioned_documents(
#'   template_dir = template_dir
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
                      writer_indices, doc_indices,
                      a = 2, b = 0.25, c = 2, d = 2, e = 0.5) {
  # load cluster template
  message("Loading cluster template...")
  template <- readRDS(file.path(template_dir, "data", "template.rds"))

  # process model training documents and save in template_dir > data > model_graphs
  message("Processing model documents...")
  process_batch_dir(
    input_dir = model_images_dir,
    output_dir = file.path(template_dir, "data", "model_graphs"),
    return_result = TRUE,
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
    a = a, b = b, c = c, d = d, e = e
  )

  rjags_data <- model_data$rjags_data

  # fit model with rjags
  message("Fitting the model with RJAGS")
  model <- rjags::jags.model(textConnection(model_wrapped_cauchy), data = rjags_data, n.chains = num_chains)

  # mcmc draws
  model <- rjags::coda.samples(model, c("pi", "gamma", "mu", "tau", "eta"), n.iter = num_iters)

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
  model$fitted_model <- lapply(model$fitted_model, function(x) coda::as.mcmc(x[(burn_in + 1):coda::niter(x), ]))
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
  all_vars <- names(as.data.frame(coda::as.mcmc(model$fitted_model[[1]])))
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
    about <- stringr::str_replace(about, " w ", paste0(" ID ", writers[writer], " "))

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


#' analyze_questioned_documents
#'
#' `analyze_questioned_documents()` estimates the posterior probability of
#' writership for the questioned documents using MCMC draws from a hierarchical
#' model created with `fit_model()`.
#'
#' @param template_dir A directory that contains a cluster template created by [`make_clustering_templates`]
#' @param questioned_images_dir A directory containing questioned documents
#' @param model A fitted model created by [`fit_model`]
#' @param num_cores An integer number of cores to use for parallel processing
#'   with the `doParallel` package.
#' @param writer_indices A vector of start and stop characters for writer IDs in file names
#' @param doc_indices A vector of start and stop characters for document names in file names
#' @return A list of likelihoods, votes, and posterior probabilities of
#'   writership for each questioned document.
#'
#' @examples
#' \dontrun{
#' template_dir <- "/path/to/template"
#' questioned_images_dir <- "/path/to/questioned_images"
#' analysis <- analyze_questioned_documents(
#'   template_dir = template_dir
#'   questioned_images_dir = questioned_images_dir
#'   model = model,
#'   num_cores = 2,
#'   writer_indices = c(2,5),
#'   doc_indices = c(7,18)
#' )
#' analysis$posterior_probabilities
#' }
#'
#' @keywords model
#'
#' @export
#' @md
analyze_questioned_documents <- function(template_dir, questioned_images_dir, model, num_cores, writer_indices, doc_indices) {
  # process questioned documents
  message("Processing questioned documents...")
  questioned_proc_list <- process_batch_dir(
    input_dir = questioned_images_dir,
    output_dir = file.path(template_dir, "data", "questioned_graphs"),
    transform_output = "document"
  )

  # load template
  message("Loading cluster template...")
  template <- readRDS(file.path(template_dir, "data", "template.rds"))
  
  # get cluster assignments
  message("Getting cluster assignments for questioned documents...")
  questioned_clusters <- get_clusterassignment(
    template_dir = template_dir,
    input_type = "questioned",
    writer_indices = writer_indices,
    doc_indices = doc_indices,
    num_cores = num_cores
  )

  # format data
  message("Getting the questioned document data ready for the model...")
  questioned_data <- format_questioned_data(
    model = model,
    questioned_clusters = questioned_clusters,
    writer_indices = writer_indices,
    doc_indices = doc_indices
  )

  rjags_data <- model$rjags_data

  # convert mcmc objects into dataframes and combine chains
  model$fitted_model <- format_draws(model = model)

  # initialize
  niter <- nrow(model$fitted_model$pis)
  pis <- array(dim = c(niter, rjags_data$G, rjags_data$W)) # 3 dim array, a row for each mcmc iter, a column for each cluster, and a layer for each writer
  mus <- taus <- array(dim = c(niter, rjags_data$Gsmall, rjags_data$W)) # 3 dim array, a row for each mcmc iter, a column for each cluster, and a layer for each writer
  dmult <- dwc_sums <- data.frame(matrix(nrow = niter, ncol = rjags_data$W))
  layered_wc_params <- array(dim = c(niter, rjags_data$Gsmall, 2))
  ls <- list()

  # reshape variables
  flat_pi <- as.data.frame(cbind(iters = 1:niter, model$fitted_model$pis))
  flat_mus <- as.data.frame(cbind(iters = 1:niter, model$fitted_model$mus))
  flat_taus <- as.data.frame(cbind(iters = 1:niter, model$fitted_model$taus))
  for (i in 1:rjags_data$W) { # i is writer, j is graph
    for (j in 1:rjags_data$G) {
      pis[, j, i] <- flat_pi[1:niter, as.character(paste0("pi[", i, ",", j, "]"))]
    }
  }
  for (i in 1:rjags_data$W) { # i is writer, j is graph
    for (j in 1:rjags_data$Gsmall) {
      mus[, j, i] <- flat_mus[1:niter, as.character(paste0("mu[", i, ",", j, "]"))]
      taus[, j, i] <- flat_taus[1:niter, as.character(paste0("tau[", i, ",", j, "]"))]
    }
  }

  # start parallel processing
  my_cluster = parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(my_cluster)

  # list writers
  writers <- unique(questioned_data$graph_measurements$writer)

  # obtain posterior samples of model parameters
  message("Obtaining likelihood evaluations...")
  likelihood_evals <- foreach::foreach(m = 1:nrow(questioned_data$cluster_fill_counts)) %dopar% {
    # filter docs for current writer
    m_qdoc <- questioned_data$graph_measurements %>% dplyr::filter(writer == writers[m])
    m_cluster <- as.numeric(m_qdoc$cluster)

    # make a circular object
    m_pcrot <- circular::circular(m_qdoc$pc_wrapped, units = "radians", modulo = "2pi")

    if (length(m_cluster) > 0) {
      for (i in 1:rjags_data$W) { # i is writer, j is graph
        dmult[, i] <- mc2d::dmultinomial(x = questioned_data$cluster_fill_counts[m, -c(1, 2)], prob = pis[, , i], log = TRUE)
        layered_wc_params[, , 1] <- mus[, , i]
        layered_wc_params[, , 2] <- taus[, , i]
        temp <- sapply(1:niter, function(it) log(circular::dwrappedcauchy(x = circular::circular(m_pcrot), mu = circular::circular(layered_wc_params[it, m_cluster, 1]), rho = layered_wc_params[it, m_cluster, 2])))
        dwc_sums[, i] <- rowSums(t(temp))
      }
      nn <- dmult + dwc_sums + abs(max(colMeans(dmult + dwc_sums)))
    } else if (length(m_cluster) == 0) {
      for (i in 1:rjags_data$W) { # i is writer, k is graph
        dmult[, i] <- dmultinomial(x = questioned_data$cluster_fill_counts[m, -c(1, 2)], prob = pis[, , i], log = TRUE)
      }
      nn <- dmult + abs(max(colMeans(dmult)))
    }
    likelihoods <- as.data.frame(exp(nn) / rowSums(exp(nn)))
    colnames(likelihoods) <- paste0("known_writer_", writers)
    return(likelihoods)
  }
  names(likelihood_evals) <- paste0("w", questioned_data$cluster_fill_counts$writer, "_", questioned_data$cluster_fill_counts$doc)

  # tally votes
  message("Tallying votes...")
  votes <- lapply(likelihood_evals, function(y) {
    as.data.frame(t(apply(y, 1, function(x) floor(x / max(x)))))
  })
  votes <- lapply(votes, function(x) colSums(x))

  # calculate posterior probability of writership
  message("Calculating posterior probabilities...")
  posterior_probabilities <- lapply(votes, function(x) x / niter)
  posterior_probabilities <- as.data.frame(posterior_probabilities)
  posterior_probabilities <- cbind("known_writer" = rownames(posterior_probabilities), data.frame(posterior_probabilities, row.names = NULL)) # change rownames to column
  
  message("Saving results...")
  analysis <- list(
    "likelihood_evals" = likelihood_evals,
    "votes" = votes,
    "posterior_probabilities" = posterior_probabilities,
    "graph_measurements" = questioned_data$graph_measurements,
    "cluster_fill_counts" = questioned_data$cluster_fill_counts
  )
  saveRDS(analysis, file.path(template_dir, "data", "analysis.rds"))

  return(analysis)
}


#' analyze_questioned_documents2
#'
#' `analyze_questioned_documents2()` estimates the posterior probability of
#' writership for the questioned documents using MCMC draws from a hierarchical
#' model created with `fit_model()`.
#'
#' @param template_dir A directory that contains a cluster template created by [`make_clustering_templates`]
#' @param questioned_images_dir A directory containing questioned documents
#' @param model A fitted model created by [`fit_model`]
#' @param num_cores An integer number of cores to use for parallel processing
#'   with the `doParallel` package.
#' @param writer_indices A vector of start and stop characters for writer IDs in file names
#' @param doc_indices A vector of start and stop characters for document names in file names
#' @return A list of likelihoods, votes, and posterior probabilities of
#'   writership for each questioned document.
#'
#' @examples
#' \dontrun{
#' template_dir <- "/path/to/template"
#' questioned_images_dir <- "/path/to/questioned_images"
#' analysis <- analyze_questioned_documents2(
#'   template_dir = template_dir
#'   questioned_images_dir = questioned_images_dir
#'   model = model,
#'   num_cores = 2,
#'   writer_indices = c(2,5),
#'   doc_indices = c(7,18)
#' )
#' analysis$posterior_probabilities
#' }
#'
#' @keywords model
#'
#' @export
#' @md
analyze_questioned_documents2 <- function(template_dir, questioned_images_dir, model, num_cores, writer_indices, doc_indices) {
  # process questioned documents
  message("Processing questioned documents...")
  questioned_proc_list <- process_batch_dir(
    input_dir = questioned_images_dir,
    output_dir = file.path(template_dir, "data", "questioned_graphs"),
    transform_output = "document"
  )
  
  # load template
  message("Loading cluster template...")
  template <- readRDS(file.path(template_dir, "data", "template.rds"))
  
  # get cluster assignments
  message("Getting cluster assignments for questioned documents...")
  questioned_clusters <- get_clusterassignment(
    template_dir = template_dir,
    input_type = "questioned",
    writer_indices = writer_indices,
    doc_indices = doc_indices,
    num_cores = num_cores
  )
  
  # format data
  message("Getting the questioned document data ready for the model...")
  questioned_data <- format_questioned_data(
    model = model,
    questioned_clusters = questioned_clusters,
    writer_indices = writer_indices,
    doc_indices = doc_indices
  )
  
  rjags_data <- model$rjags_data
  
  # convert mcmc objects into dataframes and combine chains
  model$fitted_model <- format_draws(model = model)
  
  # initialize
  niter <- nrow(model$fitted_model$pis)
  pis <- array(dim = c(niter, rjags_data$G, rjags_data$W)) # 3 dim array: a row for each mcmc iter, a column for each cluster, and a layer for each writer
  mus <- taus <- array(dim = c(niter, rjags_data$Gsmall, rjags_data$W)) # 3 dim array: a row for each mcmc iter, a column for each cluster, and a layer for each writer
  dmult2 <- dwc_sums2 <- data.frame(matrix(nrow = niter, ncol = rjags_data$W))  # 2 dim array: a row for each mcmc iter, a column for each writer
  ls <- list()
  
  # reshape variables
  flat_pi <- as.data.frame(cbind(iters = 1:niter, model$fitted_model$pis))
  flat_mus <- as.data.frame(cbind(iters = 1:niter, model$fitted_model$mus))
  flat_taus <- as.data.frame(cbind(iters = 1:niter, model$fitted_model$taus))
  for (w in 1:rjags_data$W) { # w is writer
    for (k in 1:rjags_data$G) {  # k is cluster
      pis[, k, w] <- flat_pi[1:niter, as.character(paste0("pi[", w, ",", k, "]"))]
    }
  }
  for (w in 1:rjags_data$W) {  # w is writer
    for (k in 1:rjags_data$Gsmall) {  # k is cluster
      mus[, k, w] <- flat_mus[1:niter, as.character(paste0("mu[", w, ",", k, "]"))]
      taus[, k, w] <- flat_taus[1:niter, as.character(paste0("tau[", w, ",", k, "]"))]
    }
  }
  
  # start parallel processing
  my_cluster = parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(my_cluster)
  
  # list writers
  writers <- unique(questioned_data$graph_measurements$writer)
  
  # obtain posterior samples of model parameters
  message("Obtaining likelihood evaluations...")
  likelihood_evals <- foreach::foreach(d = 1:nrow(questioned_data$cluster_fill_counts)) %dopar% {  # d is document
    # filter docs for current writer
    qdoc2 <- questioned_data$graph_measurements %>% dplyr::filter(writer == writers[d])  # identical to m_qdoc
    
    # get cluster assignments
    qcluster2 <- as.numeric(qdoc2$cluster)  # identical to m_cluster
    
    # Get wrapped rotation angles. NOTE: Amy's original code represents the
    # eigenvectors as angles between -pi and pi (see format_questioned_data()
    # when it calls angle()). Then her wrapped angles are between -2pi and 2pi.
    # When she analyzed the questioned docs, she used the circular::circular to
    # make the angles circular objects and map the angles in the range (-2pi,0)
    # to (0, 2pi). She also used circular::dwrappedcauchy to evaluate wrapped
    # Cauchy likelihood function. But circular::dwrappedcauchy returns a warning
    # because she is feeding it a vector instead of a scalar for rho. To fix the
    # warning, and so that handwriter doesn't need to use the circular package,
    # I manually map angles from (-2pi,0) to (0,2pi) here and wrote a function
    # to evaluate the wrapped Cauchy likelihood later on.
    qpcrot2 <- ifelse(qdoc2$pc_wrapped < 0, qdoc2$pc_wrapped + 2*pi, qdoc2$pc_wrapped)  # equal to m_pcrot (not identical because m_pcrot is a circular object)
    
    for (w in 1:rjags_data$W) { # w is writer
      # doc-level
      dmult2[, w] <- mc2d::dmultinomial(x = questioned_data$cluster_fill_counts[d, -c(1, 2)], prob = pis[, , w], log = TRUE)
      # graph-level
      temp2 <- sapply(1:niter, function(iter) log(calculate_wc_likelihood(x=qpcrot2, mu=mus[iter, qcluster2, w], tau=taus[iter, qcluster2, w])))
      dwc_sums2[, w] <- rowSums(t(temp2))
    }
    nn <- dmult2 + dwc_sums2 + abs(max(colMeans(dmult2 + dwc_sums2)))
    likelihoods <- as.data.frame(exp(nn) / rowSums(exp(nn)))
    colnames(likelihoods) <- paste0("known_writer_", writers)
    return(likelihoods)
  }
  names(likelihood_evals) <- paste0("w", questioned_data$cluster_fill_counts$writer, "_", questioned_data$cluster_fill_counts$doc)
  
  # tally votes
  message("Tallying votes...")
  votes <- lapply(likelihood_evals, function(y) {
    as.data.frame(t(apply(y, 1, function(x) floor(x / max(x)))))
  })
  votes <- lapply(votes, function(x) colSums(x))
  
  # calculate posterior probability of writership
  message("Calculating posterior probabilities...")
  posterior_probabilities <- lapply(votes, function(x) x / niter)
  posterior_probabilities <- as.data.frame(posterior_probabilities)
  posterior_probabilities <- cbind("known_writer" = rownames(posterior_probabilities), data.frame(posterior_probabilities, row.names = NULL)) # change rownames to column
  
  message("Saving results...")
  analysis <- list(
    "likelihood_evals" = likelihood_evals,
    "votes" = votes,
    "posterior_probabilities" = posterior_probabilities,
    "graph_measurements" = questioned_data$graph_measurements,
    "cluster_fill_counts" = questioned_data$cluster_fill_counts
  )
  saveRDS(analysis, file.path(template_dir, "data", "analysis.rds"))
  
  return(analysis)
}


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
  draws_list <- lapply(model$fitted_model, function(x) draws_to_dataframe(x))

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

#' calculate_accuracy
#'
#' `calculate_accuracy` measures the accuracy of fitted model on a test set of
#' documents by calculating the average posterior probability assigned to the
#' true writer. Fit a model with [`fit_model`] and calculate posterior
#' probabilities of writership with [`analyze_questioned_documents`].
#'
#' @param analysis Writership analysis output by
#'   [`analyze_questioned_documents`]
#' @return The model's accuracy on the test set
#'
#' @export
#' @md
calculate_accuracy <- function(analysis){
  pp <- analysis$posterior_probabilities
  accuracy = sum(diag(as.matrix(pp[,-c(1)])))/nrow(pp)
  return(accuracy)
}


#' calculate_wc_likelihood
#'
#' Calculate the Wrapped Cauchy likelihood function for x, mu, and tau.
#' 
#' @param x 
#' @param mu 
#' @param tau 
#'
#' @return Value
#' 
#' @noRd
calculate_wc_likelihood <- function(x, mu, tau){
  (1 - tau^2)/(2*pi*(1 + tau^2 - 2*tau*cos(x-mu)))
}

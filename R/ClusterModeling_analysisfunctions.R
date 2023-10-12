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

#' Analyze Questioned Documents
#'
#' `analyze_questioned_documents()` estimates the posterior probability of
#' writership for the questioned documents using Markov Chain Monte Carlo (MCMC) draws from a hierarchical
#' model created with [`fit_model()`].
#'
#' @param template_dir A directory that contains a cluster template created by [`make_clustering_templates()`]
#' @param questioned_images_dir A directory containing questioned documents
#' @param model A fitted model created by [`fit_model()`]
#' @param num_cores An integer number of cores to use for parallel processing
#'   with the `doParallel` package.
#' @param num_graphs "All" or integer number of graphs to randomly select from each questioned document.
#' @param writer_indices A vector of start and stop characters for writer IDs in file names
#' @param doc_indices A vector of start and stop characters for document names in file names
#' @return A list of likelihoods, votes, and posterior probabilities of
#'   writership for each questioned document.
#'
#' @examples
#' \dontrun{
#' template_dir <- "/path/to/template_dir"
#' questioned_images_dir <- "/path/to/questioned_images"
#' analysis <- analyze_questioned_documents(
#'   template_dir = template_dir,
#'   questioned_images_dir = questioned_images_dir,
#'   model = model,
#'   num_cores = 2,
#'   num_graphs = "All",
#'   writer_indices = c(2, 5),
#'   doc_indices = c(7, 18)
#' )
#' analysis$posterior_probabilities
#' }
#'
#' @export
#' @md
analyze_questioned_documents <- function(template_dir, questioned_images_dir, model, num_cores, num_graphs = "All", writer_indices, doc_indices) {
  # bind global variables to fix check() note
  writer <- d <- NULL
  
  # process questioned documents
  message("Processing questioned documents...")
  questioned_proc_list <- process_batch_dir(
    input_dir = questioned_images_dir,
    output_dir = file.path(template_dir, "data", "questioned_graphs")
  )

  # load template
  message("Loading cluster template...")
  template <- readRDS(file.path(template_dir, "data", "template.rds"))

  # get cluster assignments
  message("Getting cluster assignments for questioned documents...")
  questioned_clusters <- get_clusterassignment(
    template_dir = template_dir,
    input_type = "questioned",
    num_graphs = num_graphs,
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
  dmult2 <- dwc_sums2 <- data.frame(matrix(nrow = niter, ncol = rjags_data$W)) # 2 dim array: a row for each mcmc iter, a column for each writer

  # reshape variables
  flat_pi <- as.data.frame(cbind(iters = 1:niter, model$fitted_model$pis))
  flat_mus <- as.data.frame(cbind(iters = 1:niter, model$fitted_model$mus))
  flat_taus <- as.data.frame(cbind(iters = 1:niter, model$fitted_model$taus))
  for (w in 1:rjags_data$W) { # w is writer
    for (k in 1:rjags_data$G) { # k is cluster
      pis[, k, w] <- flat_pi[1:niter, as.character(paste0("pi[", w, ",", k, "]"))]
    }
  }
  for (w in 1:rjags_data$W) { # w is writer
    for (k in 1:rjags_data$Gsmall) { # k is cluster
      mus[, k, w] <- flat_mus[1:niter, as.character(paste0("mu[", w, ",", k, "]"))]
      taus[, k, w] <- flat_taus[1:niter, as.character(paste0("tau[", w, ",", k, "]"))]
    }
  }

  # start parallel processing
  my_cluster <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(my_cluster)

  # list questioned writers
  qwriters <- unique(questioned_data$graph_measurements$writer)
  
  # list known writers
  kwriters <- unique(model$graph_measurements$writer)

  # obtain posterior samples of model parameters
  message("Obtaining likelihood evaluations...")
  likelihood_evals <- foreach::foreach(d = 1:nrow(questioned_data$cluster_fill_counts)) %dopar% { # d is document
    # filter docs for current writer
    qdoc2 <- questioned_data$graph_measurements %>% dplyr::filter(writer == qwriters[d]) # identical to m_qdoc

    # get cluster assignments
    qcluster2 <- as.numeric(qdoc2$cluster) # identical to m_cluster

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
    qpcrot2 <- ifelse(qdoc2$pc_wrapped < 0, qdoc2$pc_wrapped + 2 * pi, qdoc2$pc_wrapped) # equal to m_pcrot (not identical because m_pcrot is a circular object)

    for (w in 1:rjags_data$W) { # w is writer
      # doc-level
      dmult2[, w] <- mc2d::dmultinomial(x = questioned_data$cluster_fill_counts[d, -c(1, 2, 3)], prob = pis[, , w], log = TRUE)
      # graph-level
      temp2 <- sapply(1:niter, function(iter) log(calculate_wc_likelihood(x = qpcrot2, mu = mus[iter, qcluster2, w], tau = taus[iter, qcluster2, w])))
      dwc_sums2[, w] <- rowSums(t(temp2))
    }
    nn <- dmult2 + dwc_sums2 + abs(max(colMeans(dmult2 + dwc_sums2)))
    likelihoods <- as.data.frame(exp(nn) / rowSums(exp(nn)))
    colnames(likelihoods) <- paste0("known_writer_", kwriters)
    return(likelihoods)
  }
  names(likelihood_evals) <- questioned_data$cluster_fill_counts$docname

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


#' Calculate Accuracy
#'
#' Fit a model with [`fit_model()`] and calculate posterior probabilities of
#' writership with [`analyze_questioned_documents()`] of a set of test documents
#' where the ground truth is known. Then use `calculate_accuracy()` to measure
#' the accuracy of the fitted model on the test documents. Accuracy is calculated as
#' the average posterior probability assigned to the true writer.
#'
#' @param analysis Writership analysis output by
#'   [`analyze_questioned_documents`]
#' @return The model's accuracy on the test set as a number
#' 
#' @examples
#' # calculate the accuracy for example analysis performed on test documents and a model with 1 chain
#' calculate_accuracy(example_analysis_1chain)
#' 
#' # calculate the accuracy for example analysis performed on test documents and a model with 2 chains
#' calculate_accuracy(example_analysis_2chains)
#'
#' \dontrun{
#' template_dir <- "/path/to/template_dir"
#' test_images_dir <- "/path/to/test_images"
#' analysis <- analyze_questioned_documents(
#'   template_dir = template_dir,
#'   questioned_images_dir = test_images_dir,
#'   model = model,
#'   num_cores = 2,
#'   num_graphs = "All",
#'   writer_indices = c(2, 5),
#'   doc_indices = c(7, 18)
#' )
#' calculate_accuracy(analysis)
#' }
#' 
#' @export
#' @md
calculate_accuracy <- function(analysis) {
  pp <- analysis$posterior_probabilities
  accuracy <- sum(diag(as.matrix(pp[, -c(1)]))) / nrow(pp)
  return(accuracy)
}


#' Get Posterior Probabilities
#'
#' Get the posterior probabilities for questioned document analyzed with [`analyze_questioned_documents()`].
#'
#' @param analysis The output of [`analyze_questioned_documents()`]. If more than
#'   one questioned document was analyzed with this function, then the data frame
#'   analysis$posterior_probabilities lists the posterior probabilities for all
#'   questioned documents. [`get_posterior_probabilities()`] creates a data frame of the
#'   posterior probabilities for a single questioned document and sorts the known writers
#'   from the most likely to least likely to have written the questioned document.
#' @param questioned_doc The filename of the questioned document
#'
#' @return A data frame of posterior probabilities for the questioned document
#' @export
#'
#' @examples
#' get_posterior_probabilities(
#'   analysis = example_analysis_1chain,
#'   questioned_doc = "w0009_s03_pWOZ_r01.png"
#' )
#' get_posterior_probabilities(
#'   analysis = example_analysis_1chain,
#'   questioned_doc = "w0030_s03_pWOZ_r01.png"
#' )
#'
#' @md
get_posterior_probabilities <- function(analysis, questioned_doc) {
  pp <- analysis$posterior_probabilities[, c("known_writer", questioned_doc)]
  pp[order(pp[, 2], decreasing = TRUE), ]
}

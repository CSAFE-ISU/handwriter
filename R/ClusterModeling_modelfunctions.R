#' fit_model
#'
#' `fit_model()` fits a Bayesian hierarchical model to `model_training_data` and
#' draws samples from the model with MCMC.
#'
#' @param model_training_data A list of input data formatted with
#'   `format_model_data` for rjags
#' @param num_iters An integer number of iterations of MCMC.
#' @return A list of data frames of MCMC draws.
#'
#' @examples
#' draws <- fit_model(example_model_data$rjags_data, 1000)
#'
#' @keywords model
#'
#' @export
#' @md
fit_model <- function(model_training_data, num_iters) {
  # fit model with rjags
  m <- rjags::jags.model(textConnection(model_wrapped_cauchy), data = model_training_data, n.chains = 1)

  # mcmc draws
  fit <- rjags::coda.samples(m, c("theta", "gamma", "mu", "rho", "eta", "nll_datamodel", "nld_locationparam"), n.iter = num_iters)

  # format draws
  draws <- as.data.frame(fit[[1]])
  draws <- list(
    thetas = draws[, grep(x = colnames(draws), pattern = "theta")],
    mus = draws[, grep(x = colnames(draws), pattern = "mu")],
    gammas = draws[, grep(x = colnames(draws), pattern = "gamma")],
    rhos = draws[, grep(x = colnames(draws), pattern = "rho")],
    etas = draws[, grep(x = colnames(draws), pattern = "^eta")],
    nll_datamodel = draws[, grep(x = colnames(draws), pattern = "nll_datamodel")],
    nld_locationparam = draws[, grep(x = colnames(draws), pattern = "nld_locationparam")]
  )
  return(draws)
}


#' drop_burnin
#'
#' `drop_burnin()` removes the burn-in from the MCMC draws.
#'
#' @param draws A list of dataframes of MCMC draws created by `fit_model()`
#' @param burn_in An integer number of starting iterations of MCMC to drop.
#' @return A list of data frames of MCMC draws with burn-in dropped.
#'
#' @examples
#' draws <- fit_model(example_model_data$rjags_data, 4000)
#' draws <- drop_burnin(draws, 1000)
#'
#' @keywords model
#'
#' @export
#' @md
drop_burnin <- function(draws, burn_in) {
  # get number of draws
  num_iters <- nrow(draws$thetas)

  # remove burn-in
  draws$thetas <- draws$thetas[(burn_in + 1):num_iters, ]
  draws$mus <- draws$mus[(burn_in + 1):num_iters, ]
  draws$gammas <- draws$gammas[(burn_in + 1):num_iters, ]
  draws$rhos <- draws$rhos[(burn_in + 1):num_iters, ]
  draws$etas <- draws$etas[(burn_in + 1):num_iters, ]
  draws$nll_datamodel <- draws$nll_datamodel[(burn_in + 1):num_iters, ]
  draws$nld_locationparam <- draws$nld_locationparam[(burn_in + 1):num_iters, ]

  return(draws)
}


model_wrapped_cauchy <- "
model {
  for(letter in 1:numletters){                          /* numletters = num unique letters with measurements */
    nll_datamodel[letter] = -log( (1-pow(rho[letterwriter[letter], lettercluster[letter]],2)) / (2*pi*(1+pow(rho[letterwriter[letter], lettercluster[letter]],2)-2*rho[letterwriter[letter], lettercluster[letter]]*cos(pc_wrapped[letter]-mu[letterwriter[letter], lettercluster[letter]]))) ) + C
    zero_vec[letter] ~ dpois( nll_datamodel[letter] )
  }

  # Priors for wrapped cauchy
  for(g in 1:Gsmall){
    gamma[g] ~ dgamma(a, b)
    eta[g] ~ dunif(0,2*pi)
    for(w in 1:W){                                      /* W = num unique writers */
      mu[w,g]  ~ dunif(0,2*pi)
      nld_locationparam[w,g] = -log( (1-pow(e,2)) / (2*pi*(1+pow(e,2)-2*e*cos(mu[w,g]-eta[g]))) ) + C
      zero_mat[w,g] ~ dpois(nld_locationparam[w,g])
      rho[w,g] ~ dbeta(c,d)
    }
  }

  for (w in 1:W) {                                      /* W = num unique writers */
    theta[w,1:G] ~ ddirch(gamma[1:G] + 0.001)
  }

  for(d in 1:D) {                                       /* D = num unique documents */
    Y[d,1:G] ~ dmulti(theta[docwriter[d],1:G], docN[d])
  }

  # other values
  C    = 30   # for the ones's trick
  pi   = 3.14159
  pi_1 = -pi
}"


#' analyze_questioned_documents
#'
#' `analyze_questioned_documents()` estimates the posterior probability of
#' writership for the questioned documents using MCMC draws from a hierarchical
#' model created with `fit_model()`.
#'
#' @param model_training_data A list of input data formatted with
#'   `format_model_data` for rjags
#' @param draws MCMC draws created with `fit_model()`
#' @param questioned_data A list questioned documents's data formatted with
#'   `format_questioned_data()`.
#' @param num_cores An integer number of cores to use for parallel processing
#'   with the `doParallel` package.
#' @return A list of likelihoods, votes, and posterior probabilities of
#'   writership for each questioned document.
#'
#' @examples
#' \dontrun{
#' draws <- fit_model(model_training_data = example_model_data$rjags_data, num_iters = 4000)
#' draws <- drop_burnin(draws, burn_in = 1000)
#' draws <- analyze_questioned_documents(example_model_data,
#'                                       draws,
#'                                       example_questioned_data,
#'                                       num_cores = 4)
#' }
#'
#' @keywords model
#'
#' @export
#' @md
analyze_questioned_documents <- function(model_training_data, draws, questioned_data, num_cores) {

  # initialize
  niter <- nrow(draws$thetas)
  thetas <- array(dim = c(niter, model_training_data$rjags_data$G, model_training_data$rjags_data$W)) # 3 dim array, a row for each mcmc iter, a column for each cluster, and a layer for each writer
  mus <- rhos <- array(dim = c(niter, model_training_data$rjags_data$Gsmall, model_training_data$rjags_data$W)) # 3 dim array, a row for each mcmc iter, a column for each cluster, and a layer for each writer
  dmult <- dwc_sums <- data.frame(matrix(nrow = niter, ncol = model_training_data$rjags_data$W))
  layered_wc_params <- array(dim = c(niter, model_training_data$rjags_data$Gsmall, 2))
  ls <- list()

  # reshape variables
  flat_theta <- as.data.frame(cbind(iters = 1:niter, draws$thetas))
  flat_mus <- as.data.frame(cbind(iters = 1:niter, draws$mus))
  flat_rhos <- as.data.frame(cbind(iters = 1:niter, draws$rhos))
  for (i in 1:model_training_data$rjags_data$W) { # i is writer, j is graph
    for (j in 1:model_training_data$rjags_data$G) {
      thetas[, j, i] <- flat_theta[1:niter, as.character(paste0("theta[", i, ",", j, "]"))]
    }
  }
  for (i in 1:model_training_data$rjags_data$W) { # i is writer, j is graph
    for (j in 1:model_training_data$rjags_data$Gsmall) {
      mus[, j, i] <- flat_mus[1:niter, as.character(paste0("mu[", i, ",", j, "]"))]
      rhos[, j, i] <- flat_rhos[1:niter, as.character(paste0("rho[", i, ",", j, "]"))]
    }
  }

  # start parallel processing
  doParallel::registerDoParallel(cores = num_cores)

  # list writers
  writers <- unique(questioned_data$graph_measurements$writer)
  
  # obtain posterior samples of model parameters
  likelihood_evals <- foreach::foreach(m = 1:nrow(questioned_data$cluster_fill_counts)) %dopar% {
    # filter docs for current writer
    m_qdoc <- questioned_data$graph_measurements %>% dplyr::filter(writer == writers[m])
    m_cluster <- as.numeric(m_qdoc$cluster)

    # make a circular object
    m_pcrot <- circular::circular(m_qdoc$pc_wrapped, units = "radians", modulo = "2pi")

    if (length(m_cluster) > 0) {
      for (i in 1:model_training_data$rjags_data$W) { # i is writer, j is graph
        dmult[, i] <- mc2d::dmultinomial(x = questioned_data$cluster_fill_counts[m, -c(1, 2)], prob = thetas[, , i], log = TRUE)
        layered_wc_params[, , 1] <- mus[, , i]
        layered_wc_params[, , 2] <- rhos[, , i]
        dwc_sums[, i] <- rowSums(t(sapply(1:niter, function(it) log(circular::dwrappedcauchy(x = circular::circular(m_pcrot), mu = circular::circular(layered_wc_params[it, m_cluster, 1]), rho = layered_wc_params[it, m_cluster, 2])))))
      }
      nn <- dmult + dwc_sums + abs(max(colMeans(dmult + dwc_sums)))
    } else if (length(m_cluster) == 0) {
      for (i in 1:model_training_data$rjags_data$W) { # i is writer, j is graph
        dmult[, i] <- dmultinomial(x = questioned_data$cluster_fill_counts[m, -c(1, 2)], prob = thetas[, , i], log = TRUE)
      }
      nn <- dmult + abs(max(colMeans(dmult)))
    }
    likelihoods <- as.data.frame(exp(nn) / rowSums(exp(nn)))
    colnames(likelihoods) <- paste0("known_writer_", writers)
    return(likelihoods)
  }
  names(likelihood_evals) <- paste0("w", questioned_data$cluster_fill_counts$writer, "_", questioned_data$cluster_fill_counts$doc)
  
  # tally votes
  votes <- lapply(likelihood_evals, function(y) {as.data.frame(t(apply(y, 1, function(x) floor(x/max(x)))))})
  votes <- lapply(votes, function(x) colSums(x))
    
  # calculate posterior probability of writership
  posterior_probabilities <- lapply(votes, function(x) x / niter)
  posterior_probabilities <- as.data.frame(posterior_probabilities)
  posterior_probabilities <- cbind("known_writer" = rownames(posterior_probabilities), data.frame(posterior_probabilities, row.names=NULL))  # change rownames to column
  
  analysis <- list("likelihood_evals" = likelihood_evals, "votes" = votes, "posterior_probabilities" = posterior_probabilities)
  return(analysis)
}

#' fit_model
#'
#' `fit_model()` fits a Bayesian hierarchical model to `model_training_data` and
#' draws samples from the model with MCMC.
#'
#' @param model_data A list of input data formatted with
#'   `format_model_data` for rjags.
#' @param num_iters An integer number of iterations of MCMC.
#' @param num_chains An integer number of chains to use.
#' @return A list of data frames of MCMC draws.
#'
#' @examples
#' model <- fit_model(
#'   model_data = example_model_data,
#'   num_iters = 500,
#'   num_chains = 1
#' )
#' model <- drop_burnin(model = model, burn_in = 250)
#' analysis <- analyze_questioned_documents(
#'   model_data = example_model_data,
#'   model = model,
#'   questioned_data = example_questioned_data,
#'   num_cores = 2
#' )
#'
#' @keywords model
#'
#' @export
#' @md
fit_model <- function(model_data, num_iters, num_chains = 1) {
  rjags_data <- model_data$rjags_data

  # fit model with rjags
  model <- rjags::jags.model(textConnection(model_wrapped_cauchy), data = rjags_data, n.chains = num_chains)

  # mcmc draws
  model <- rjags::coda.samples(model, c("pi", "gamma", "mu", "tau", "eta", "theta", "nld_locationparam"), n.iter = num_iters)

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
#' @inherit fit_model examples
#'
#' @keywords model
#'
#' @export
#' @md
drop_burnin <- function(model, burn_in) {
  model <- lapply(model, function(x) coda::as.mcmc(x[(burn_in + 1):coda::niter(x), ]))
  return(model)
}


model_wrapped_cauchy <- "
model {
  for(letter in 1:numletters){                          /* numletters = num unique letters with measurements */
    theta[letter] = -log( (1-pow(tau[letterwriter[letter], lettercluster[letter]],2)) / (2*pi_constant*(1+pow(tau[letterwriter[letter], lettercluster[letter]],2)-2*tau[letterwriter[letter], lettercluster[letter]]*cos(pc_wrapped[letter]-mu[letterwriter[letter], lettercluster[letter]]))) ) + C
    zero_vec[letter] ~ dpois( theta[letter] )
  }

  # Priors for wrapped cauchy
  for(g in 1:Gsmall){
    gamma[g] ~ dgamma(a, b)
    eta[g] ~ dunif(0,2*pi_constant)
    for(w in 1:W){                                      /* W = num unique writers */
      mu[w,g]  ~ dunif(0,2*pi_constant)
      nld_locationparam[w,g] = -log( (1-pow(e,2)) / (2*pi_constant*(1+pow(e,2)-2*e*cos(mu[w,g]-eta[g]))) ) + C
      zero_mat[w,g] ~ dpois(nld_locationparam[w,g])
      tau[w,g] ~ dbeta(c,d)
    }
  }

  for (w in 1:W) {                                      /* W = num unique writers */
    pi[w,1:G] ~ ddirch(gamma[1:G] + 0.001)
  }

  for(d in 1:D) {                                       /* D = num unique documents */
    Y[d,1:G] ~ dmulti(pi[docwriter[d],1:G], docN[d])
  }

  # other values
  C    = 30   # for the ones's trick
  pi_constant   = 3.14159
  pi_1 = -pi_constant
}"


#' analyze_questioned_documents
#'
#' `analyze_questioned_documents()` estimates the posterior probability of
#' writership for the questioned documents using MCMC draws from a hierarchical
#' model created with `fit_model()`.
#'
#' @param model_data A list of input data formatted with
#'   `format_model_data` for rjags
#' @param model A list of MCMC draws from a model fit with [`fit_model()`].
#' @param questioned_data A list of questioned documents' data formatted with
#'   `format_questioned_data()`.
#' @param num_cores An integer number of cores to use for parallel processing
#'   with the `doParallel` package.
#' @return A list of likelihoods, votes, and posterior probabilities of
#'   writership for each questioned document.
#'
#' @inherit fit_model examples
#'
#' @keywords model
#'
#' @export
#' @md
analyze_questioned_documents <- function(model_data, model, questioned_data, num_cores) {
  rjags_data <- model_data$rjags_data

  # convert mcmc objects into dataframes and combine chains
  model <- format_draws(model = model)

  # initialize
  niter <- nrow(model$pis)
  pis <- array(dim = c(niter, rjags_data$G, rjags_data$W)) # 3 dim array, a row for each mcmc iter, a column for each cluster, and a layer for each writer
  mus <- taus <- array(dim = c(niter, rjags_data$Gsmall, rjags_data$W)) # 3 dim array, a row for each mcmc iter, a column for each cluster, and a layer for each writer
  dmult <- dwc_sums <- data.frame(matrix(nrow = niter, ncol = rjags_data$W))
  layered_wc_params <- array(dim = c(niter, rjags_data$Gsmall, 2))
  ls <- list()

  # reshape variables
  flat_pi <- as.data.frame(cbind(iters = 1:niter, model$pis))
  flat_mus <- as.data.frame(cbind(iters = 1:niter, model$mus))
  flat_taus <- as.data.frame(cbind(iters = 1:niter, model$taus))
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
      for (i in 1:rjags_data$W) { # i is writer, j is graph
        dmult[, i] <- mc2d::dmultinomial(x = questioned_data$cluster_fill_counts[m, -c(1, 2)], prob = pis[, , i], log = TRUE)
        layered_wc_params[, , 1] <- mus[, , i]
        layered_wc_params[, , 2] <- taus[, , i]
        dwc_sums[, i] <- rowSums(t(sapply(1:niter, function(it) log(circular::dwrappedcauchy(x = circular::circular(m_pcrot), mu = circular::circular(layered_wc_params[it, m_cluster, 1]), rho = layered_wc_params[it, m_cluster, 2])))))
      }
      nn <- dmult + dwc_sums + abs(max(colMeans(dmult + dwc_sums)))
    } else if (length(m_cluster) == 0) {
      for (i in 1:rjags_data$W) { # i is writer, j is graph
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
  votes <- lapply(likelihood_evals, function(y) {
    as.data.frame(t(apply(y, 1, function(x) floor(x / max(x)))))
  })
  votes <- lapply(votes, function(x) colSums(x))

  # calculate posterior probability of writership
  posterior_probabilities <- lapply(votes, function(x) x / niter)
  posterior_probabilities <- as.data.frame(posterior_probabilities)
  posterior_probabilities <- cbind("known_writer" = rownames(posterior_probabilities), data.frame(posterior_probabilities, row.names = NULL)) # change rownames to column

  analysis <- list("likelihood_evals" = likelihood_evals, "votes" = votes, "posterior_probabilities" = posterior_probabilities)
  return(analysis)
}

#' format_draws
#'
#' `format_draws()` formats the coda samples output by [`fit_model()`] into a
#' more convenient matrix.
#'
#' @param model A list of MCMC draws from a model fit with [`fit_model()`].
#' @return MCMC draws formatted into a list of dataframes.
#'
#' @noRd
format_draws <- function(model) {

  # convert mcmc object to list of data frames
  draws_to_dataframe <- function(model_chain) {
    draws <- as.data.frame(model_chain)
    draws <- list(
      pis = draws[, grep(x = colnames(draws), pattern = "pi")],
      mus = draws[, grep(x = colnames(draws), pattern = "mu")],
      gammas = draws[, grep(x = colnames(draws), pattern = "gamma")],
      taus = draws[, grep(x = colnames(draws), pattern = "tau")],
      etas = draws[, grep(x = colnames(draws), pattern = "^eta")],
      theta = draws[, grep(x = colnames(draws), pattern = "theta")],
      nld_locationparam = draws[, grep(x = colnames(draws), pattern = "nld_locationparam")]
    )
    return(draws)
  }

  # make data frames for each chain and each variable group
  draws_list <- lapply(model, function(x) draws_to_dataframe(x))

  # combine data frames from different chains by variable group
  if (length(draws_list) > 1) {
    vars <- c("pis", "mus", "gammas", "taus", "etas", "theta", "nld_locationparam")
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

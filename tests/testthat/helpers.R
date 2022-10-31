# enumerate the variables in fitted model created by fit_model()
list_model_variables <- function(num_writers, num_clusters) {
  etas <- gammas <- mus <- pis <- taus <- c()
  for (j in 1:num_clusters) {
    etas <- c(etas, paste0("eta[", j, "]"))
    gammas <- c(gammas, paste0("gamma[", j, "]"))
    for (i in 1:num_writers) {
      mus <- c(mus, paste0("mu[", i, ",", j, "]"))
      pis <- c(pis, paste0("pi[", i, ",", j, "]"))
      taus <- c(taus, paste0("tau[", i, ",", j, "]"))
    }
  }
  variables <- c(etas, gammas, mus, pis, taus)
  return(variables)
}

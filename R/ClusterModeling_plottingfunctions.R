#' Plot Cluster Fill Counts
#'
#' Create a barchart of the cluster fill counts for a particular writer and
#' document whose data is stored in formatted data.
#'
#' @param formatted_data A list of formatted data created by
#'   `format_model_data()` or `format_questioned_data`.
#' @param writerID An integer writer ID
#' @param docname A document name as a character string
#' @return ggplot barchart of cluster fill counts
#' 
#' @examples
#' plot_cluster_fill_counts(formatted_data = example_model_data, 
#'                          writerID = 9, 
#'                          docname = "s01_pWOZ_r1.")
#'
#' @export
#' @md
plot_cluster_fill_counts <- function(formatted_data, writerID, docname){
  
  counts <- formatted_data$cluster_fill_counts
  
  # filter for writer and document
  counts <- counts %>% dplyr::filter(writer == writerID, doc == docname)
  
  # make cluster and count columns
  counts <- counts %>% tidyr::pivot_longer(cols = -c(1,2), names_to = "cluster", values_to = "count")
  
  # plot
  p <- counts %>% ggplot2::ggplot(aes(x=cluster, y=count)) +
    geom_bar(stat = "identity") +
    theme_bw()
  return(p)
}


#' Plot Trace
#'
#' Create a traceplot for all chains for a single variable in an MCMC object
#' created by [`fit_model`]. If the model contains more than one chain, the
#' chains will be combined by pasting them together.
#'
#' @param model An MCMC object created by [`fit_model`]
#' @param variable The name of a variable in the MCMC object
#' @return ggplot line plot
#'
#' @examples
#' model <- fit_model(example_model_data, num_iters = 500, num_chains = 1)
#' plot_trace(model = model, variable = "theta[1,1]")
#'
#' @export
#' @md
plot_trace <- function(model, variable) {
  # format MCMC draws from fitted model
  model <- format_draws(model)
  
  # get parameter name from variable (E.g. theta[1,1] -> theta) and add an s on
  # the end
  param <- paste0(sub("\\[.*", "", variable), "s")
  
  # select data frame for variable
  p <- model[[param]]
  # add iteration column to data frame
  p["iteration"] <- 1:nrow(p)
  
  # rename variable column. ggplot doesn't like names with brackets
  colnames(p)[colnames(p) == variable] <- "y"
  
  # plot
  p <- p %>%
    ggplot2::ggplot(aes(x = iteration, y = y)) +
    geom_line() +
    labs(y = variable)
  
  return(p)
}


#' plot_posterior_probabilities
#'
#' Creates a tile plot of posterior probabilities of writership for each
#' questioned document and each known writer analyzed with
#' [`analyze_questioned_documents()`].
#'
#' @param analysis A named list of analysis results from [`analyze_questioned_documents()`].
#' @return A tile plot of posterior probabilities of writership.
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
#' plot_posterior_probabilities(analysis = analysis)
#'
#' @export
#' @md
plot_posterior_probabilities <- function(analysis) {
  # plot

  pp <- analysis$posterior_probabilities %>%
    tidyr::pivot_longer(
      cols = -known_writer,
      names_to = "questioned_document",
      values_to = "posterior_probability"
    )
  p <- pp %>%
    ggplot2::ggplot(aes(x = known_writer, y = questioned_document, fill = posterior_probability)) +
    geom_tile() +
    scale_fill_gradient2("Probability ", low = "grey90", midpoint = 0, high = "steelblue") +
    ylab("Questioned Document") +
    xlab("Known writer") +
    theme_bw() +
    theme(legend.position = "right", axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5))
  return(p)
}


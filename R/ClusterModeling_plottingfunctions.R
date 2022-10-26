#' Plot Cluster Fill Counts
#'
#' Plot the cluster fill counts for each document in `formatted_data`.
#'
#' @param formatted_data A list of formatted data created by
#'   `format_model_data()` or `format_questioned_data`.
#' @param facet `TRUE` uses `facet_wrap` to create a subplot for each
#'   writer. `FALSE` plots the data on a single plot.
#' @return ggplot plot of cluster fill counts
#'
#' @examples
#' plot_cluster_fill_counts(formatted_data = example_model_data, facet = TRUE)
#' plot_cluster_fill_counts(formatted_data = example_questioned_data, facet = FALSE)
#'
#' @export
#' @md
plot_cluster_fill_counts <- function(formatted_data, facet = FALSE){
  
  counts <- formatted_data$cluster_fill_counts
  
  # only one doc per writer?
  single_doc <- ifelse(length(unique(counts$writer)) == length(counts$writer), TRUE, FALSE)
  
  # make cluster and count columns
  counts <- counts %>% 
    tidyr::pivot_longer(cols = -c(1,2), names_to = "cluster", values_to = "count")
  
  # change writer and cluster to factor 
  counts <- counts %>%
    dplyr::mutate(cluster = factor(cluster),
                  writer = factor(writer))
  
  # plot
  if (single_doc){  # one doc per writer
    p <- counts %>% 
      ggplot2::ggplot(aes(x=cluster, y=count, color = writer)) +
      geom_point(position=position_dodge(width=0.75)) + 
      theme_bw()
  } else {  # at least one writer has more than one doc
    p <- counts %>% 
      ggplot2::ggplot(aes(x=cluster, y=count, color = writer)) +
      geom_line(position=position_dodge(width=0.75)) +
      geom_point(position=position_dodge(width=0.75)) + 
      theme_bw()
  }
  
  # facet (optional)
  if (facet){
    p <- p + facet_wrap(~writer)
  }
  
  return(p)
}


#' Plot Trace
#'
#' Create a traceplot for all chains for a single variable in an MCMC object
#' created by [`fit_model`]. If the model contains more than one chain, the
#' chains will be combined by pasting them together.
#'
#' @param model An MCMC object created by [`fit_model`]
#' @param model_data Data output by [`format_model_data`] and input to [`fit_model`]
#' @param variable The name of a variable in the MCMC object
#' @return ggplot line plot
#'
#' @examples
#' model <- fit_model(model_data = example_model_data, num_iters = 500, num_chains = 1)
#' plot_trace(model = model, model_data = example_model_data, variable = "pi[1,1]")
#' plot_trace(model = model, model_data = example_model_data, variable = "mu[2,3]")
#' plot_trace(model = model, model_data = example_model_data, variable = "gamma[1]")
#' plot_trace(model = model, model_data = example_model_data, variable = "tau[2,3]")
#' plot_trace(model = model, model_data = example_model_data, variable = "eta[2]")
#'
#' @export
#' @md
plot_trace <- function(model, model_data, variable) {
  # format MCMC draws from fitted model
  formatted_model <- format_draws(model)
  
  # get parameter name from variable (E.g. mu[4,5] -> mu) and add an s on
  # the end
  param <- sub("\\[.*", "", variable)
  params <- paste0(param, "s")
  
  # select data frame for variable
  p <- formatted_model[[params]]
  # add iteration column to data frame
  p["iteration"] <- 1:nrow(p)
  
  # rename variable column. ggplot doesn't like names with brackets
  colnames(p)[colnames(p) == variable] <- "y"
  
  # plot
  p <- p %>%
    ggplot2::ggplot(aes(x = iteration, y = y)) +
    geom_line() +
    labs(y = param,
         title = "Trace Plot",
         subtitle = about_variable(variable, model_data, model))
  
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


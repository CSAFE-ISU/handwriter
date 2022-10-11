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
#' \dontrun{
#' draws <- fit_model(example_model_data$rjags_data, num_iters = 4000)
#' draws <- drop_burnin(draws, 1000)
#' analysis <- analyze_questioned_documents(example_model_data, draws, example_questioned_data, num_cores = 4)
#' }
#' 
#' @export
#' @md
plot_posterior_probabilities <- function(analysis) {
  # plot

  pp <- analysis$posterior_probabilities %>% 
      tidyr::pivot_longer(cols = -known_writer, 
                          names_to = "questioned_document", 
                          values_to = "posterior_probability")
  p <- pp %>%
    ggplot2::ggplot(aes(x = known_writer, y = questioned_document, fill=posterior_probability)) + 
    geom_tile() +
    scale_fill_gradient2("Probability ", low="grey90", midpoint = 0, high="steelblue") +
    ylab("Questioned Document") + 
    xlab("Known writer") + 
    theme_bw()+
    theme(legend.position="right", axis.text.x = element_text(angle=90, hjust=0, vjust=.5))
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
#' \dontrun{
#' draws <- fit_model(example_model_data, num_iters = 1000, num_chains = 1)
#' plot_trace(model = draws, variable = "theta[1,1]")
#' }
#'
#' @export
#' @md
plot_trace <- function(model, variable){
  # format MCMC draws from fitted model
  draws <- format_draws(model)
  
  # get parameter name from variable (E.g. theta[1,1] -> theta) and add an s on
  # the end
  param <- paste0(sub("\\[.*", "", variable), "s")
  
  # select data frame for variable 
  p <- draws[[param]]
  # add iteration column to data frame
  p['iteration'] <- 1:nrow(p)
  
  # rename variable column. ggplot doesn't like names with brackets
  colnames(p)[colnames(p) == variable] <- "y"
  
  # plot
  p <- p %>% 
    ggplot2::ggplot(aes(x=iteration, y=y)) + 
    geom_line() + 
    labs(y=variable)
  
  return(p)
}

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


#' Plot Cluster Fill Counts
#'
#' Plot the cluster fill counts for each document in `formatted_data`.
#'
#' @param formatted_data Data created by [`format_template_data()`],
#'   [`fit_model()`], or [`analyze_questioned_documents()`]
#' @param facet `TRUE` uses `facet_wrap` to create a subplot for each writer.
#'   `FALSE` plots the data on a single plot.
#' @return ggplot plot of cluster fill counts
#'
#' @examples
#' # Plot cluster fill counts for template training documents
#' template_data <- format_template_data(example_cluster_template)
#' plot_cluster_fill_counts(formatted_data = template_data, facet = TRUE)
#'
#' # Plot cluster fill counts for model training documents
#' plot_cluster_fill_counts(formatted_data = example_model_1chain, facet = TRUE)
#'
#' # Plot cluster fill counts for questioned documents
#' plot_cluster_fill_counts(formatted_data = example_analysis_1chain, facet = FALSE)
#'
#' @export
#' @md
plot_cluster_fill_counts <- function(formatted_data, facet = FALSE) {
  # bind global variables to fix check() note
  writer <- cluster <- count <- doc <- NULL
  
  counts <- formatted_data$cluster_fill_counts

  # only one doc per writer?
  single_doc <- ifelse(length(unique(counts$writer)) == length(counts$writer), TRUE, FALSE)

  # make cluster and count columns
  counts <- counts %>%
    tidyr::pivot_longer(cols = -c(1, 2, 3), names_to = "cluster", values_to = "count")

  # change writer to factor
  counts <- counts %>%
    dplyr::mutate(writer = factor(writer))

  # plot
  if (single_doc) { # one doc per writer
    p <- counts %>%
      dplyr::mutate(cluster = as.integer(cluster)) %>% # allow lines between clusters
      ggplot2::ggplot(aes(x = cluster, y = count, color = writer)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = as.integer(unique(counts$cluster))) +
      theme_bw()
  } else { # at least one writer has more than one doc
    p <- counts %>%
      dplyr::mutate(cluster = as.integer(cluster)) %>% # allow lines between clusters
      ggplot2::ggplot(aes(x = cluster, y = count, group = interaction(writer, doc), color = writer)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = as.integer(unique(counts$cluster))) +
      theme_bw()
  }

  # facet (optional)
  if (facet) {
    p <- p + facet_wrap(~writer)
  }

  return(p)
}

#' Plot Cluster Fill Rates
#'
#' Plot the cluster fill rates for each document in `formatted_data`.
#'
#' @param formatted_data Data created by [`format_template_data()`],
#'   [`fit_model()`], or [`analyze_questioned_documents()`]
#' @param facet `TRUE` uses `facet_wrap` to create a subplot for each writer.
#'   `FALSE` plots the data on a single plot.
#' @return ggplot plot of cluster fill rates
#'
#' @examples
#' # Plot cluster fill rates for template training documents
#' template_data <- format_template_data(example_cluster_template)
#' plot_cluster_fill_rates(formatted_data = template_data, facet = TRUE)
#'
#' # Plot cluster fill rates for model training documents
#' plot_cluster_fill_rates(formatted_data = example_model_1chain, facet = TRUE)
#'
#' # Plot cluster fill rates for questioned documents
#' plot_cluster_fill_rates(formatted_data = example_analysis_1chain, facet = FALSE)
#'
#' @export
#' @md
plot_cluster_fill_rates <- function(formatted_data, facet = FALSE) {
  # bind globabl variables to fix check() note
  writer <- cluster <- rate <- doc <- NULL
  
  counts <- formatted_data$cluster_fill_counts
  
  # calculate rates
  rates <- counts
  rates[,-c(1,2,3)] <- rates[,-c(1,2,3)] / rowSums(rates[, -c(1,2,3)])
  
  # only one doc per writer?
  single_doc <- ifelse(length(unique(rates$writer)) == length(rates$writer), TRUE, FALSE)
  
  # make cluster and rate columns
  rates <- rates %>%
    tidyr::pivot_longer(cols = -c(1, 2, 3), names_to = "cluster", values_to = "rate")
  
  # change writer to factor
  rates <- rates %>%
    dplyr::mutate(writer = factor(writer))
  
  # plot
  if (single_doc) { # one doc per writer
    p <- rates %>%
      dplyr::mutate(cluster = as.integer(cluster)) %>% # allow lines between clusters
      ggplot2::ggplot(aes(x = cluster, y = rate, color = writer)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = as.integer(unique(rates$cluster))) +
      theme_bw()
  } else { # at least one writer has more than one doc
    p <- rates %>%
      dplyr::mutate(cluster = as.integer(cluster)) %>% # allow lines between clusters
      ggplot2::ggplot(aes(x = cluster, y = rate, group = interaction(writer, doc), color = writer)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = as.integer(unique(rates$cluster))) +
      theme_bw()
  }
  
  # facet (optional)
  if (facet) {
    p <- p + facet_wrap(~writer)
  }
  
  return(p)
}


#' Plot Trace
#'
#' Create a trace plot for all chains for a single variable of a fitted model
#' created by [`fit_model()`]. If the model contains more than one chain, the
#' chains will be combined by pasting them together.
#'
#' @param variable The name of a variable in the model
#' @param model A model created by [`fit_model()`]
#' @return A trace plot
#'
#' @examples
#' plot_trace(model = example_model_1chain, variable = "pi[1,1]")
#' plot_trace(model = example_model_1chain, variable = "mu[2,3]")
#'
#' @export
#' @md
plot_trace <- function(variable, model) {
  # bind global variable to fix check() note
  iteration <- y <- NULL
  
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
    labs(
      y = param,
      title = "Trace Plot",
      subtitle = about_variable(variable = variable, model = model)
    ) +
    theme_bw()

  return(p)
}

#' Plot Credible Intervals
#'
#' Plot credible intervals for the model's pi parameters that estimate the true writer
#' cluster fill counts.
#'
#' @param model A model created by [`fit_model()`]
#' @param interval_min The lower bound of the credible interval. It must be greater than zero and less than 1.
#' @param interval_max The upper bound of the credible interval. It must be greater than the interval minimum and less than 1.
#' @param facet `TRUE` uses `facet_wrap` to create a subplot for each writer.
#'   `FALSE` plots the data on a single plot.
#' @return ggplot plot credible intervals
#'
#' @examples
#' plot_credible_intervals(model = example_model_1chain)
#' plot_credible_intervals(model = example_model_1chain, facet = TRUE)
#'
#' @export
#' @md
plot_credible_intervals <- function(model, interval_min = 0.025, interval_max = 0.975, facet = FALSE) {
  # bind global variables to fix check() note
  cluster <- writer <- quantile <- `50%` <- NULL
  
  ci <- get_credible_intervals(
    model = model,
    interval_min = interval_min,
    interval_max = interval_max
  )
  
  # reshape and clean-up for plotting
  ci <- do.call(rbind, ci)
  colnames(ci) <- stringr::str_replace_all(colnames(ci), "_", " ")
  ci <- ci %>% tidyr::pivot_longer(cols = dplyr::starts_with("cluster"), names_to = "cluster", values_to = "pi")
  ci$writer <- as.factor(ci$writer)
  ci$cluster <- as.factor(ci$cluster)
  ci <- ci %>% tidyr::pivot_wider(names_from=quantile, values_from=pi)
  
  # plot
  p <- ci %>%
    ggplot(aes(x = cluster, y = `50%`, color = writer, group = writer)) +
    geom_line() +
    geom_errorbar(aes(ymin=!!sym(paste0(100*interval_min, "%")), ymax=!!sym(paste0(100*interval_max, "%")), group=writer), width=0.15, alpha=0.75) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(y = "median", color = "writer")

  # facet (optional)
  if (facet) {
    p <- p + facet_wrap(~writer)
  }

  return(p)
}


#' Plot Posterior Probabilities
#'
#' Creates a tile plot of posterior probabilities of writership for each
#' questioned document and each known writer analyzed with
#' [`analyze_questioned_documents()`].
#'
#' @param analysis A named list of analysis results from [`analyze_questioned_documents()`].
#' @return A tile plot of posterior probabilities of writership.
#'
#' @examples
#' plot_posterior_probabilities(analysis = example_analysis_1chain)
#'
#' @export
#' @md
plot_posterior_probabilities <- function(analysis) {
  # bind global variables to fix check() note
  known_writer <- posterior_probability <- questioned_document <- NULL
  
  # reshape
  pp <- analysis$posterior_probabilities %>%
    tidyr::pivot_longer(
      cols = -known_writer,
      names_to = "questioned_document",
      values_to = "posterior_probability"
    )
  
  # make x-axis labels the same order as known writers in pp
  writer_levels <- unique(pp$known_writer)
  pp$known_writer <- factor(pp$known_writer, levels=writer_levels)
  
  # create barchart for single qdoc and tileplot for multiple qdocs
  if (length(unique(pp$questioned_document)) == 1){
    p <- pp %>%
      ggplot(aes(x = known_writer, y = posterior_probability)) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(y="posterior probability") + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  } else {
    p <- pp %>%
      ggplot2::ggplot(aes(x = known_writer, y = questioned_document, fill = posterior_probability)) +
      geom_tile() +
      scale_fill_gradient2("Probability ", low = "grey90", midpoint = 0, high = "steelblue") +
      ylab("Questioned Document") +
      xlab("Known writer") +
      theme_bw() +
      theme(legend.position = "right", axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5))
  }
  return(p)
}

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
#' plot_cluster_fill_counts(formatted_data = example_model, facet = TRUE)
#'
#' # Plot cluster fill counts for questioned documents
#' plot_cluster_fill_counts(formatted_data = example_analysis, facet = FALSE)
#'
#' @export
#' @md
plot_cluster_fill_counts <- function(formatted_data, facet = TRUE) {
  # bind global variables to fix check() note
  writer <- cluster <- count <- doc <- NULL
  
  # get type of data - plot lines for model writers will be different colors but
  # plot lines for questioned documents will all be black with different
  # linetypes to prevent a questioned document from having the same line color
  # as one of the model writers
  if ("fitted_model" %in% names(formatted_data)){
    type <- "model"
  } else {
    type <- "questioned"
  }
  
  counts <- formatted_data$cluster_fill_counts
  
  # make cluster and count columns
  counts <- counts %>%
    tidyr::pivot_longer(cols = -c(1, 2, 3), names_to = "cluster", values_to = "count")
  
  # change writer to factor
  counts <- counts %>%
    dplyr::mutate(writer = factor(writer))
  
  # plot
  p <- switch(type,
              "model" = {
                counts %>%
                  dplyr::mutate(cluster = as.integer(cluster)) %>%  # allows lines between clusters
                  ggplot2::ggplot(aes(x = cluster, 
                                      y = count, 
                                      group = interaction(writer, doc), 
                                      color = writer)) + 
                  geom_line() +
                  geom_point() +
                  scale_x_continuous(breaks = as.integer(unique(counts$cluster))) +
                  theme_bw()
              },
              "questioned" = {
                counts %>%
                  dplyr::mutate(cluster = as.integer(cluster)) %>%  # allows lines between clusters
                  ggplot2::ggplot(aes(x = cluster, 
                                      y = count, 
                                      group = interaction(writer, doc), 
                                      linetype = writer)) + 
                  geom_line() +
                  geom_point() +
                  scale_x_continuous(breaks = as.integer(unique(counts$cluster))) +
                  theme_bw()
              })
  
  # facet (optional)
  if (facet) {
    p <- p + 
      facet_wrap(~writer) +
      # writer IDs appear above each box so hide the legend
      theme(legend.position = "none")
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
#' plot_cluster_fill_rates(formatted_data = example_model, facet = TRUE)
#'
#' # Plot cluster fill rates for questioned documents
#' plot_cluster_fill_rates(formatted_data = example_analysis, facet = FALSE)
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

#' Plot Writer Profiles
#'
#' Create a line plot of writer profiles for one or more documents.
#'
#' @param profiles A data frame of writer profiles created with
#'   \code{{get_writer_profiles}}.
#' @param color_by A column name. 'ggplot2' will always group by docname, but
#'   will use this column to assign colors.
#' @param ... Additional arguments passed to `ggplot2::facet_wrap`, such as
#'   `facets`, `nrow`, etc.
#'
#' @return A line plot
#'
#' @export
#'
#' @examples
#' \donttest{
#' docs <- system.file(file.path("extdata"), package = "handwriter")
#' profiles <- get_writer_profiles(docs, measure = "counts")
#' plot_writer_profiles(profiles)
#' 
#' profiles <- get_writer_profiles(docs, measure = "rates")
#' plot_writer_profiles(profiles)
#' }
#' 
#' @md
plot_writer_profiles <- function(profiles, color_by = "docname", ...) {
  # prevent note: "no visible binding for global variable"
  docname <- cluster <- rate <- .data <- NULL
  
  profiles <- profiles %>%
    tidyr::pivot_longer(
      cols = -tidyselect::any_of(c("docname", "writer", "doc", "total_graphs")),
      names_to = "cluster",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      docname = factor(docname),
      cluster = as.integer(stringr::str_replace(cluster, "cluster", ""))
    )
  
  # Add counts or rates label
  measure <- ifelse(max(profiles$value) > 1, "counts", "rates")
  colnames(profiles)[colnames(profiles) == "value"] <- measure
  
  p <- profiles %>%
    ggplot2::ggplot(ggplot2::aes(
      x = cluster,
      y = .data[[measure]],
      group = docname,
      color = .data[[color_by]]
    )) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::theme_bw()
  
  # optional. facet by writer or docname
  extra_args <- list(...)
  if (length(extra_args) > 0) {
    p <- p +
      ggplot2::facet_wrap(...)
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
#' plot_trace(model = example_model, variable = "pi[1,1]")
#' plot_trace(model = example_model, variable = "mu[2,3]")
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
#' plot_credible_intervals(model = example_model)
#' plot_credible_intervals(model = example_model, facet = TRUE)
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
#' plot_posterior_probabilities(analysis = example_analysis)
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

#' Plot Template Cluster Centers
#'
#' Plot the cluster centers of a cluster template created with
#' [`make_clustering_template`]. This function uses a K-Means type algorithm to
#' sort graphs from training documents into clusters. On each iteration of the
#' algorithm, it calculates the mean graph of each cluster and finds the graph
#' in each cluster that is closest to the mean graph. The graphs closest to the
#' mean graphs are used as the cluster centers for the next iteration.
#' Handwriter stores the cluster centers of a cluster template as graph
#' prototypes. A graph prototype consists of the starting and ending points of
#' each path in the graph, as well as and evenly spaced points along each path.
#' The prototype also stores the center point of the graph. All points are
#' represented as xy-coordinates and the center point is at (0,0).
#'
#' @param template A cluster template created with [`make_clustering_template`]
#' @param plot_graphs TRUE plots all graphs in each cluster in addition to the
#'   cluster centers. FALSE only plots the cluster centers.
#' @param size The size of the output plot
#'
#' @return A plot
#'
#' @examples
#' # plot cluster centers from example template
#' plot_cluster_centers(example_cluster_template)
#' plot_cluster_centers(example_cluster_template, plot_graphs = TRUE)
#'
#' @export
#' @md
plot_cluster_centers <- function(template, plot_graphs = FALSE, size=100) {
  
  cluster <- distance <- endx <- endy <- graph_num <- startx <- starty <- NULL
  
  build_segment_df <- function(proto, cluster = NULL){
    # a letter prototype consists of the start and end points of paths, points
    # evenly spaced along each path, and the center of the letter. The letter's
    # center might not be a point on the graph itself, so we don't use it in the
    # plot. For each path in the letter, we will plot a line between each point and
    # the next. Build a data frame of those line segments.
    
    num_paths <- dim(proto$pathEnds)[1]
    
    paths <- list()
    for (i in 1:num_paths){
      startx <- proto$pathEnds[i, 1]
      starty <- proto$pathEnds[i, 2]
      qrtsx <- proto$pathQuarters[i, seq(1, dim(proto$pathQuarters)[2]-1, 2)]
      qrtsy <- proto$pathQuarters[i, seq(2, dim(proto$pathQuarters)[2], 2)]
      endx <- proto$pathEnds[i, 3]
      endy <- proto$pathEnds[i, 4]
      pathx <- c(startx, qrtsx, endx)
      pathy <- c(starty, qrtsy, endy)
      
      # we will tell ggplot to draw a line between each point and the next point 
      # in the sequence so we make a data frame where each row contains the
      # xy-coordinates of the start and end point of a line
      start_pts <- data.frame(startx=pathx, starty=pathy)[1:(length(pathx)-1),]
      end_pts <- data.frame(endx=pathx, endy=pathy)[2:length(pathx),]
      df <- cbind(start_pts, end_pts)
      paths[[i]] <- df
    }
    df <- do.call(rbind, paths)
    if (!is.null(cluster)){
      df$cluster <- as.factor(paste("cluster", cluster))
    }
    
    return(df)
  }
  
  build_segments_by_cluster <- function(template, graphs) {
    # group graphs by cluster then build segment data frames
    graphs_by_cluster <- list()
    for (k in 1:template$K){
      graphs_k <- graphs[template$cluster==k]
      graphs_k <- lapply(1:length(graphs_k), function(i) build_segment_df(proto = graphs_k[[i]]))
      graphs_k_df <- do.call(rbind, graphs_k)
      graphs_k_df$cluster <- as.factor(paste("cluster", k))
      graphs_by_cluster[[k]] <- graphs_k_df
    }
    graphs_by_cluster <- do.call(rbind, graphs_by_cluster)
    return(graphs_by_cluster)
  }
  
  build_segments_for_centers <- function(template) {
    # build a data frame of segments for plotting the cluster centers
    centers <- template$centers
    centers_dfs <- lapply(1:length(centers), function(i) build_segment_df(proto = centers[[i]], cluster = i))
    centers_df <- do.call(rbind, centers_dfs)
    return(centers_df)
  }
  
  # check whether template contains template graphs
  if (plot_graphs && is.null(template$template_graphs)){
    plot_graphs <- FALSE
    message("The cluster template does not contain the training graphs. Only the cluster centers will be plotted.")
  }
  
  if (plot_graphs) {
    graphs <- template$template_graphs
    # convert graphs to graph prototypes for easier plotting. Graph prototypes
    # place the center of the graph at (0, 0) so the prototypes can be easily
    # centered. The graph images would need to be centered before we can plot
    # the clusters from the graph images directly.
    graphs <- lapply(graphs, graphToPrototype)
    graphs_by_cluster <- build_segments_by_cluster(template, graphs)
  } 
  
  centers_df <- build_segments_for_centers(template)

  plot_items <- function(plot_graphs) {
    # add plot elements to a list so that plotting the graphs is controlled by
    # an if statement. See Hadley's book
    # https://ggplot2-book.org/programming.html#multiple-components.
    list(
      if (plot_graphs) {ggplot2::geom_segment(data=graphs_by_cluster, aes(x=startx, y=starty, xend=endx, yend=endy), color = "grey30", alpha=0.05)},
      ggplot2::geom_segment(aes(x=startx, y=starty, xend=endx, yend=endy), color = "orange"),
      xlim(-size, size),
      ylim(-size, size),
      theme_void(),
      facet_wrap(~cluster)
    )
  }
  p <- centers_df %>% ggplot2::ggplot() + 
    plot_items(plot_graphs)
  
  return(p)
}

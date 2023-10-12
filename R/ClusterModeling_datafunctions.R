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


#' Format Template Data
#'
#' `format_template_data()` formats the template data for use with
#' [`plot_cluster_fill_counts()`]. The output is a list that contains a data frame
#' called `cluster_fill_counts`.
#'
#' @param template A single cluster template created by
#'   [`make_clustering_templates()`]
#' @return List that contains the cluster fill counts
#'
#' @examples
#' template_data <- format_template_data(template = example_cluster_template)
#' plot_cluster_fill_counts(formatted_data = template_data, facet = TRUE)
#'
#' @export
#' @md
format_template_data <- function(template) {
  writer <- doc <- cluster <- count <- NULL
  
  # make dataframe
  counts <- data.frame("writer" = template$writers, "doc" = template$doc, "cluster" = template$cluster)

  # get cluster fill counts
  counts <- counts %>%
    dplyr::group_by(writer, doc, cluster) %>%
    dplyr::summarize(count = dplyr::n())

  # make integer
  counts <- counts %>%
    dplyr::mutate(writer = as.integer(writer))

  # make a column for each cluster
  counts <- counts %>%
    tidyr::pivot_wider(names_from = cluster, values_from = count, values_fill = 0)

  # sort cluster columns
  sorted <- as.character(sort(unique(as.integer(colnames(counts[, -c(1, 2)])))))
  if ("-1" %in% colnames(counts)){  # outliers
    sorted <- sorted[sorted != -1] 
    counts <- cbind(counts[, c(1, 2)], counts[, "-1"], counts[, sorted])
  } else {  # no outliers
    counts <- cbind(counts[, c(1, 2)], counts[, sorted])
  }
  
  # make list
  data <- list("cluster_fill_counts" = counts)

  return(data)
}


# Internal Functions ------------------------------------------------------


#' Format Model Data
#'
#' `format_model_data()` formats the data need for the rjags model.
#'
#' @param model_clusters Data frame of cluster assignments for a set of model
#'   training documents created by `get_clusterassignment()`
#' @param writer_indices Vector of start and end indices for the writer id in
#'   the document names.
#' @param doc_indices Vector of start and end indices for the document id in the
#'   document names.
#' @param a Scalar
#' @param b Scalar
#' @param c Scalar
#' @param d Scalar
#' @param e Scalar
#' @return List of data formatted for rjags.
#'
#' @noRd
format_model_data <- function(model_clusters, writer_indices, doc_indices, a = 2, b = 0.25, c = 2, d = 2, e = 0.5) {
  # bind global variable to fix
  cluster <- NULL
  
  graph_measurements <- model_clusters 
  
  # if clusters aren't numbered sequentially, relabel them
  if (length(unique(graph_measurements$cluster)) < max(graph_measurements$cluster)) {
    graph_measurements <- graph_measurements %>% dplyr::rename("old_cluster" = cluster)
    cluster_lookup <- data.frame("old_cluster" = sort(unique(graph_measurements$old_cluster)), "cluster" = 1:length(unique(graph_measurements$old_cluster)))
    graph_measurements <- graph_measurements %>% dplyr::left_join(cluster_lookup, by = "old_cluster")
  }

  # get cluster fill counts ----
  # count number of graphs in each cluster for each writer
  cluster_fill_counts <- get_cluster_fill_counts(graph_measurements[, c("docname", "writer", "doc", "cluster")])

  # format data for rjags ----
  rjags_data <- list(
    Y = cluster_fill_counts[, -c(1, 2, 3)], # multinomial data
    G = ncol(cluster_fill_counts[, -c(1, 2, 3)]), # number of clusters (40)
    D = nrow(cluster_fill_counts[, -c(1, 2, 3)]), # total number of documents
    W = length(unique(cluster_fill_counts$writer)), # number of unique writers
    # docwise
    docN = as.integer(apply(cluster_fill_counts[, -c(1, 2, 3)], FUN = sum, MARGIN = 1)), # number of letters in each doc, e.g. N[1] = 354
    docwriter = as.integer(as.factor(cluster_fill_counts$writer)), # vector of writers for each document numbered 1,2,...,W
    # letterwise
    zero_vec = rep(0, times = length(graph_measurements$pc_wrapped)),
    Gsmall = length(unique(graph_measurements$cluster)), # number of clusters (20)
    numletters = length(graph_measurements$pc_wrapped), # total number of letters
    pc_wrapped = graph_measurements$pc_wrapped, # principal component rotation observations
    letterwriter = as.integer(as.factor(graph_measurements$writer)), # vector of writers for each letter
    lettercluster = as.integer(graph_measurements$cluster), # vector of cluster assignments, one for each letter
    zero_mat = matrix(0, nrow = length(unique(cluster_fill_counts$writer)), ncol = length(unique(graph_measurements$cluster))),
    a = a, b = b, c = c, d = d, e = e
  )

  data <- list(
    "graph_measurements" = graph_measurements,
    "cluster_fill_counts" = cluster_fill_counts,
    "rjags_data" = rjags_data
  )
  return(data)
}


#' Format Questioned Data
#'
#' `format_questioned_data()` formats the questioned data for analysis with the
#' hierarchical model.
#'
#' @param model A fitted model created by [`fit_model`]
#' @param questioned_clusters Data frame of cluster assignments for a set of
#'   questioned documents created by `get_clusterassignment()`
#' @param writer_indices Vector of start and end indices for the writer id in
#'   the document names.
#' @param doc_indices Vector of start and end indices for the document id in the
#'   document names.
#' @return List of data formatted analysis.
#'
#' @noRd
format_questioned_data <- function(model, questioned_clusters, writer_indices, doc_indices) {
  # bind global variable to fix check() note
  old_cluster <- cluster <- NULL
  
  graph_measurements <- questioned_clusters

  # if model clusters were relabeled, relabel the questioned clusters
  if (any(names(model$graph_measurements) == "old_cluster")) {
    # make lookup table from model cluster data
    cluster_lookup <- model$graph_measurements %>%
      dplyr::select(old_cluster, cluster) %>%
      dplyr::distinct()
    # store clusters as old clusters
    graph_measurements <- graph_measurements %>%
      dplyr::rename("old_cluster" = cluster)
    # get new cluster labels
    graph_measurements <- graph_measurements %>%
      dplyr::left_join(cluster_lookup, by = "old_cluster")
  }

  # get cluster fill counts ----
  # count number of graphs in each cluster for each writer
  cluster_fill_counts <- get_cluster_fill_counts(graph_measurements[, c("docname", "writer", "doc", "cluster")])
  
  # check for missing clusters
  if (ncol(cluster_fill_counts) < ncol(model$cluster_fill_counts)){
    # zero data frame
    full_cluster_fill_counts <- as.data.frame(matrix(0, nrow = nrow(cluster_fill_counts), ncol = ncol(model$cluster_fill_counts)))
    # fill column names
    colnames(full_cluster_fill_counts) <- colnames(model$cluster_fill_counts)
    # fill writers and docs
    full_cluster_fill_counts$writer <- cluster_fill_counts$writer
    full_cluster_fill_counts$doc <- cluster_fill_counts$doc
    # add missing columns
    full_cluster_fill_counts <- dplyr::left_join(cluster_fill_counts, full_cluster_fill_counts) %>% 
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(.x, 0)))
    # sort columns
    cols <- c(colnames(full_cluster_fill_counts[, c(1, 2)]), sort(as.numeric(colnames(full_cluster_fill_counts[, -c(1, 2)]))))
    full_cluster_fill_counts <- full_cluster_fill_counts[, cols]
    # rename
    cluster_fill_counts <- full_cluster_fill_counts
  }
  
  data <- list(
    "graph_measurements" = graph_measurements,
    "cluster_fill_counts" = cluster_fill_counts
  )
  return(data)
}


#' get_cluster_fill_counts
#'
#' `get_cluster_fill_counts()` creates a data frame that shows the number of
#' graphs in each cluster for each input document.
#'
#' @param df A data frame with columns `writer`, `doc`, and `cluster`. Each
#'   row corresponding to a graph and lists the writer of that graph, the document
#'   from which the graph was obtained, and the cluster to which that graph is assigned.
#' @return A dataframe of cluster fill counts for each document in the input data frame.
#'
#' @noRd
get_cluster_fill_counts <- function(df) {
  docname <- writer <- doc <- cluster <- n <- NULL
  
  # count number of graphs in each cluster for each writer
  cluster_fill_counts <- df %>%
    dplyr::group_by(docname, writer, doc, cluster) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(n = as.integer(n)) %>%
    tidyr::pivot_wider(names_from = cluster, values_from = n, values_fill = 0)

  # sort columns
  cols <- c(colnames(cluster_fill_counts[, c(1, 2, 3)]), sort(as.numeric(colnames(cluster_fill_counts[, -c(1, 2, 3)]))))
  cluster_fill_counts <- cluster_fill_counts[, cols]

  return(cluster_fill_counts)
}


#' angle
#'
#' `angle()` gives a value in (-pi,pi), where negative values come from unit vectors below the x axis (kinda weird/not traditional)
#' https://stackoverflow.com/questions/1897704/angle-between-two-vectors-in-r
#'
#' @param N a vector
#' @param M a vector
#' @return an angle value in (-pi,pi)
#'
#' @noRd
angle <- function(N, M) {
  theta <- atan2(N[2], N[1]) - atan2(M[2], M[1])
  ifelse(theta > 0, as.numeric(theta), theta + pi)
  return(theta)
}

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

#' Get Cluster Fill Counts
#'
#' `get_cluster_fill_counts()` creates a data frame that shows the number of
#' graphs in each cluster for each input document.
#'
#' @param df A data frame of cluster assignments from `get_clusters_batch`. 
#'   The data frame has columns `docname` and `cluster`. Each row
#'   corresponds to a graph and lists the document from which the graph was
#'   obtained and the cluster to which that graph is assigned. Optionally, the
#'   data frame might also have `writer` and `doc` columns. If present, `writer`
#'   lists the writer ID of each document and `doc` is an identifier to
#'   distinguish between different documents from the same writer.
#' @return A dataframe of cluster fill counts for each document in the input
#'   data frame.
#'
#' @examples
#' docname <- c(rep("doc1", 20), rep("doc2", 20), rep("doc3", 20))
#' writer <- c(rep(1, 20), rep(2, 20), rep(3, 20))
#' doc <- c(rep(1, 20), rep(2, 20), rep(3, 20))
#' cluster <- sample(3, 60, replace = TRUE)
#' df <- data.frame(docname, writer, doc, cluster)
#' get_cluster_fill_counts(df)
#'
#' @export
#' @md
get_cluster_fill_counts <- function(df) {
  docname <- writer <- doc <- cluster <- n <- NULL

  # count number of graphs in each cluster for each writer
  cluster_fill_counts <- df %>%
    dplyr::group_by(dplyr::pick(tidyselect::any_of(c("docname", "writer", "doc", "cluster")))) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(n = as.integer(n)) %>%
    tidyr::pivot_wider(names_from = cluster, values_from = n, values_fill = 0) %>%
    dplyr::select(tidyselect::any_of(c("docname", "writer", "doc")), tidyselect::everything())

  return(cluster_fill_counts)
}

#' Get Cluster Fill Rates
#'
#' `get_cluster_fill_rates()` creates a data frame that shows the proportion of
#' graphs assigned to each cluster in a cluster template.
#'
#' @param df A data frame of cluster assignments from `get_clusters_batch`. 
#'   The data frame has columns `docname` and `cluster`. Each row
#'   corresponds to a graph and lists the document from which the graph was
#'   obtained and the cluster to which that graph is assigned. Optionally, the
#'   data frame might also have `writer` and `doc` columns. If present, `writer`
#'   lists the writer ID of each document and `doc` is an identifier to
#'   distinguish between different documents from the same writer.
#'
#' @return A data frame of cluster fill rates.
#'
#' @export
#'
#' @examples
#' docname <- c(rep("doc1", 20), rep("doc2", 20), rep("doc3", 20))
#' writer <- c(rep(1, 20), rep(2, 20), rep(3, 20))
#' doc <- c(rep(1, 20), rep(2, 20), rep(3, 20))
#' cluster <- sample(3, 60, replace = TRUE)
#' df <- data.frame(docname, writer, doc, cluster)
#' rates <- get_cluster_fill_rates(df)
#'
#' @md
get_cluster_fill_rates <- function(df) {
  counts <- get_cluster_fill_counts(df = df)
  
  # get label columns. docname is required for input data frames but writer and
  # doc are optional.
  label_cols <- counts %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::any_of(c("docname", "writer", "doc")))

  # drop label columns and calculate cluster fill rates: each row sums to 1.
  df_clusters_only <- counts %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::any_of(c("docname", "writer", "doc")))
  df_clusters_only <- as.matrix(df_clusters_only)
  total_graphs <- rowSums(df_clusters_only)
  if (length(total_graphs) == 1) {
    cfr <- (1 / total_graphs) * df_clusters_only
  } else {
    cfr <- diag(1 / total_graphs) %*% df_clusters_only
  }

  # add missing clusters
  missing_labels <- setdiff(1:40, colnames(cfr))
  if (length(missing_labels) > 0) {
    missing <- lapply(missing_labels, function(k) data.frame(k = rep(0, nrow(cfr))))
    missing <- do.call(cbind, missing)
    colnames(missing) <- missing_labels
    cfr <- cbind(cfr, missing)
    # sort columns numerically
    cfr <- cfr[as.character(sort(as.numeric(colnames(cfr))))]
  }

  # add "cluster" to column names
  colnames(cfr) <- paste0("cluster", colnames(cfr))

  # check all rows sum to 1 (within machine precision)
  if (!all.equal(rep(1, nrow(cfr)), rowSums(cfr), tolerance = sqrt(.Machine$double.eps))) {
    stop("One or more rows does not sum to 1 (within machine precision).")
  }

  # add label columns and total_graphs column
  cfr <- cbind(label_cols, data.frame(total_graphs = total_graphs), cfr)

  return(cfr)
}

#' Estimate Writer Profiles
#'
#' Estimate writer profiles from handwritten documents scanned and saved as PNG
#' files. Each file in `input_dir` is split into component shapes called graphs
#' with [`process_batch_dir`]. Then the graphs are sorted into clusters with
#' similar shapes using the cluster `template` and [`get_clusters_batch`]. An
#' estimate of the writer profile for a document is the proportion of graphs
#' from that document assigned to each of the clusters in `template`. The writer
#' profiles are estimated by running [`get_cluster_fill_counts`]. If `measure`
#' is `counts` than the cluster fill counts are returned. If `measure` is
#' `rates` than [`get_cluster_fill_rates`] is run and the cluster fill rates are
#' returned.
#'
#' The functions [`process_batch_dir`] and [`get_clusters_batch`] take upwards
#' of 30 seconds per document and the results are saved to RDS files in
#' `project_dir` > graphs and `project_dir` > clusters, respectively. If
#' `project_dir` is NULL than the results are saved to the temporary directory
#' and deleted before the function terminates.
#'
#' @param input_dir A filepath to a folder containing one or more handwritten
#'   documents, scanned and saved as PNG file(s).
#' @param measure A character string: either `counts` or `rates`. `counts`
#'   returns the cluster fill counts, I.e., the number of graphs assigned to
#'   each cluster. `rates` returns the cluster fill rates, I.e., the proportion
#'   of graphs assigned to each cluster.
#' @param template Optional. A cluster template created with
#'   [`make_clustering_template`]. The default is `templateK40`.
#' @param num_cores An integer number greater than or equal to 1 of cores to use
#'   for parallel processing.
#' @param writer_indices A vector of start and stop characters for writer IDs in file names
#' @param doc_indices A vector of start and stop characters for document names in file names
#' @param output_dir Optional. A filepath to a folder to save the RDS files
#'   created by [`process_batch_dir`] and [`get_clusters_batch`]. If no folder
#'   is supplied, the RDS files will be saved to the temporary directory and
#'   then deleted before the function terminates.
#'
#' @return A data frame
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
get_writer_profiles <- function(
    input_dir,
    measure = "counts",
    num_cores = 1,
    template = templateK40,
    writer_indices = NULL,
    doc_indices = NULL,
    output_dir = NULL) {
  
  # Check measure
  if (!(measure %in% c("counts", "rates"))) {
    stop("measure must be 'counts' or 'rates'")
  }
  
  # Use tempdir if output_dir is NULL
  if (is.null(output_dir)) {
    output_dir <- file.path(tempdir(), "writer_profiles")
    create_dir(output_dir)
  }

  process_batch_dir(
    input_dir = input_dir,
    output_dir = file.path(output_dir, "graphs")
  )

  clusters <- get_clusters_batch(
    template = template,
    input_dir = file.path(output_dir, "graphs"),
    output_dir = file.path(output_dir, "clusters"),
    writer_indices = writer_indices,
    doc_indices = doc_indices,
    num_cores = num_cores,
    save_master_file = FALSE
  )
  
  if (measure == "counts") {
    profiles <- get_cluster_fill_counts(clusters)
  } else if (measure == "rates") {
    profiles <- get_cluster_fill_rates(clusters)
  } else {
    # Redundant with previous check
    stop("measure must be 'counts' or 'rates'")
  }

  if (output_dir == file.path(tempdir(), "writer_profiles")) {
    unlink(file.path(tempdir(), "writer_profiles"), recursive = TRUE)
  }

  return(profiles)
}

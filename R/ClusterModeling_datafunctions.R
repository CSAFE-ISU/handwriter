#' format_template_data
#'
#' `format_template_data()` formats the template data for use with
#' `plot_cluster_fill_counts()`. The output is a list that contains a data frame
#' called `cluster_fill_counts`. This structure mimics the structure of the
#' output of  `format_questioned_data()` which has an additional data
#' frame called `graph_measurements`. Graph measurements are not needed for the
#' template training data.
#'
#' @param template A single cluster template created by
#'   `make_clustering_templates()`
#' @return List that contains the cluster fill counts
#'
#' @examples
#' template_data <- format_template_data(template = example_cluster_template[[1]])
#' plot_cluster_fill_counts(formatted_data = template_data)
#'
#' @export
#' @md
format_template_data <- function(template) {
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


#' format_model_data
#'
#' `format_model_data()` formats the data need for the rjags model.
#'
#' @param model_proc_list List of processed handwriting from a set of documents
#'   created by `get_clusterassignment()`. Each item in the list contains the
#'   extracted graphs from a document. These processed documents will be used to train a model.
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
#' @export
#' @md
format_model_data <- function(model_proc_list, writer_indices, doc_indices, a = 2, b = 0.25, c = 2, d = 2, e = 0.5) {

  # get cluster assignment, slope, and pc_rotation for each graph in each model doc ----
  # get doc names from proclist
  model_docs <- sapply(model_proc_list, function(x) x$docname)

  # get writer ids
  writers <- as.integer(sapply(model_docs, function(x) substr(x, start = writer_indices[1], stop = writer_indices[2])))
  docs <- sapply(model_docs, function(x) substr(x, start = doc_indices[1], stop = doc_indices[2]), USE.NAMES = FALSE)

  for (j in 1:length(model_proc_list)) { # for each doc
    # initialize dataframe
    letter_measurements <- data.frame(matrix(ncol = 1, nrow = length(model_proc_list[[j]]$process$letterList)))
    names(letter_measurements) <- "writer"

    # add writer and doc names
    letter_measurements$writer <- writers[j]
    letter_measurements$doc <- docs[j]

    # add cluster assignment
    letter_measurements$cluster <- sapply(model_proc_list[[j]]$process$letterList, function(x) {
      x$cluster
    })

    # add other measurements
    letter_measurements$slope <- sapply(model_proc_list[[j]]$process$letterList, function(x) {
      x$characterFeatures$slope
    })
    letter_measurements$pc_rotation <- sapply(model_proc_list[[j]]$process$letterList, function(x) {
      xv <- x$characterFeatures$xvar
      yv <- x$characterFeatures$yvar
      cv <- x$characterFeatures$covar
      eig <- eigen(cbind(c(xv, cv), c(cv, yv)), symmetric = TRUE)
      return(angle(t(as.matrix(eig$vectors[, 1])), as.matrix(c(1, 0))))
    })
    letter_measurements$pc_wrapped <- letter_measurements$pc_rotation * 2

    # add to master dataframe
    if (j == 1) {
      graph_measurements <- letter_measurements
    } else {
      graph_measurements <- rbind(graph_measurements, letter_measurements)
    }
  }

  # if clusters aren't numbered sequentially, relabel them
  if (length(unique(graph_measurements$cluster)) < max(graph_measurements$cluster)) {
    graph_measurements <- graph_measurements %>% dplyr::rename("old_cluster" = cluster)
    cluster_lookup <- data.frame("old_cluster" = sort(unique(graph_measurements$old_cluster)), "cluster" = 1:length(unique(graph_measurements$old_cluster)))
    graph_measurements <- graph_measurements %>% dplyr::left_join(cluster_lookup, by = "old_cluster")
  }

  # get cluster fill counts ----
  # count number of graphs in each cluster for each writer
  cluster_fill_counts <- get_cluster_fill_counts(graph_measurements[, c("writer", "doc", "cluster")])

  # format data for rjags ----
  rjags_data <- list(
    Y = cluster_fill_counts[, -c(1, 2)], # multinomial data
    G = ncol(cluster_fill_counts[, -c(1, 2)]), # number of clusters (40)
    D = nrow(cluster_fill_counts[, -c(1, 2)]), # total number of documents
    W = length(unique(cluster_fill_counts$writer)), # number of unique writers
    # docwise
    docN = as.integer(apply(cluster_fill_counts[, -c(1, 2)], FUN = sum, MARGIN = 1)), # number of letters in each doc, e.g. N[1] = 354
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


#' format_questioned_data
#'
#' `format_questioned_data()` formats the questioned data for analysis with the
#' hierarchical model.
#'
#' @param model A fitted model created by [`fit_model`]
#' @param questioned_proc_list List of processed handwriting from a set of questioned
#'   documents created by `get_clusterassignment()`. Each item in the list
#'   contains the extracted graphs from a document.
#' @param writer_indices Vector of start and end indices for the writer id in
#'   the document names.
#' @param doc_indices Vector of start and end indices for the document id in the
#'   document names.
#' @return List of data formatted analysis.
#'
#' @export
#' @md
format_questioned_data <- function(model, questioned_proc_list, writer_indices, doc_indices) {

  # get cluster assignment, slope, and pc_rotation for each graph in each questioned doc ----
  # get doc names from proclist
  questioned_docs <- sapply(questioned_proc_list, function(x) x$docname)

  # get writer ids
  writers <- as.integer(sapply(questioned_docs, function(x) substr(x, start = writer_indices[1], stop = writer_indices[2])))
  docs <- sapply(questioned_docs, function(x) substr(x, start = doc_indices[1], stop = doc_indices[2]), USE.NAMES = FALSE)

  for (j in 1:length(questioned_proc_list)) { # for each doc
    # initialize dataframe
    letter_measurements <- data.frame(matrix(ncol = 1, nrow = length(questioned_proc_list[[j]]$process$letterList)))
    names(letter_measurements) <- c("writer")

    # add writer and doc names
    letter_measurements$writer <- writers[j]
    letter_measurements$doc <- docs[j]

    # add cluster assignment
    letter_measurements$cluster <- sapply(questioned_proc_list[[j]]$process$letterList, function(x) {
      x$cluster
    })

    # add other measurements
    letter_measurements$slope <- sapply(questioned_proc_list[[j]]$process$letterList, function(x) {
      x$characterFeatures$slope
    })
    letter_measurements$pc_rotation <- sapply(questioned_proc_list[[j]]$process$letterList, function(x) {
      xv <- x$characterFeatures$xvar
      yv <- x$characterFeatures$yvar
      cv <- x$characterFeatures$covar
      eig <- eigen(cbind(c(xv, cv), c(cv, yv)), symmetric = TRUE)
      return(angle(t(as.matrix(eig$vectors[, 1])), as.matrix(c(1, 0))))
    })
    letter_measurements$pc_wrapped <- letter_measurements$pc_rotation * 2

    # add to master dataframe
    if (j == 1) {
      graph_measurements <- letter_measurements
    } else {
      graph_measurements <- rbind(graph_measurements, letter_measurements)
    }
  }

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
  cluster_fill_counts <- get_cluster_fill_counts(graph_measurements[, c("writer", "doc", "cluster")])
  
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
      dplyr::mutate(across(where(is.numeric), ~ tidyr::replace_na(.x, 0)))
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
#' @md
get_cluster_fill_counts <- function(df) {
  # count number of graphs in each cluster for each writer
  cluster_fill_counts <- df %>%
    dplyr::group_by(writer, doc, cluster) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(n = as.integer(n)) %>%
    tidyr::pivot_wider(names_from = cluster, values_from = n, values_fill = 0)

  # sort columns
  cols <- c(colnames(cluster_fill_counts[, c(1, 2)]), sort(as.numeric(colnames(cluster_fill_counts[, -c(1, 2)]))))
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

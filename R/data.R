# data documentation

#' Cursive written word: csafe
#'
#' @format Binary image matrix. 111 rows and 410 columns.
#' @examples
#' csafe_document <- list()
#' csafe_document$image <- csafe
#' plotImage(csafe_document$image)
#' csafe_document$thin <- thinImage(csafe_document$image)
#' plotImageThinned(csafe_document$image, csafe_document$thin)
#' csafe_processList <- processHandwriting(csafe_document$thin, dim(csafe_document$image))
"csafe"

#' Cursive written word: London
#'
#' @format Binary image matrix. 148 rows and 481 columns.
#' @examples
#' london_document <- list()
#' london_document$image <- london
#' plotImage(london_document$image)
#' london_document$thin <- thinImage(london_document$image)
#' plotImageThinned(london_document$image, london_document$thin)
#' london_processList <- processHandwriting(london_document$thin, dim(london_document$image))
"london"

#' Full page image of the handwritten London letter.
#'
#' @format Binary image matrix. 1262 rows and 1162 columns.
#' @examples
#' \dontrun{
#' message_document <- list()
#' message_document$image <- message
#' plotImage(message_document$image)
#' message_document$thin <- thinImage(message_document$image)
#' plotImageThinned(message_document$image, message_document$thin)
#' message_processList <- processHandwriting(message_document$thin, dim(message_document$image))
#' }
"message"

#' Full page image of the 4th sample (nature) of handwriting from the first writer.
#'
#' @format Binary image matrix. 811 rows and 1590 columns.
#' @examples
#' \dontrun{
#' nature1_document <- list()
#' nature1_document$image <- nature1
#' plotImage(nature1_document$image)
#' nature1_document$thin <- thinImage(nature1_document$image)
#' plotImageThinned(nature1_document$image, nature1_document$thin)
#' nature1_processList <- processHandwriting(nature1_document$thin, dim(nature1_document$image))
#' }
"nature1"

#' Two sentence printed example handwriting
#'
#' @format Binary image matrix. 396 rows and 1947 columns
#' @examples
#' \dontrun{
#' twoSent_document <- list()
#' twoSent_document$image <- twoSent
#' plotImage(twoSent_document$image)
#' twoSent_document$thin <- thinImage(twoSent_document$image)
#' plotImageThinned(twoSent_document$image, twoSent_document$thin)
#' twoSent_processList <- processHandwriting(twoSent_document$thin, dim(twoSent_document$image))
#' }
"twoSent"

#' Example cluster template
#'
#' An example cluster template created from the template training example
#' handwriting documents included in the package. These documents are located in
#' `system.file("extdata/example_images/template_training_images", package =
#' "handwriter")`. The cluster template was created with K=10 clusters and a
#' small, random sample of 1000 graphs.
#'
#' @format A list containing a single cluster template created by
#'   [`make_clustering_templates()`]. The cluster template was created by
#'   sorting a random sample of 1000 graphs from 10 training documents into 10
#'   clusters with a K-means algorithm. The cluster template is a named list
#'   with 14 items: \describe{ \item{cluster}{A vector of cluster assignments
#'   for each graph used to create the cluster template.} \item{centers}{A list
#'   of graphs used as the starting cluster centers for the K-means algorithm.}
#'   \item{K}{The number of clusters to build (10) with the K-means algorithm.}
#'   \item{n}{The number of training graphs to use (1000) in the K-means
#'   algorithm.} \item{docnames}{A vector of the file names of the training
#'   documents.} \item{iters}{The maximum number of iterations for the K-means
#'   algorithm (3).} \item{changes}{A vector of the number of graphs that
#'   changed clusters on each iteration of the K-means algorithm.}
#'   \item{outlierCutoff}{A vector of the outlier cutoff values calculated on
#'   each iteration of the K-means algorithm.} \item{stop_reason}{The reason the
#'   K-means algorithm terminated.} \item{wcd}{A matrix of the within cluster
#'   distances on each iteration of the K-means algorithm. More specifically,
#'   the distance between each graph and the center of the cluster to which it
#'   was assigned  on each iteration.} \item{wcss}{A vector of the
#'   within-cluster sum of squares on each iteration of the K-means algorithm.}
#'   \item{rmse}{A vector of the root-mean square error on each iteration of the
#'   K-means algorithm.} \item{DaviesBouldinIndex}{The Davies-Bouldin index on
#'   each iteration of the K-means algorithm.} \item{VarianceRatioCriterion}{The
#'   variance-ratio criterion on each iteration of the K-means algorithm.} }
#' @examples
#' \dontrun{
#' # get cluster assignments for model training documents
#' model_dir <- "path/to/model_training_docs"
#' model_proc_list <- get_clusterassignment(example_template, model_dir)
#' }
#' @md
"example_cluster_template"

#' Example model training data with cluster assignments.
#'
#' The model training handwriting documents used for this example are included
#' in the package. They are located in
#' `system.file("extdata/example_images/template_training_images", package =
#' "handwriter")`. The documents were first processed with
#' [`process_batch_dir`] then their processed graphs where assigned to the
#' nearest cluster in the example cluster template with
#' [`get_clusterassignment`].
#'
#' @format List created by [`get_clusterassignment`] with extra fields removed
#'   to make the example file smaller.
#' @examples
#' model_data <- format_model_data(
#'   model_proc_list = example_model_clusters,
#'   writer_indices = c(2, 5),
#'   doc_indices = c(7, 18),
#'   a = 2, b = 0.25, c = 2, d = 2, e = 0.5
#' )
#' model <- fit_model(
#'   model_data = model_data,
#'   num_iters = 500,
#'   num_chains = 1
#' )
#' model <- drop_burnin(
#'   model = model,
#'   burn_in = 250
#' )
#' q_data <- format_questioned_data(
#'   formatted_model_data = model_data,
#'   questioned_proc_list = example_questioned_clusters,
#'   writer_indices = c(2, 5),
#'   doc_indices = c(7, 18)
#' )
#' analysis <- analyze_questioned_documents(
#'   model_data = model_data,
#'   model = model,
#'   questioned_data = q_data,
#'   num_cores = 2
#' )
#'
#' @md
"example_model_clusters"

#' Example questioned documents with with cluster assignments.
#'
#' The example questioned documents are included in the package. They are
#' located in `system.file("extdata/example_images/template_training_images",
#' package = "handwriter")`. The documents were first processed with
#' [`process_batch_dir`] then their processed graphs where assigned to the
#' nearest cluster in the example cluster template with
#' [`get_clusterassignment`].
#'
#' @format List created by [`get_clusterassignment`] with extra fields removed
#'   to make the example file smaller.
#'
#' @inherit example_model_clusters examples
#'
#' @md
"example_questioned_clusters"

#' Example of formatted model data
#'
#' @format A named list created by [`format_model_data`] with 19 items:
#' \describe{
#'   \item{Y}{data frame of cluster fill counts for each document}
#'   \item{G}{number of clusters}
#'   \item{D}{number of documents}
#'   \item{W}{number of writers}
#'   \item{docN}{number of graphs in each document}
#'   \item{docwriter}{writer id of each document}
#'   \item{zero_vec}{vector of zeros for used for the zeros-trick}
#'   \item{Gsmall}{number of non-empty clusters}
#'   \item{numletters}{total number of graphs}
#'   \item{pc_wrapped}{principal component rotation angle of each graph}
#'   \item{letterwriter}{writer id of each graph}
#'   \item{lettercluster}{cluster assignment of each graph}
#'   \item{zero_mat}{matrix of zeros for the zeros-trick}
#'   \item{a}{parameter}
#'   \item{b}{parameter}
#'   \item{c}{parameter}
#'   \item{d}{parameter}
#'   \item{e}{paramter}
#' }
#' @inherit example_model_clusters examples
#'
#' @md
"example_model_data"

#' RJAGS Wrapped Cauchy Model
#'
#' @format Wrapped Cauchy model written in RJAGS syntax.
#' @examples
#' rjags_model <- textConnection(model_wrapped_cauchy)
#' model_data <- example_model_data$rjags_data
#' m <- rjags::jags.model(file = rjags_model, data = model_data, n.chains = 1)
"model_wrapped_cauchy"

#' Example of questioned data formatted for the hierarchical model
#'
#' @format A named list created by [`format_questioned_data`] with 2 items:
#'   \describe{
#'   \item{graph_measurements}{data frame that shows the writer ID,
#'   document ID, slope, principal component rotation angle, and wrapped
#'   principal rotation angle for each graph }
#'   \item{cluster_fill_counts}{data frame that shows the number of graphs
#'   assigned to each cluster for each document.}
#'   }
#'
#' @inherit example_model_clusters examples
#'
#' @md
"example_questioned_data"

#' Example of writership analysis
#'
#' @format A named list created by [`analyze_questioned_documents`] with 3 items:
#'   \describe{
#'   \item{likelihoods}{list of data frames where each data frame
#'   contains the likelihoods for a questioned document for each MCMC iteration.}
#'   \item{votes}{list of vote tallys for each questioned document.}
#'   \item{posterior_probabilites}{list of posterior probabilities of writership
#'   for each questioned document and each known writer in the closed set used to train the
#'   hierarchical model.}
#'   }
#'
#' @examples
#' plot_posterior_probabilities(analysis = example_analysis)
#'
#' @md
"example_analysis"

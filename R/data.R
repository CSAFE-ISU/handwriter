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

#' Example of a hierarchical model
#'
#' @format An MCMC list created by [`fit_model`] with a single chain and 50 MCMC iterations.
#' The MCMC list contains a single MCMC object with 50 rows and 136 columns. Each row corresponds to
#' an MCMC iteration and each column corresponds to a variable:
#' \describe{
#'   \item{eta\[k\]}{Hyper priors for cluster k.}
#'   \item{gamma\[k\]}{The pseudo-cluster count across all writers for cluster k.}
#'   \item{mu\[w,k\]}{The location parameter of a wrapped-Cauchy distribution for writer w and cluster k.}
#'   \item{pi\[w,k\]}{The cluster fill probability for writer w and cluster k.}
#'   \item{tau\[w,k\]}{The scale parameter of a wrapped-Cauchy distribution for writer w and cluster k.}
#' }
#' @examples
#' # convert to a data frame and view all variable names
#' df <- as.data.frame(example_model_1chain[[1]])
#' colnames(df)
#'
#' # analyze questioned documents
#' analysis <- analyze_questioned_documents(
#'   model_data = example_model_data,
#'   model = example_model_1chain,
#'   questioned_data = example_questioned_data,
#'   num_cores = 2
#' )
#' plot_posterior_probabilities(analysis)
#'
#' @md
"example_model_1chain"

#' Example of a hierarchical model
#'
#' @format An MCMC list created by [`fit_model`] with two chains and 50 MCMC iterations per chain.
#' The MCMC list contains two MCMC objects, each with 50 rows and 136 columns. Each row corresponds to
#' an MCMC iteration and each column corresponds to a variable:
#' \describe{
#'   \item{eta\[k\]}{Hyper priors for cluster k.}
#'   \item{gamma\[k\]}{The pseudo-cluster count across all writers for cluster k.}
#'   \item{mu\[w,k\]}{The location parameter of a wrapped-Cauchy distribution for writer w and cluster k.}
#'   \item{pi\[w,k\]}{The cluster fill probability for writer w and cluster k.}
#'   \item{tau\[w,k\]}{The scale parameter of a wrapped-Cauchy distribution for writer w and cluster k.}
#' }
#' @examples
#' # convert the first chain to data frame and view all variable names
#' df <- as.data.frame(example_model_2chains[[1]])
#' colnames(df)
#'
#' # analyze questioned documents
#' analysis <- analyze_questioned_documents(
#'   model_data = example_model_data,
#'   model = example_model_2chains,
#'   questioned_data = example_questioned_data,
#'   num_cores = 2
#' )
#' plot_posterior_probabilities(analysis)
#'
#' @md
"example_model_2chains"

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
"example_analysis_1chain"

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
"example_analysis_2chains"

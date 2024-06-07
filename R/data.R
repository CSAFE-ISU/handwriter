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


#' Cursive written word: csafe
#'
#' @format Binary image matrix. 111 rows and 410 columns.
#' @examples
#' csafe_document <- list()
#' csafe_document$image <- csafe
#' plotImage(csafe_document)
#' csafe_document$thin <- thinImage(csafe_document$image)
#' plotImageThinned(csafe_document)
#' csafe_processList <- processHandwriting(csafe_document$thin, dim(csafe_document$image))
"csafe"

#' Cursive written word: London
#'
#' @format Binary image matrix. 148 rows and 481 columns.
#' @examples
#' london_document <- list()
#' london_document$image <- london
#' plotImage(london_document)
#' london_document$thin <- thinImage(london_document$image)
#' plotImageThinned(london_document)
#' london_processList <- processHandwriting(london_document$thin, dim(london_document$image))
"london"

#' Full page image of the handwritten London letter.
#'
#' @format Binary image matrix. 1262 rows and 1162 columns.
#' @examples
#' message_document <- list()
#' message_document$image <- message
#' plotImage(message_document)
#' 
#' \dontrun{
#' message_document <- list()
#' message_document$image <- message
#' plotImage(message_document)
#' message_document$thin <- thinImage(message_document$image)
#' plotImageThinned(message_document)
#' message_processList <- processHandwriting(message_document$thin, dim(message_document$image))
#' }
"message"

#' Full page image of the 4th sample (nature) of handwriting from the first writer.
#'
#' @format Binary image matrix. 811 rows and 1590 columns.
#' @examples
#' nature1_document <- list()
#' nature1_document$image <- nature1
#' plotImage(nature1_document)
#' 
#' \dontrun{
#' nature1_document <- list()
#' nature1_document$image <- nature1
#' plotImage(nature1_document)
#' nature1_document$thin <- thinImage(nature1_document$image)
#' plotImageThinned(nature1_document)
#' nature1_processList <- processHandwriting(nature1_document$thin, dim(nature1_document$image))
#' }
"nature1"

#' Two sentence printed example handwriting
#'
#' @format Binary image matrix. 396 rows and 1947 columns
#' @examples
#' twoSent_document <- list()
#' twoSent_document$image <- twoSent
#' plotImage(twoSent_document)
#' 
#' \dontrun{
#' twoSent_document <- list()
#' twoSent_document$image <- twoSent
#' plotImage(twoSent_document)
#' twoSent_document$thin <- thinImage(twoSent_document$image)
#' plotImageThinned(twoSent_document)
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
#'   [`make_clustering_template()`]. The cluster template was created by
#'   sorting a random sample of 1000 graphs from 10 training documents into 10
#'   clusters with a K-means algorithm. The cluster template is a named list
#'   with 16 items:
#' \describe{
#' \item{seed}{An integer for the random number generator.}
#' \item{cluster}{A vector of cluster assignments
#'   for each graph used to create the cluster template.}
#' \item{centers}{A list
#'   of graphs used as the starting cluster centers for the K-means algorithm.}
#' \item{K}{The number of clusters to build (10) with the K-means algorithm.}
#' \item{n}{The number of training graphs to use (1000) in the K-means
#'   algorithm.}
#' \item{docnames}{A vector that lists the training document from which each graph originated.}
#' \item{writers}{A vector that lists the writer of each graph.}
#' \item{iters}{The maximum number of iterations for the K-means
#'   algorithm (3).}
#' \item{changes}{A vector of the number of graphs that
#'   changed clusters on each iteration of the K-means algorithm.}
#' \item{outlierCutoff}{A vector of the outlier cutoff values calculated on
#'   each iteration of the K-means algorithm.}
#' \item{stop_reason}{The reason the
#'   K-means algorithm terminated.}
#' \item{wcd}{A matrix of the within cluster
#'   distances on each iteration of the K-means algorithm. More specifically,
#'   the distance between each graph and the center of the cluster to which it
#'   was assigned  on each iteration.}
#' \item{wcss}{A vector of the
#'   within-cluster sum of squares on each iteration of the K-means algorithm.}}
#' @examples
#' # view cluster fill counts for template training documents
#' template_data <- format_template_data(example_cluster_template)
#' plot_cluster_fill_counts(template_data, facet = TRUE)
#' 
#' @md
"example_cluster_template"

#' Example of a hierarchical model
#'
#' @format A hierarchical model created by [`fit_model`] with a single chain of 100 MCMC iterations. It is a named 
#' list of 4 objects: 
#' \describe{
#'   \item{graph_measurements}{A data frame of model training data that shows the writer, document name, cluster assignment, 
#'   slope, principle component rotation angle, and wrapped principle component rotation angle for each training graph.}
#'   \item{cluster_fill_counts}{A data frame of the cluster fill counts for each model training document.}
#'   \item{rjags_data}{The model training information from `graph_measurements` and `cluster_fill_counts` formatted for RJAGS.}
#'   \item{fitted_model}{A model fit using the `rjags_data` and the RJAGS and coda packages. It is an MCMC list that contains a single 
#'   MCMC object.}
#' }
#' @examples
#' # convert to a data frame and view all variable names
#' df <- as.data.frame(coda::as.mcmc(example_model$fitted_model))
#' colnames(df)
#' 
#' # view a trace plot
#' plot_trace(variable = "mu[1,1]", model = example_model)
#' 
#' # drop the first 25 MCMC iterations for burn-in
#' model <- drop_burnin(model = example_model, burn_in = 25)
#'
#' \dontrun{
#' # analyze questioned documents
#' main_dir <- /path/to/main_dir
#' questioned_docs <- /path/to/questioned_documents_directory
#' analysis <- analyze_questioned_documents(
#'    main_dir = main_dir,
#'    questioned_docs = questioned_docs
#'    model = example_model
#'    num_cores = 2
#' )
#' analysis$posterior_probabilities
#' }
#' 
#' @md
"example_model"

#' Example of writership analysis
#'
#' @format The results of [`analyze_questioned_documents()`] stored in a named list with 5 items:
#'   \describe{
#'   \item{graph_measurements}{A data frame of that shows the writer, document name, cluster assignment, 
#'   slope, principle component rotation angle, and wrapped principle component rotation angle for each training graph in each 
#'   questioned documents.}
#'   \item{cluster_fill_counts}{A data frame of the cluster fill counts for each questioned document.}
#'   \item{likelihoods}{A list of data frames where each data frame
#'   contains the likelihoods for a questioned document for each MCMC iteration.}
#'   \item{votes}{A list of vote tallies for each questioned document.}
#'   \item{posterior_probabilites}{A list of posterior probabilities of writership
#'   for each questioned document and each known writer in the closed set used to train the
#'   hierarchical model.}
#'   }
#'
#' @examples
#' plot_cluster_fill_counts(formatted_data = example_analysis)
#' plot_posterior_probabilities(analysis = example_analysis)
#'
#' @md
"example_analysis"

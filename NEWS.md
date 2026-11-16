# handwriter 3.3.0

## Breaking changes

* rjags moved from Imports to Suggests, because rjags requires the JAGS system library, and it can't be installed on platforms where JAGS 4 isn't available (#233). Only `fit_model()` uses rjags. It now gives an informative error if rjags isn't available. The rest of handwriter no longer needs JAGS or rjags. To use `fit_model()`, install JAGS 4.x.y and then rjags. coda, which was previously installed with rjags, moved from Suggests to Imports.

## Minor improvements and fixes

* Fixed bug in `get_cluster_fill_counts()`, introduced in version 3.2.3, where the cluster columns were no longer sorted numerically. They followed the order in which clusters first appeared in the data. `fit_model()` and `analyze_questioned_documents()` refer to clusters by column position, so the cluster fill counts of the model documents, the cluster fill counts of the questioned documents, and the cluster labels used for the wrapped-Cauchy parameters could refer to different clusters. This lowered the accuracy of the closed-set model. Models fit with versions 3.2.3 or 3.2.4 should be refit (#231).

* Fixed `analyze_questioned_documents()` so that known writers are labeled in the same order the model numbers them. Previously, if the writers' order in the data differed from their sorted order, posterior probabilities could be attributed to the wrong writer. `about_variable()` had the same issue and was also fixed (#231).

* Fixed `analyze_questioned_documents()` so that the document-level and graph-level likelihoods are always evaluated on the same questioned document (#231).

* Fixed bug in `process_batch_dir()` and `process_batch_list()` where console output could stay diverted to the problems.txt log file if processing stopped early, for example if the user interrupted it. The functions now always close the log file when they exit, and they no longer close sinks opened by the user (#229).

* Added the `num_clusters` argument to `get_cluster_fill_rates()`. Previously, the function assumed the cluster template had 40 clusters. The default is still 40. `get_writer_profiles()` now uses the number of clusters in its `template` (#231).

* The `fit_model()` documentation now recommends 4000 MCMC iterations, with the first 1000 dropped as burn-in using `drop_burnin()` (#223).

# handwriter 3.2.4

## Minor improvements and fixes

* Fixed `get_writer_profiles()` and `plot_writer_profiles()` so that they are exported functions.

# handwriter 3.2.3

## New features

* Created `get_cluster_fill_rates()` to calculate the proportion of graphs assigned to each cluster in a cluster template.

* Created `get_writer_profiles()` to estimate writer profiles for all handwritten documents in the input directory. Writer profiles are the cluster fill counts or the cluster fill rates. This function runs `process_batch_dir()` to split the handwriting into component shapes called graphs. Then, it runs `get_clusters_batch()` to assign each graph to a cluster in a cluster template. Lastly, it runs `get_cluster_fill_counts()` or `get_cluster_fill_rates()` depending on whether the user chooses counts or rates.

## Minor improvements and fixes

* Changed the `writer_indices` and `doc_indices` arguments in `get_clusters_batch()` to be optional. The main reason for this change is so that users do not need to use a naming convention for their handwriting files in order to use `handwriterRF::calculate_slr()` and `handwriterRF::compare_documents()` which call `get_clusters_batch()`.

* Fixed bug in `get_clusters_batch()` so that the warning "closing unused connection" no longer occurs if the user runs the function in parallel.

# handwriter 3.2.1

## Minor improvements and fixes

* The new function `plotGraphs()` added in version 3.2.0 mistakenly was not exported. The function has been renamed as `plot_graphs()` and is now available to users.

* Fixed a bug in `get_clusters_batch()` so it no longer returns the error message, "Unable to get cluster assignments for one or more documents," if the file all_clusters.rds exists in the output directory.

* Fixed a bug in `get_clusters_batch()` so it no longer mistakenly returns the error message, "Unable to get cluster assignments for one or more documents," even if cluster assignments were successfully processed for all input documents.

* If the graphs from the model training documents do not populate all available clusters in the cluster template, one or more of the graphs from the questioned document(s) could potentially be assigned to a cluster in the template that doesn't contain any model training graphs. Previously, `analyze_questioned_documents()` threw an error if this occurs. Now, it only throws an error if more than 10% of the graphs from the questioned documents are assigned to clusters that are not populated by model training graphs. The mismatch between clusters used by the model document graphs and clusters used by the questioned document(s) graphs only becomes an issue if it occurs for a large number of graphs.

# handwriter 3.2.0

## New features

* The new function `plot_cluster_centers()` creates a plot of the clusters centers from a cluster template. The cluster centers are displayed as orange shapes. The function also plots all graphs in each cluster as grey shapes with 5% transparency to depict the variability of graph shapes within each cluster.

* The new function `plotGraphs()` plots every graph in a document processed with `processDocument()`.

## Minor improvements and fixes

* Fixed bug in `processDocument()` when the writing in the document is a single connected component, such as a single word written in cursive. Previously, the output of `processDocument()` for this kind of document was formatted incorrectly.

* Fixed bug in `get_credible_intervals()` and `plot_credible_intervals()` where the model writers were numbered sequentially. Now these functions use the writer IDs.

* Fixed bug in `format_template_data()` where the function coerced writer IDs to integers even if the writer IDs contained characters.

* Fixed bug in `get_clusters_batch()` where the function would stall but not return an error message if a document had a graph with a large number of edges (paths). Now the function ignores graphs with more than 30 edges.

* Fixed bug in `fit_model()` where the function saves the same data in two separate files: "model_clusters.rds" and "all_clusters.rds". The argument 'save_master_file' was added to `get_clusters_batch()`. If TRUE, a data frame of all cluster assignments, "all_clusters.rds", will be saved. The default is FALSE.

# handwriter 3.1.1

## Patches

* The functions `fit_model()` and `analyze_questioned_documents()` now allow writer IDs that contain numbers and letters.


# handwriter 3.1.0

## Minor improvements and fixes

* Increased the speed of `processHandwriting()` by changing the function to process a handwritten document in sections instead of all at once. Nodes created by `processHandwriting()` in version 3.1.0 might differ slightly in placement from previous versions. 

* Fixed bugs in `fit_model()` and `analyze_questioned_documents()` introduced by the changes to `process_batch_list()` in version 3.0.0.

# handwriter 3.0.0

* Major reductions made to the memory required by `process_batch_list()` so it can now process paragraph length documents from the CSAFE Handwriting Database on machines with 8 GB of RAM.

* `process_batch_list()` now skips to the next document if an error is returned while trying to process a document. A log file records the document name(s) and error message(s) of any problem documents. If the user reruns `process_batch_list()` they now have the option to either try a second time to process problem documents or skip them entirely.

* New function `get_clusters_batch()` calculates cluster assignments of all files in a directory.


# handwriter 2.0.3

* Fix initializer warning with arma::vec. Apparently different versions of gcc were raising warnings with the assignment of values in neighbors. The recommended fix was to use an initializer list.


# handwriter 2.0.2

## New features

* `fit_model()` allows the user to fit a statistical model to known handwriting samples collected from a closed-set of persons.

* `analyze_questioned_documents()` in conjunction with a model created by `fit_model()` allows a user to calculate the posterior probability that each known writer in the model is the writer of the questioned document(s). `analyze_questioned_documents()` only works when the questioned document(s) had to have been written by one of the model writers. This function must NOT be used if someone other than one of the model writers could written the questioned document(s) as it could yield misleading results.

## Minor improvements and fixes

* `processDocument()` is a new wrapper function that runs `readPNGBinary()`, `thinImage()`, and `processHandwriting()` so the user doesn't need to run these functions individually.

* `plotImage()`, `plotImageThinned()`, and `plotNodes` now only need one input, a document processed with `processDocument()`

* `processHandwriting()` no longer crashes when the input writing is a single letter or word

* `read_and_process()` superseded in favor of `processDocument()`

* `extractGraphs()` superseded in favor of `process_batch_dir()`

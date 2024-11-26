# handwriter 3.2.2

## Minor improvements and fixes

* Changed the `writer_indices` and `doc_indices` arguments in `get_clusters_batch()` to be optional. The main reason for this change is so that users do not need to use a naming convention for their handwriting files in order to use `handwriterRF::calculate_slr()` and `handwriterRF::compare_documents()` which call `get_clusters_batch()`.

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

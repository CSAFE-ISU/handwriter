# handwriter (development version)

## Minor improvements and fixes

* Fixed bug in `processDocument()` when the writing in the document is a single connected component, such as a single word written in cursive. Previously, the output of `processDocument()` for this kind of document was formatted incorrectly.

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

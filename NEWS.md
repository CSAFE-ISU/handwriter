# handwriter 2.0.0

## New features

* `fit_model()` allows the user to fit a statistical model to known handwriting samples collected from a closed-set of persons.

* `analyze_questioned_documents()` in conjunction with a model created by `fit_model()` allows a user to calculate the posterior probability that each known writer in the model is the writer of the questioned document(s). `analyze_questioned_documents()` only works when the questioned document(s) had to have been written by one of the model writers. This function must NOT be used if someone other than one of the model writers could written the questioned document(s) as it could yield misleading results.

## Minor improvements and fixes

* `processDocument()` is a new wrapper function that runs `readPNGBinary()`, `thinImage()`, and `processHandwriting()` so the user doesn't need to run these functions individually.

* `plotImage()`, `plotImageThinned()`, and `plotNodes` now only need one input, a document processed with `processDocument()`

* `processHandwriting()` no longer crashes when the input writing is a single letter or word

* `read_and_process()` superseded in favor of `processDocument()`

* `extractGraphs()` superseded in favor of `process_batch_dir()`
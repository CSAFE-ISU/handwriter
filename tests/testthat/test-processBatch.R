test_that("Batch processing a directory works", {
  expect_no_error(process_batch_dir(input_dir = testthat::test_path("fixtures", "processHandwriting", "samples"),
                                    output_dir = tempdir(),
                                    skip_docs_on_retry = TRUE))
  
})
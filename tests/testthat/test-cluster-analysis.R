# Single Chain ------------------------------------------------------------
test_that("analyze questioned document works when writer IDs contain numbers and letters", {
  
  # delete tempdir() > main_dir
  empty_tempdir(subfolder = "main_dir")
  
  # copy docs and graphs to tempdir > main_dir > data to save processing time on
  # the next step
  files2tempdir(type = "questioned")
  
  actual <- analyze_questioned_documents(main_dir = file.path(tempdir(), 'main_dir'),
                                         questioned_docs = file.path(tempdir(), 'main_dir', 'data', 'questioned_docs'),
                                         model = example_model,
                                         num_cores = 1,
                                         writer_indices = c(1, 5),
                                         doc_indices = c(7, 18))
  
  expect_identical(actual, example_analysis)
})

test_that("calculate accuracy works on a single chain", {
  actual <- calculate_accuracy(example_analysis)
  
  expect_equal(actual, 1)
})

# process_batch_dir -------------------------------------------------------

testthat::test_that("Batch processing a directory works", {
  empty_tempdir("batch")
  
  # Check that process_batch_dir() processes docs without error
  testthat::expect_no_error(
    process_batch_dir(
      input_dir = testthat::test_path("fixtures", "temp1qd", "data", "template_docs"),
      output_dir = file.path(tempdir(), "batch"),
      skip_docs_on_retry = TRUE
    )
  )
  
  # Check that process_batch_dir() detects that docs were already processed
  testthat::expect_message(
    process_batch_dir(
      input_dir = testthat::test_path("fixtures", "temp1qd", "data", "template_docs"),
      output_dir = file.path(tempdir(), "batch"),
      skip_docs_on_retry = TRUE
    ),
    "All documents have been processed or flagged as problem files."
  )
})


# show_problem_docs -------------------------------------------------------

test_that("Show problem docs gives correct message when log is empty", {
  withr::with_file("file1", {
    writeLines("", "file1")
    show_problem_docs("file1")
    expect_message(show_problem_docs("file1"), "All documents were successfully processed...")
  })
})

test_that("Show problem docs gives correct message when log does not contain the phrase 'error with document'", {
  withr::with_file("file1", {
    writeLines("aklsdmfc oaierj mamv oiaerj oiaejrm avmoimrfio w0001.png", "file1")
    show_problem_docs("file1")
    expect_message(show_problem_docs("file1"), "All documents were successfully processed...")
  })
})

test_that("Show problem docs gives correct message when log contains as single problem document", {
  # document has .png extension
  withr::with_file("file1", {
    writeLines("error with document w0001_s03_pLND_r01.png", "file1")
    show_problem_docs("file1")
    expect_message(show_problem_docs("file1"), "The following documents could not be processed: w0001_s03_pLND_r01.png")
  })
})  

test_that("Show problem docs gives correct message when problem document has uppercase file extension", {
  withr::with_file("file1", {
    writeLines("error with document w0001_s03_pLND_r01.PNG", "file1")
    show_problem_docs("file1")
    expect_message(show_problem_docs("file1"), "The following documents could not be processed: w0001_s03_pLND_r01.PNG")
  })
}) 

test_that("Show problem docs gives correct message when log contains as multiple problem documents", {
  # multiple lines, each with a problem doc
  withr::with_file("file1", {
    writeLines(c("error with document w0001_s03_pLND_r01.png", "error with document cvl0001.png"), "file1")
    show_problem_docs("file1")
    expect_message(show_problem_docs("file1"), "The following documents could not be processed: w0001_s03_pLND_r01.png, cvl0001.png")
  })
})

test_that("Show problem docs gives correct message when log contains as multiple problem documents and line without a document", {
  withr::with_file("file1", {
    writeLines(c("error with document w0001_s03_pLND_r01.png", "null", "error with document cvl0001.png"), "file1")
    show_problem_docs("file1")
    expect_message(show_problem_docs("file1"), "The following documents could not be processed: w0001_s03_pLND_r01.png, cvl0001.png")
  })
})

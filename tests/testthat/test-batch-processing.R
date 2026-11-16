# process_batch_dir -------------------------------------------------------

testthat::test_that("process_batch_dir works", {
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

testthat::test_that("process_batch_dir works when a doc has been flagged as a problem and all other docs have been processed", {
  # w0030_s03_pWOZ_r01.png was artificially flagged as a problem doc in problems.txt
  
  empty_tempdir("main_dir")
  
  # copy docs
  copy_files(
    input_dir = testthat::test_path("fixtures", "batch-processing", "docs"),
    output_dir = file.path(tempdir(), "main_dir", "docs")
  )
  # copy graph file and problems.txt
  copy_files(
    input_dir = testthat::test_path("fixtures", "batch-processing", "graphs"),
    output_dir = file.path(tempdir(), "main_dir", "graphs"),
    extensions = ".rds|.txt"
  )
  
  testthat::expect_no_error(
    process_batch_dir(
      input_dir = file.path(tempdir(), "main_dir", "docs"),
      output_dir = file.path(tempdir(), "main_dir", "graphs"),
      skip_docs_on_retry = TRUE
    )
  )
  
  testthat::expect_identical(list.files(file.path(tempdir(), "main_dir", "graphs")),
                             c("problems.txt", "w0238_s01_pLND_r01_proclist.rds"))
  
})

testthat::test_that("process_batch_dir works when a doc has been flagged as a problem and another doc has not been processed", {
  # w0030_s03_pWOZ_r01.png was artificially flagged as a problem doc in problems.txt
  
  empty_tempdir("main_dir")
  
  # copy docs
  copy_files(
    input_dir = testthat::test_path("fixtures", "batch-processing", "docs"),
    output_dir = file.path(tempdir(), "main_dir", "docs"),
    extensions = ".png"
  )
  # copy problems.txt only
  copy_files(
    input_dir = testthat::test_path("fixtures", "batch-processing", "graphs"),
    output_dir = file.path(tempdir(), "main_dir", "graphs"),
    extensions = ".txt"
  )
  
  testthat::expect_no_error(
    process_batch_dir(
      input_dir = file.path(tempdir(), "main_dir", "docs"),
      output_dir = file.path(tempdir(), "main_dir", "graphs"),
      skip_docs_on_retry = TRUE
    )
  )
  
  testthat::expect_identical(list.files(file.path(tempdir(), "main_dir", "graphs")),
                             c("problems.txt", "w0238_s01_pLND_r01_proclist.rds"))
  
})


# show_problem_docs -------------------------------------------------------

testthat::test_that("process_batch_list closes the log sink if it is interrupted", {
  # issue #229: if processing stopped early, console output stayed diverted to
  # problems.txt
  output_dir <- file.path(tempdir(), "sink_test")
  unlink(output_dir, recursive = TRUE)
  images <- list.files(testthat::test_path("fixtures", "temp1qd", "data", "questioned_docs"),
                       pattern = ".png", full.names = TRUE)
  
  # simulate the user interrupting processing. tryCatch(error = ) in
  # process_batch_list() doesn't catch interrupts. testthat aborts the test run
  # on a real "interrupt" condition, so use a custom condition that also isn't
  # an error and exits the function the same way.
  testthat::local_mocked_bindings(processDocument = function(...) {
    stop(structure(class = c("simulated_interrupt", "condition"), list(message = "interrupted", call = NULL)))
  })
  
  sinks_before <- sink.number()
  result <- tryCatch(
    suppressMessages(process_batch_list(images = images, output_dir = output_dir)),
    simulated_interrupt = function(cnd) "interrupted"
  )
  testthat::expect_equal(result, "interrupted")
  testthat::expect_equal(sink.number(), sinks_before)
})

testthat::test_that("process_batch_list doesn't close sinks opened by the user", {
  output_dir <- file.path(tempdir(), "sink_test")
  unlink(output_dir, recursive = TRUE)
  images <- list.files(testthat::test_path("fixtures", "temp1qd", "data", "questioned_docs"),
                       pattern = ".png", full.names = TRUE)
  testthat::local_mocked_bindings(processDocument = function(...) stop("test error"))
  
  user_log <- tempfile(fileext = ".txt")
  sink(user_log)
  sinks_before <- sink.number()
  suppressMessages(process_batch_list(images = images, output_dir = output_dir))
  sinks_after <- sink.number()
  sink()
  
  testthat::expect_equal(sinks_after, sinks_before)
  # the error was logged to problems.txt
  testthat::expect_true(any(grepl("test error", readLines(file.path(output_dir, "problems.txt")))))
})

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

test_that("Show problem docs gives correct message when log contains multiple problem documents", {
  # multiple lines, each with a problem doc
  withr::with_file("file1", {
    writeLines(c("error with document w0001_s03_pLND_r01.png", "error with document cvl0001.png"), "file1")
    show_problem_docs("file1")
    expect_message(show_problem_docs("file1"), "The following documents could not be processed: w0001_s03_pLND_r01.png, cvl0001.png")
  })
})

test_that("Show problem docs gives correct message when log contains multiple problem documents and line without a document", {
  withr::with_file("file1", {
    writeLines(c("error with document w0001_s03_pLND_r01.png", "null", "error with document cvl0001.png"), "file1")
    show_problem_docs("file1")
    expect_message(show_problem_docs("file1"), "The following documents could not be processed: w0001_s03_pLND_r01.png, cvl0001.png")
  })
})

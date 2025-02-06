devtools::load_all()

main_dir <- testthat::test_path("fixtures", "batch-processing")

process_batch_dir(input_dir = file.path(main_dir, "docs"),
                  output_dir = file.path(main_dir, "graphs"),
                  skip_docs_on_retry = TRUE)

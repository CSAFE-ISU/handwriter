test_that("get_csafe_writerIDs extracts writer IDs from file names", {
  docs <- c("w0001_s01_pLND_r01.png", "w0001_s02_pWOZ_r01.png",
            "w0238_s01_pPHR_r02.png")
  
  expect_equal(get_csafe_writerIDs(docs), c("w0001", "w0001", "w0238"))
})

test_that("get_csafe_writerIDs extracts writer IDs from full file paths", {
  docs <- c("path/to/w0001_s01_pLND_r01.png",
            "/another/path/w0238_s01_pPHR_r02.png")
  
  expect_equal(get_csafe_writerIDs(docs), c("w0001", "w0238"))
})

test_that("get_csafe_writerIDs handles a mix of file names and paths", {
  docs <- c("path/to/w0001_s01_pLND_r01.png", "w0238_s01_pPHR_r02.png")
  
  expect_equal(get_csafe_writerIDs(docs), c("w0001", "w0238"))
})

test_that("get_csafe_writerIDs returns unique writer IDs when only_unique = TRUE", {
  docs <- c("w0001_s01_pLND_r01.png", "path/to/w0001_s02_pWOZ_r01.png",
            "w0238_s01_pPHR_r02.png")
  
  expect_equal(get_csafe_writerIDs(docs, only_unique = TRUE),
               c("w0001", "w0238"))
})

test_that("get_csafe_writerIDs returns integers when as_integer = TRUE", {
  docs <- c("w0001_s01_pLND_r01.png", "w0238_s01_pPHR_r02.png")
  
  result <- get_csafe_writerIDs(docs, as_integer = TRUE)
  
  expect_type(result, "integer")
  expect_equal(result, c(1L, 238L))
})

test_that("get_csafe_writerIDs combines only_unique and as_integer", {
  docs <- c("w0001_s01_pLND_r01.png", "path/to/w0001_s02_pWOZ_r01.png",
            "w0238_s01_pPHR_r02.png")
  
  expect_equal(
    get_csafe_writerIDs(docs, only_unique = TRUE, as_integer = TRUE),
    c(1L, 238L)
  )
})

test_that("get_csafe_writerIDs handles empty input", {
  expect_equal(get_csafe_writerIDs(character(0)), character(0))
  expect_equal(get_csafe_writerIDs(character(0), as_integer = TRUE),
               integer(0))
})

# Single Chain ------------------------------------------------------------
test_that("analyze questioned documents works with a single chain", {
  iters <- 200  # number of MCMC iterations in example_model_1chain
  template_dir <- test_path("fixtures", "template")
  questioned_docs <- system.file("extdata/example_images/questioned_docs", package = "handwriter")
  analysis <- analyze_questioned_documents(template_dir = template_dir, 
                                           questioned_images_dir = questioned_docs, 
                                           model = example_model_1chain, 
                                           num_cores = 2,
                                           writer_indices = c(2,5),
                                           doc_indices = c(7,17))
  
  # expect named list
  expect_named(analysis, c("likelihood_evals", "votes", "posterior_probabilities", "graph_measurements",
                           "cluster_fill_counts"))
  
  # check cluster fill counts column names
  expect_equal(colnames(example_model_1chain$cluster_fill_counts), colnames(analysis$cluster_fill_counts))
  
  # check vote totals
  sapply(analysis$votes, function(x) expect_equal(sum(x), iters))
  
  # check posterior probability totals
  sapply(analysis$posterior_probabilities[,-1], function(x) expect_equal(sum(x), 1))
})

test_that("calculate accuracy works on a single chain", {
  accuracy <- calculate_accuracy(example_analysis_1chain)
  
  expect_equal(accuracy, 0.86)
})

test_that("get top writer works on a single chain", {
  expect_equal(get_top_writer(example_analysis_1chain, "w0009_s03_pWOZ_r01.png"), "known_writer_9")
  expect_equal(get_top_writer(example_analysis_1chain, "w0030_s03_pWOZ_r01.png"), "known_writer_30")
  expect_equal(get_top_writer(example_analysis_1chain, "w0238_s03_pWOZ_r01.png"), "known_writer_238")
  expect_equal(get_top_writer(example_analysis_1chain, "w0400_s03_pWOZ_r01.png"), "known_writer_400")
  # method misidentifies top writer for w0203_s03_pWOZ_r01.png
  expect_equal(get_top_writer(example_analysis_1chain, "w0203_s03_pWOZ_r01.png"), "known_writer_238")
})

test_that("count csafe top writer works on a single chain", {
  results <- count_csafe_correct_top_writer(example_analysis_1chain)
  
  expect_named(results, c("correct", "total"))
  expect_equal(results$correct, 4)
  expect_equal(results$total, 5)
})


# Multiple Chains ---------------------------------------------------------
test_that("analyze questioned documents works with multiple chains", {
  iters <- 200  # number of MCMC iterations per chain in example_model_2chains
  template_dir <- test_path("fixtures", "template")
  questioned_docs <- system.file("extdata/example_images/questioned_docs", package = "handwriter")
  analysis <- analyze_questioned_documents(template_dir = template_dir, 
                                           questioned_images_dir = questioned_docs, 
                                           model = example_model_2chains, 
                                           num_cores = 2,
                                           writer_indices = c(2,5),
                                           doc_indices = c(7,17))
  
  # expect named list
  expect_named(analysis, c("likelihood_evals", "votes", "posterior_probabilities", "graph_measurements",
                           "cluster_fill_counts"))
  
  # check cluster fill counts column names
  expect_equal(colnames(example_model_2chains$cluster_fill_counts), colnames(analysis$cluster_fill_counts))
  
  # check vote totals
  sapply(analysis$votes, function(x) expect_equal(sum(x), 2*iters))
  
  # check posterior probability totals
  sapply(analysis$posterior_probabilities[,-1], function(x) expect_equal(sum(x), 1))
})

test_that("calculate accuracy works on mulitple chains", {
  accuracy <- calculate_accuracy(example_analysis_2chains)
  
  expect_equal(accuracy, 0.8605)
})

test_that("get top writer works on multiple chains", {
  expect_equal(get_top_writer(example_analysis_2chains, "w0009_s03_pWOZ_r01.png"), "known_writer_9")
  expect_equal(get_top_writer(example_analysis_2chains, "w0030_s03_pWOZ_r01.png"), "known_writer_30")
  expect_equal(get_top_writer(example_analysis_2chains, "w0238_s03_pWOZ_r01.png"), "known_writer_238")
  expect_equal(get_top_writer(example_analysis_2chains, "w0400_s03_pWOZ_r01.png"), "known_writer_400")
  # method misidentifies top writer for w0203_s03_pWOZ_r01.png
  expect_equal(get_top_writer(example_analysis_2chains, "w0203_s03_pWOZ_r01.png"), "known_writer_238")
})

test_that("count csafe top writer works on multiple chains", {
  results <- count_csafe_correct_top_writer(example_analysis_2chains)
  
  expect_named(results, c("correct", "total"))
  expect_equal(results$correct, 4)
  expect_equal(results$total, 5)
})

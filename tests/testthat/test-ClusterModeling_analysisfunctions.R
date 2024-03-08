# Single Chain ------------------------------------------------------------
# test_that("analyze questioned documents works with a single chain", {
#   iters <- 200  # number of MCMC iterations in example_model_1chain
#   template_dir <- test_path("fixtures", "template")
#   questioned_docs <- system.file("extdata/example_images/questioned_docs", package = "handwriter")
#   analysis <- analyze_questioned_documents(template_dir = template_dir, 
#                                            questioned_images_dir = questioned_docs, 
#                                            model = example_model_1chain, 
#                                            num_cores = 2,
#                                            writer_indices = c(2,5),
#                                            doc_indices = c(7,17))
#   
#   # expect named list
#   expect_named(analysis, c("likelihood_evals", "votes", "posterior_probabilities", "graph_measurements",
#                            "cluster_fill_counts"))
#   
#   # check cluster fill counts column names
#   expect_equal(colnames(example_model_1chain$cluster_fill_counts), colnames(analysis$cluster_fill_counts))
#   
#   # check vote totals
#   sapply(analysis$votes, function(x) expect_equal(sum(x), iters))
#   
#   # check posterior probability totals
#   sapply(analysis$posterior_probabilities[,-1], function(x) expect_equal(sum(x), 1))
# })

test_that("calculate accuracy works on a single chain", {
  accuracy <- calculate_accuracy(example_analysis_1chain)
  
  expect_equal(accuracy, 0.86)
})

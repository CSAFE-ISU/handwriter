actual <- get_posterior_probabilities(
  analysis = example_analysis,
  questioned_doc = "w0030_s03_pWOZ_r01"
)
saveRDS(actual, testthat::test_path("fixtures", "analysis", "posterior_probabilities.rds"))

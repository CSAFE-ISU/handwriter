testthat::test_that("Plot cluster fill counts runs sucessfully", {
  # template data
  template_data <- format_template_data(example_cluster_template)
  testthat::expect_no_error(
    plot_cluster_fill_counts(formatted_data = template_data, facet = TRUE)
  )

  # model data
  testthat::expect_no_error(
    plot_cluster_fill_counts(formatted_data = example_model, facet = TRUE)
  )

  # analysis data
  testthat::expect_no_error(
    plot_cluster_fill_counts(formatted_data = example_analysis, facet = TRUE)
  )
  
})

testthat::test_that("Plot cluster fill rates runs sucessfully", {
  # template data
  template_data <- format_template_data(example_cluster_template)
  testthat::expect_no_error(
    plot_cluster_fill_rates(formatted_data = template_data, facet = TRUE)
  )
  
  # model data
  testthat::expect_no_error(
    plot_cluster_fill_rates(formatted_data = example_model, facet = TRUE)
  )
  
  # analysis data
  testthat::expect_no_error(
    plot_cluster_fill_rates(formatted_data = example_analysis, facet = TRUE)
  )
  
})

testthat::test_that("Plot writer profiles runs successfully with cluster fill counts", {
  counts <- readRDS(testthat::test_path("fixtures", "processHandwriting", "counts.rds"))
  
  # with default values
  testthat::expect_no_error(plot_writer_profiles(counts))
  testthat::expect_no_warning(plot_writer_profiles(counts))
  
  # color by writer instead of docname
  testthat::expect_no_error(plot_writer_profiles(counts, color_by = "writer"))
  testthat::expect_no_warning(plot_writer_profiles(counts, color_by = "writer"))
  
  # facet by writer
  testthat::expect_no_error(plot_writer_profiles(counts, facets = "writer"))
  testthat::expect_no_warning(plot_writer_profiles(counts, facets = "writer"))
  testthat::expect_no_error(plot_writer_profiles(counts, color_by = "writer", facets = "writer"))
  testthat::expect_no_warning(plot_writer_profiles(counts, color_by = "writer", facets = "writer"))
  
  # facet by docname
  testthat::expect_no_error(plot_writer_profiles(counts, color_by = "writer", facets = "docname"))
  testthat::expect_no_warning(plot_writer_profiles(counts, color_by = "writer", facets = "docname"))
  testthat::expect_no_error(plot_writer_profiles(counts, color_by = "writer", facets = "docname"))
  testthat::expect_no_warning(plot_writer_profiles(counts, color_by = "writer", facets = "docname"))
  
  # facet by docname and set number of rows
  testthat::expect_no_error(plot_writer_profiles(counts, color_by = "writer", facets = "docname", nrow = 4))
  testthat::expect_no_warning(plot_writer_profiles(counts, color_by = "writer", facets = "docname", nrow = 4))
  
  # facet by docname and set number of rows
  testthat::expect_no_error(plot_writer_profiles(counts, color_by = "docname", facets = "docname", ncol = 4))
  testthat::expect_no_warning(plot_writer_profiles(counts, color_by = "docname", facets = "docname", ncol = 4))
})

testthat::test_that("Plot writer profiles runs successfully with cluster fill rates", {
  rates <- readRDS(testthat::test_path("fixtures", "processHandwriting", "rates.rds"))
  
  # with default values
  testthat::expect_no_error(plot_writer_profiles(rates))
  
  # color by writer instead of docname
  testthat::expect_no_error(plot_writer_profiles(rates, color_by = "writer"))
  
  # facet by writer
  testthat::expect_no_error(plot_writer_profiles(rates, facets = "writer"))
  testthat::expect_no_error(plot_writer_profiles(rates, color_by = "writer", facets = "writer"))
  
  # facet by docname
  testthat::expect_no_error(plot_writer_profiles(rates, color_by = "writer", facets = "docname"))
  testthat::expect_no_error(plot_writer_profiles(rates, color_by = "writer", facets = "docname"))
  
  # facet by docname and set number of rows
  testthat::expect_no_error(plot_writer_profiles(rates, color_by = "writer", facets = "docname", nrow = 4))
  
  # facet by docname and set number of rows
  testthat::expect_no_error(plot_writer_profiles(rates, color_by = "docname", facets = "docname", ncol = 4))
})

testthat::test_that("Plot trace runs successfully", {
  testthat::expect_no_error(plot_trace(model = example_model, variable = "pi[1,1]"))
  testthat::expect_no_error(plot_trace(model = example_model, variable = "mu[2,3]"))
})

testthat::test_that("Plot credible intervals runs sucessfully", {
  testthat::expect_no_error(plot_credible_intervals(model = example_model))
  testthat::expect_no_error(plot_credible_intervals(model = example_model, facet = TRUE))
})

testthat::test_that("Plot posterior probabilities runs successfully", {
  testthat::expect_no_error(plot_posterior_probabilities(analysis = example_analysis))
})

testthat::test_that("Plot cluster centers runs successfully", {
  testthat::expect_no_error(plot_cluster_centers(example_cluster_template))
  testthat::expect_no_error(plot_cluster_centers(example_cluster_template, plot_graphs = TRUE))
})

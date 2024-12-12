# Save Amy's template as a data object. example_cluster_template$K has five
# clusters and is used in many examples because it is small and runs fast. Amy's
# template is has forty clusters and can be used for real analyses.

templateK40 <- readRDS("data-raw/templateK40.rds")
usethis::use_data(templateK40)

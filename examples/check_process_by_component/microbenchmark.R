devtools::load_all()


main_dir <- 'examples/check_process_by_component/docs'
files <- list.files(main_dir, full.names = TRUE)

for (i in 2:length(files)){
  file <- files[i]
  m <- microbenchmark::microbenchmark(processDocument(file), processDocument_by_component(file), times = 5)
  write.csv(summary(m), file.path('examples/check_process_by_component/data', stringr::str_replace(basename(file), '.png', '_microbenchmark.csv')))
}

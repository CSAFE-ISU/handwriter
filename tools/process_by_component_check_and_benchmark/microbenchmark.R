library(ggplot2)

devtools::load_all()

input_dir <- '/Volumes/T7_Shield/CSAFE/handwriting_datasets/CSAFE_docs'
docs_dir <- 'examples/check_process_by_component/docs'

# # sample docs ----
# # make dataframe
# files <- list.files(input_dir, full.names = TRUE)
# df <- data.frame(path = files)
# df$file <- sapply(df$path, basename)
# df <- df %>% tidyr::separate_wider_delim(file, delim = '_', names = c('writer', 'session', 'prompt', 'repetition'), cols_remove = FALSE)
# 
# # sample LND
# lnd <- df %>% 
#   dplyr::filter(prompt == 'pLND') %>% 
#   dplyr::group_by(writer) %>% 
#   dplyr::slice_sample(n=1) %>% 
#   dplyr::ungroup() %>%
#   dplyr::slice_sample(n=10)
# # copy to docs dir
# file.copy(lnd$path, docs_dir)
# 
# # sample PHR
# phr <- df %>% 
#   dplyr::filter(prompt == 'pPHR') %>% 
#   dplyr::group_by(writer) %>% 
#   dplyr::slice_sample(n=1) %>% 
#   dplyr::ungroup() %>%
#   dplyr::slice_sample(n=10)
# # copy to docs dir
# file.copy(phr$path, docs_dir)
# 
# # sample WOZ
# woz <- df %>% 
#   dplyr::filter(prompt == 'pWOZ') %>% 
#   dplyr::group_by(writer) %>% 
#   dplyr::slice_sample(n=1) %>% 
#   dplyr::ungroup() %>%
#   dplyr::slice_sample(n=10)
# # copy to docs dir
# file.copy(woz$path, docs_dir)

# # benchmark ----
# docs <- list.files(docs_dir, full.names = TRUE, pattern='.png')
# for (i in 1:length(docs)){
#   doc <- docs[i]
#   m <- microbenchmark::microbenchmark(processDocument(doc), processDocument_by_component(doc), times = 5)
#   write.csv(summary(m), file.path('examples/check_process_by_component/data', stringr::str_replace(basename(doc), '.png', '_microbenchmark.csv')))
# }

# analyze ----
# make dataframe
files <- list.files('examples/check_process_by_component/data', full.names = TRUE, pattern = '.csv')
df <- do.call(rbind, lapply(files, function(file) {
  df <- read.csv2(file, sep=',')
  # drop index
  df <- df %>% dplyr::select(-X)
  # add file name
  df$file <- stringr::str_replace(basename(file), '_microbenchmark.csv', '')
  return(df)
}))

# wrangle dataframe
df <- df %>% dplyr::select(file, everything())
df <- df %>% tidyr::separate_wider_delim(file, delim = '_', names = c('writer', 'session', 'prompt', 'repetition'), cols_remove = FALSE)
df <- df %>% dplyr::mutate(expr = as.factor(ifelse(expr=="processDocument(doc)", 'document', 'component')))
df <- df %>% dplyr::mutate(mean = as.numeric(mean))
df <- df %>% 
  dplyr::select(writer, session, prompt, repetition, file, expr, mean) %>% 
  tidyr::pivot_wider(names_from = expr, values_from = mean)
df <- df %>% dplyr::mutate(percentage_decrease = 100 * (document - component) / document)
df <- df %>% dplyr::mutate(times_faster = document / component)

# plot
p <- df %>% ggplot(aes(x=prompt, y=percentage_decrease)) + 
  geom_boxplot() +
  labs(x = 'document', y = 'percentage decrease') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

df %>% ggplot(aes(x=prompt, y=times_faster)) + 
  geom_boxplot() +
  labs(x = 'prompt', y = 'times faster') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave('examples/check_process_by_component/plots/times_faster.png')

# Generate the csafe_docs data frame and save in the data folder. Requires
# that all CSAFE documents be saved in the input_dir. Do not rename any of the 
# documents.

input_dir <- "~/Documents/non_version_control/CSAFE/data_portal/downloaded"
csafe_docs = data.frame(doc = list.files(input_dir, pattern = ".png"))
csafe_docs = csafe_docs %>% tidyr::separate(doc, into=c("writer", "session", "prompt", "repetition"), extra="drop", remove = FALSE)
usethis::use_data(csafe_docs, overwrite = TRUE)
# Generate the csafe_docs data frame and save in the data folder. Requires
# that all CSAFE documents be saved in the input_dir. Do not rename any of the 
# documents.

library(dplyr)


input_dir <- "/Volumes/T7 Shield/CSAFE/handwriting/CSAFE_handwriting_database"

# list docs
docs <-list.files(input_dir, pattern="*.png")

# make data frame
csafe_docs <- data.frame(doc = docs)

# fix reps
csafe_docs <- csafe_docs %>% 
  dplyr::mutate(doc = stringr::str_replace_all(doc, "r1", "r01"),
                doc = stringr::str_replace_all(doc, "r2", "r02"),
                doc = stringr::str_replace_all(doc, "r3", "r03"))

# extract writer, session, prompt, and repetition
csafe_docs <- csafe_docs %>% 
  tidyr::separate(doc, into=c("writer", "session", "prompt", "repetition"), extra="drop", remove = FALSE)

# remove any duplicate rows
csafe_docs <- csafe_docs %>% dplyr::distinct()

# save to data folder
usethis::use_data(csafe_docs, overwrite = TRUE)

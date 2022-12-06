# Generate the csafe_docs data frame and save in the data folder. Requires
# that all CSAFE documents be saved in the input_dir. Do not rename any of the 
# documents.

input_dir <- "/Volumes/lss/research/csafe-handwriting/Data_Processing/Stage4_Cropped/Writing/Complete"

# list writer folders
writers <- dir(input_dir, recursive = FALSE, full.names = TRUE)
writers <- writers[grepl("w\\d+", writers)]  # remove any non-writer folders

# list docs
docs <- unlist(sapply(writers, function(x) list.files(x, pattern = ".png"), USE.NAMES = FALSE))

# make data frame
csafe_docs <- data.frame(doc = docs)
csafe_docs = csafe_docs %>% 
  tidyr::separate(doc, into=c("writer", "session", "prompt", "repetition"), extra="drop", remove = FALSE)

# make reps 
csafe_docs <- csafe_docs %>% 
  dplyr::mutate(doc = stringr::str_replace_all(doc, "r1", "r01"),
                doc = stringr::str_replace_all(doc, "r2", "r02"),
                doc = stringr::str_replace_all(doc, "r3", "r03"))

csafe_docs <- csafe_docs %>% 
  dplyr::mutate(repetition = stringr::str_replace_all(repetition, "r1", "r01"),
                repetition = stringr::str_replace_all(repetition, "r2", "r02"),
                repetition = stringr::str_replace_all(repetition, "r3", "r03"))

# save to data folder
usethis::use_data(csafe_docs, overwrite = TRUE)

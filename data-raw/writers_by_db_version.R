main_dir <- "/Volumes/lss/research/csafe-handwriting/Data_Permanent_Snapshot/newproc/processed_scans/datashare_snapshot_with_versions/versions"

vs <- list.dirs(main_dir, full.names = TRUE, recursive = FALSE)

sessions <- c("session1", "session2", "session3")

d <- vs[1]
s <- sessions[1]

files <- lapply(vs, function(d) {
  unlist(lapply(sessions, function(s) {
    list.files(file.path(d, s), pattern = "\\.png", full.names = FALSE)
  }))
})
names(files) <- basename(vs)

csafe_handwriting_db <- files
usethis::use_data(csafe_handwriting_db)

writers <- get_csafe_writerIDs(csafe_handwriting_db[["version1"]], only_unique=TRUE)
write.csv(writers, "~/Documents/CSAFE_handwriting_db_version1_writers.csv", row.names = FALSE)

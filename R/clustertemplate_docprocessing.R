#' processDocsForClustering
#'
#' Some stuff here
processDocsForClustering = function(documentDirectory, logDirectory, dataDirectory, numCores){

  doParallel::registerDoParallel(numCores)
  
  # Folder containing PNG images of handwriting samples
  filenames <- list.files(documentDirectory, pattern="*.png", full.names=TRUE)
  
  # Remove handwriting samples that already have an rds file from the list
  #completefiles = list.files(file.path("rdata","doclists"), pattern="*.rds", full.names=FALSE) 
  #completefiles = substr(basename(completefiles), start = 0, stop = nchar(basename(completefiles))-13)
  #filenames = filenames[which(!(substr(basename(filenames), start = 0, stop = nchar(basename(filenames))-4) %in% completefiles))]
  
  #Set log and data names if not provided
  if (logDirectory == "") logDirectory = documentDirectory
  if (dataDirectory == "") dataDirectory = documentDirectory
  
  #Extract graphs from each handwriting sample
  trash = foreach(i = 1:length(filenames), .packages =c('stringr', 'handwriter')) %dopar% {
    logname = basename(str_replace(filenames[i], '.png', ''))
    dir.create(file.path(logDirectory, "logs"), showWarnings = FALSE, recursive = TRUE)
    sink(file.path(logDirectory, "logs", paste0(logname, ".txt")), append=TRUE)
    cat("Document ", basename(filenames[i]), " is being processed. \t (", i, " of ", length(filenames) , ")\n", sep = "")
    
    doc = list()
    doc$image = readPNGBinary(filenames[i])
    doc$thin = thinImage(doc$image)
    
    # Get paths and graphemes, as well as breakpoints and extra nodes
    proclist = list()
    proclist[["process"]] = processHandwriting(doc$thin, dim(doc$image))
    proclist[["docname"]] = substr(basename(filenames[i]), start = 0, stop = nchar(basename(filenames[i]))-4)
    proclist[["thin"]] = doc$thin
    proclist[["image"]] = doc$image
    dir.create(file.path(dataDirectory, "rdata", "doclists"), showWarnings = FALSE, recursive = TRUE)
    saveRDS(object = proclist, file = file.path(dataDirectory, "rdata", "doclists", paste0(substr(basename(filenames[i]), start = 0, stop = nchar(basename(filenames[i]))-4), "_proclist.rds")))
    cat("Document ", basename(filenames[i]), " features saved.")
    gc()
 }
  
  
  #################### combine everything to one big list ####################
  templateproclist = list.files(file.path(dataDirectory, "rdata", "doclists"), pattern = "*_proclist.rds", full.names = TRUE) %>%
    map(readRDS) %>%
    list_merge()

  saveRDS(templateproclist, file = file.path(dataDirectory, "rdata", paste0(basename(dataDirectory), ".rds")))
  
  closeAllConnections()
  cat("done")
}



#### Magick code
# resize large document images - WILL WRITE OVER ORIGINALS - resizes to 72 pixels per square inch
# cd filepath2images
# for f in *.png; do convert -units PixelsPerInch "$f" -density 72 "$f"; done

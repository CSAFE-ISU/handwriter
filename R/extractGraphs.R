
# EXPORTED ----------------------------------------------------------------


#' Extract Graphs
#' 
#' `r lifecycle::badge("superseded")`
#' 
#' Development on `extractGraphs()` is complete. We recommend using `process_batch_dir()` instead.
#' 
#' Extracts graphs from .png images and saves each by their respective writer.
#' 
#' @import foreach
#' @import doParallel
#' 
#' @param source_folder path to folder containing .png images
#' @param save_folder path to folder where graphs are saved to
#' @return saves graphs in an rds file
#' @examples 
#' \dontrun{
#' sof = "path to folder containing .png images"
#' saf = "path to folder where graphs will be saved to"
#' extractGraphs(sof, saf)
#' }
#' @export
extractGraphs = function(source_folder = getwd(), save_folder = getwd()){
  # bind global variable to fix check() note
  i <- NULL
  
  doParallel::registerDoParallel(1)
  #setwd(source_folder)
  filenames = as.list(list.files(source_folder, pattern = ".png"))
  
  foreach(i = 1:length(filenames)) %dopar%{
    path = paste0(source_folder,"/", filenames[[i]])
    writer_id = paste0(substr(basename(path), start = 0, stop = nchar(basename(path))-17))
    
    #setwd(save_folder)
    ifelse(!dir.exists(file.path(save_folder, writer_id)), dir.create(file.path(save_folder, writer_id)), FALSE)
    graph_writer = paste0(save_folder, writer_id, "/")
    
    getGraphs(filenames[[i]], source_folder = source_folder, save_folder = graph_writer)
  }
}


# Internal Functions ------------------------------------------------------

#' Extracts graphs from a .png image using handwriter functions
#' 
#' extractGraphs
#' 
#' @param image .png image to be processed
#' @param source_folder path to folder containing .png images
#' @param save_folder path to folder where graphs are saved to
#' @return saves proclist to an rds file
#' 
#' @noRd
getGraphs = function(image, source_folder = getwd(), save_folder = getwd()){
  #setwd(source_folder)
  doc = proclist = list() 
  doc$image = readPNGBinary(image)
  doc$thin = thinImage(doc$image)
  proclist[["process"]] = processHandwriting(doc$thin, dim(doc$image))
  proclist[["docname"]] = substr(basename(image), start = 0, stop = nchar(basename(image))-4)
  proclist[["thin"]] = doc$thin
  proclist[["image"]] = doc$image
  
  #setwd(save_folder)  
  saveRDS(object = proclist, file = paste0(save_folder, "/", substr(basename(image), start = 0, stop = nchar(basename(image))-4), "_proclist.rds"))
  gc()
}

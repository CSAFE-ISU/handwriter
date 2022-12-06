#' process_batch_list
#'
#' @param image_list A list of image file paths
#' @param output_dir A directory to save the processed images
#' @param transform_output The type of transformation to perform on the output
#' @return A list of processed documents. The list is also saved to output_dir.
#'
#' @keywords ?
#' @export
process_batch_list <- function(image_list, output_dir, transform_output = "document") {
  document_list <- lapply(image_list, read_and_process, transform_output)

  # Save as RDS while renaming with _proclist suffix
  if (!dir.exists(batch_output_dir)) dir.create(batch_output_dir, recursive = TRUE)
  for (i in 1:length(document_list)) {
    saveRDS(document_list[[i]],
      file = paste0(batch_output_dir, "/", paste0(tools::file_path_sans_ext(document_list[[i]]$docname), "_proclist.rds"))
    )
  }

  return(document_list)
}

#' process_batch_dir
#'
#' @param input_dir Input directory that contains images
#' @param output_dir A directory to save the processed images
#' @param return_result TRUE/FALSE whether to return the result. If TRUE, the
#'   processed documents with be saved and a list of the processed documents
#'   will be returned. If FALSE, the processed documents will be saved, but
#'   nothing will be returned.
#' @param transform_output The type of transformation to perform on the output
#' @return A list of processed documents. The list is also saved to output_dir.
#'
#' @keywords ?
#' @export
process_batch_dir <- function(input_dir, output_dir = ".", return_result = TRUE, transform_output = "document") {
  message("Listing documents to be processed...")
  file_list <- list.files(input_dir, full.names = TRUE)

  if (!dir.exists(output_dir)) {
    message("Creating output directory...")
    dir.create(output_dir, recursive = TRUE)
  }

  # Save as RDS while renaming with _proclist suffix
  # Skip if a processed file with that name already exists in output_dir
  document_list <- list()
  counter <- 1
  for (i in 1:length(file_list)) {
    # format path and file name for output
    outfile <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(file_list[[i]])), "_proclist.rds"))
    # if output file doesn't already exist, process the input file
    if (!file.exists(outfile)) {
      message(sprintf("Processing document %d...", i))
      doc <- read_and_process(file_list[[i]], transform_output)
      message(sprintf("Saving processed document %d...", i))
      saveRDS(doc, file = outfile)
      document_list[[counter]] <- doc
      counter <- counter + 1
    } else {
      message(sprintf("Document %d had already been processed...", i))
    }
  }

  # Return list
  if (return_result) {
    output_list <- list.files(output_dir, full.names = TRUE)
    # If document_list doesn't contain all docs in output folder, load all docs in folder
    if (length(document_list) < length(output_list)) {
      message("Loading processed documents...")
      document_list <- lapply(output_list, readRDS)
    }
    return(document_list)
  }
}


#' read_and_process
#'
#' @param image_name The file path to an image
#' @param transform_output The type of transformation to perform on the output
#' @return A list of the processed image components
#'
#' @keywords ?
#' @export
read_and_process <- function(image_name, transform_output) {
  document <- list()

  document$image <- readPNGBinary(image_name)
  document$thin <- thinImage(document$image)
  processList <- processHandwriting(document$thin, dim(document$image))

  if (transform_output == "document") {
    document$process <- processList
    document$docname <- basename(image_name)
    return(document)
  }

  processList$docname <- basename(image_name)
  return(processList)
}

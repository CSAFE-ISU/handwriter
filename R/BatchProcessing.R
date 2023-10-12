
# EXPORTED ----------------------------------------------------------------


#' Process Batch List
#' 
#' Process a list of handwriting samples saved as PNG images: 
#'     (1) Load the image and convert it to black and white with [`readPNGBinary()`]
#'     (2) Thin the handwriting to one pixel in width with [`thinImage()`]
#'     (3) Run [`processHandwriting()`] to split the handwriting into parts called *edges* and place *nodes* at the ends of 
#'     edges. Then combine edges into component shapes called *graphs*.
#'     (4) Save the processed document in an RDS file.
#'     (5) Optional. Return a list of the processed documents.
#' 
#' @param images A vector of image file paths
#' @param output_dir A directory to save the processed images
#' @param return_result TRUE/FALSE whether to return the result. If TRUE, the
#'   processed documents with be saved and a list of the processed documents
#'   will be returned. If FALSE, the processed documents will be saved, but
#'   nothing will be returned.
#' @return A list of processed documents
#'
#' @examples
#' \dontrun{
#' images <- c('path/to/image1.png', 'path/to/image2.png', 'path/to/image3.png')
#' process_batch_list(images, "path/to/output_dir", FALSE)
#' docs <- process_batch_list(images, "path/to/output_dir", TRUE)
#' }
#'
#' @export
process_batch_list <- function(images, output_dir, return_result = TRUE) {
  if (!dir.exists(output_dir)) {
    message("Creating output directory...")
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Save as RDS while renaming with _proclist suffix
  # Skip if a processed file with that name already exists in output_dir
  document_list <- list()
  counter <- 1
  for (i in 1:length(images)) {
    # format path and file name for output
    outfile <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(images[[i]])), "_proclist.rds"))
    # if output file doesn't already exist, process the input file
    if (!file.exists(outfile)) {
      message(sprintf("Processing document %d...", i))
      doc <- processDocument(images[[i]])
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

#' Process Batch Directory
#' 
#' Process a list of handwriting samples saved as PNG images in a directory: 
#'     (1) Load the image and convert it to black and white with [`readPNGBinary()`]
#'     (2) Thin the handwriting to one pixel in width with [`thinImage()`]
#'     (3) Run [`processHandwriting()`] to split the handwriting into parts called *edges* and place *nodes* at the ends of 
#'     edges. Then combine edges into component shapes called *graphs*.
#'     (4) Save the processed document in an RDS file.
#'     (5) Optional. Return a list of the processed documents.
#'
#' @param input_dir Input directory that contains images
#' @param output_dir A directory to save the processed images
#' @param return_result TRUE/FALSE whether to return the result. If TRUE, the
#'   processed documents with be saved and a list of the processed documents
#'   will be returned. If FALSE, the processed documents will be saved, but
#'   nothing will be returned.
#' 
#' @return A list of processed documents
#' 
#' @examples
#' \dontrun{
#' process_batch_list("path/to/input_dir", "path/to/output_dir", FALSE)
#' docs <- process_batch_list("path/to/input_dir", "path/to/output_dir", TRUE)
#' }
#'
#' @export
process_batch_dir <- function(input_dir, output_dir = ".", return_result = TRUE) {
  message("Listing documents to be processed...")
  file_list <- list.files(input_dir, full.names = TRUE)

  document_list <- process_batch_list(images=file_list, 
                                      output_dir=output_dir, 
                                      return_result = return_result)
  return(document_list)
}


#' Read and Process
#' 
#' @description
#' `r lifecycle::badge("superseded")`
#' 
#' Development on `read_and_process()` is complete. We recommend using [`processDocument()`].
#' `read_and_process(image_name, "document")` is equivalent to `processDocument(image_name)`.
#'
#' @param image_name The file path to an image
#' @param transform_output The type of transformation to perform on the output
#' @return A list of the processed image components
#' 
#' @examples
#' # use handwriting example from handwriter package
#' image_path <- system.file("extdata", "phrase_example.png", package = "handwriter")
#' doc <- read_and_process(image_path, "document")
#' 
#' @export
#' @md
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

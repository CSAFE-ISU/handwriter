# The handwriter R package performs writership analysis of handwritten documents. 
# Copyright (C) 2021 Iowa State University of Science and Technology on behalf of its Center for Statistics and Applications in Forensic Evidence
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


# EXPORTED ----------------------------------------------------------------


#' Process Batch List
#'
#' Process a list of handwriting samples saved as PNG images: (1) Load the image
#' and convert it to black and white with [`readPNGBinary()`] (2) Thin the
#' handwriting to one pixel in width with [`thinImage()`] (3) Run
#' [`processHandwriting()`] to split the handwriting into parts called *edges*
#' and place *nodes* at the ends of edges. Then combine edges into component
#' shapes called *graphs*. (4) Save the processed document in an RDS file. (5)
#' Optional. Return a list of the processed documents.
#'
#' @param images A vector of image file paths
#' @param output_dir A directory to save the processed images
#' @param skip_docs_on_retry Logical whether to skip documents in the images arguement that
#'   caused errors on a previous run. The errors and document names are stored
#'   in output_dir > problems.txt. If this is the first run,
#'   `process_batch_list` will attempt to process all documents in the images arguement.
#'
#' @return No return value, called for side effects
#'
#' @examples
#' \dontrun{
#' images <- c('path/to/image1.png', 'path/to/image2.png', 'path/to/image3.png')
#' process_batch_list(images, "path/to/output_dir", FALSE)
#' process_batch_list(images, "path/to/output_dir", TRUE)
#' }
#'
#' @export
#' @md
process_batch_list <- function(images, output_dir, skip_docs_on_retry=TRUE) {
  # output directory
  create_dir(output_dir, msg = "Creating output directory...", recursive = TRUE)
  
  # skip docs that have already been processed
  outfiles <- sapply(images, function(img) file.path(output_dir, paste0(tools::file_path_sans_ext(basename(img)), "_proclist.rds")), USE.NAMES = FALSE)
  outfiles_exist <- sapply(outfiles, function(f) file.exists(f), USE.NAMES = FALSE)
  images <- images[!outfiles_exist]
  outfiles <- outfiles[!outfiles_exist]
  # exit if all images have been processed
  if (length(images) == 0) {
    message('All documents have been processed or flagged as problem files.')
    return()
  }
  
  # list problem docs
  prob_log_file <- file.path(output_dir, 'problems.txt')
  if (file.exists(prob_log_file)){
    problem_docs <- get_prob_docs_from_log(prob_log_file)
  } else {
    problem_docs <- c()
  }
  
  # skip problem docs (optional)
  if (skip_docs_on_retry){
    images <- remove_prob_docs_from_list(problem_docs, images)
    # exit if all images have been processed
    if (length(images) == 0) {
      message('All documents have been processed or flagged as problem documents.')
      return()
    }
  }
  
  # start printing to log
  if (file.exists(prob_log_file)){
    sink(prob_log_file, append=TRUE)
  } else {
    sink(prob_log_file, append=FALSE)
  }
  
  # Save as RDS while renaming with _proclist suffix
  for (i in 1:length(images)) {
    possibleError <- tryCatch(
      expr = { 
        image <- images[[i]]
        outfile <- outfiles[[i]]
        message(sprintf("Processing document %s...", basename(image)))
        doc <- processDocument(image)
        message(sprintf("Saving processed document %s...\n", basename(image)))
        saveRDS(doc, file = outfile)
      }, 
      error = function(e) {
        cat(paste0("error with document ", basename(images[[i]]), " ", e))
      })
  }
  
  # stop sinking to problem log file
  sink(file = NULL, type = "output")
  sink(file = NULL, type = "message")
  
  # show list of problem docs from log NOTE: Initially, I added problem doc names
  # to a vector inside the error part of trycatch and returned the list of
  # problem docs as the function output. But if batch_process_list is run
  # multiple times on the same input / out dirs, then only errors from the most recent
  # run would be returned.
  show_problem_docs(prob_log_file)
  
  return()
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
#' @param skip_docs_on_retry Logical whether to skip documents in input_dir that
#'   caused errors on a previous run. The errors and document names are stored
#'   in output_dir > problems.txt. If this is the first run,
#'   `process_batch_list` will attempt to process all documents in input_dir.
#' 
#' @return No return value, called for side effects
#' 
#' @examples
#' \dontrun{
#' process_batch_dir("path/to/input_dir", "path/to/output_dir")
#'
#' @export
#' @md
process_batch_dir <- function(input_dir, output_dir = ".", skip_docs_on_retry=TRUE) {
  message("Listing documents to be processed...")
  file_list <- list.files(input_dir, pattern = "(.PNG|.png)$", full.names = TRUE)
  
  process_batch_list(images=file_list, 
                     output_dir=output_dir,
                     skip_docs_on_retry=skip_docs_on_retry)
  return()
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


# INTERNAL ----------------------------------------------------------------


#' Get Document Name from Line
#'
#' This is an internal function to retrieve the document name from a line of
#' text in the log file problems.txt created by `process_batch_list`. Problem
#' documents are added to the log with a statement "error with document
#' <docname>..." This function searches the line of text for the string "error
#' with document" and returns the the first word after the string.
#'
#' @param line
#'
#' @return document name or NA
#'
#' @noRd
get_docname_from_line <- function(line) {
  docname <- stringr::str_extract(line, "(?<=error with document )(.+[p|P][n|N][g|G])")
  return(docname)
}

#' Get Problem Documents from Log File
#' 
#' Internal function to read the problem log file created by `process_batch_list`
#' and return a vector of problem document names. Problem documents are added to 
#' the log with a statement "error with document <docname>..." This function 
#' searches for the string of text "error with document" and returns the the 
#' first word after the string.
#'
#' @param log_file File path to log file
#'
#' @return Vector of problem documents
#'
#' @noRd
get_prob_docs_from_log <- function(log_file){
  lines <- readLines(log_file)
  problem_docs <- unlist(lapply(lines, get_docname_from_line))
  problem_docs <- problem_docs[!is.na(problem_docs)]
  return(problem_docs)
}

remove_prob_docs_from_list <- function(problem_docs, images){
  images <- images[!(basename(images) %in% problem_docs)]
  return(images)
}

show_problem_docs <- function(prob_log_file) {
  problem_docs <- get_prob_docs_from_log(prob_log_file)
  if (length(problem_docs) == 0){
    message("All documents were successfully processed...\n")
  } else{
    message("The following documents could not be processed: ", paste0(problem_docs, collapse = ', '))
  }
}



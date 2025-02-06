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
#' sof <- "path to folder containing .png images"
#' saf <- "path to folder where graphs will be saved to"
#' extractGraphs(sof, saf)
#' }
#' @export
extractGraphs <- function(source_folder = getwd(), save_folder = getwd()) {
  # bind global variable to fix check() note
  i <- NULL

  doParallel::registerDoParallel(1)
  # setwd(source_folder)
  filenames <- as.list(list.files(source_folder, pattern = ".png", full.names = TRUE))

  foreach(i = 1:length(filenames)) %dopar% {
    path <- file.path(source_folder, filenames[[i]])
    writer_id <- paste0(substr(basename(path), start = 0, stop = nchar(basename(path)) - 17))

    # setwd(save_folder)
    ifelse(!dir.exists(file.path(save_folder, writer_id)), dir.create(file.path(save_folder, writer_id)), FALSE)
    graph_writer <- file.path(save_folder, writer_id)

    getGraphs(filenames[[i]], save_folder = graph_writer)
  }
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
  } else {
    processList$docname <- basename(image_name)
    return(processList)
  }
}


# Internal Functions ------------------------------------------------------

#' Extracts graphs from a .png image using handwriter functions
#'
#' `r lifecycle::badge("superseded")`
#'
#' Development on `getGraphs()` is complete. We recommend using `processDocument()` instead.
#'
#' @param image .png image to be processed
#' @param save_folder path to folder where graphs are saved to
#' @return saves proclist to an rds file
#'
#' @noRd
getGraphs <- function(image, save_folder = getwd()) {
  doc <- proclist <- list()
  doc$image <- readPNGBinary(image)
  doc$thin <- thinImage(doc$image)
  proclist[["image"]] <- doc$image
  proclist[["thin"]] <- doc$thin
  proclist[["process"]] <- processHandwriting(doc$thin, dim(doc$image))
  proclist[["docname"]] <- substr(basename(image), start = 0, stop = nchar(basename(image)) - 4)

  saveRDS(object = proclist, file = paste0(save_folder, "/", substr(basename(image), start = 0, stop = nchar(basename(image)) - 4), "_proclist.rds"))
  gc()
  
  return(proclist)
}

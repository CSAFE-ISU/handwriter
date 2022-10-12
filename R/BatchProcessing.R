#' process_batch_list
#'
#' @param image_list A list of image file paths
#' @param output_dir A directory to save the processed images
#' @param transform_output The type of transformation to perform on the output
#' @return A list of processed documents. The list is also saved to output_dir.
#'
#' @keywords ?
#' @export
process_batch_list = function(image_list, output_dir, transform_output = 'document'){
  document_list = lapply(image_list, read_and_process, transform_output)
  
  #Save as RDS while renaming with _proclist suffix
  if (!dir.exists(batch_output_dir)) dir.create(batch_output_dir, recursive = TRUE)
  for(i in 1:length(document_list)){
    saveRDS(document_list[[i]], 
            file=paste0(batch_output_dir, '/', paste0(tools::file_path_sans_ext(document_list[[i]]$docname),'_proclist.rds')))
  }
  
  return(document_list)
}

#' process_batch_dir
#'
#' @param input_dir Input directory that contains images
#' @param output_dir A directory to save the processed images
#' @param transform_output The type of transformation to perform on the output
#' @return A list of processed documents. The list is also saved to output_dir.
#'
#' @keywords ?
#' @export
process_batch_dir = function(input_dir, output_dir = '.', transform_output = 'document'){
  file_list = list.files(batch_input_dir, full.names = TRUE)
  
  document_list = lapply(file_list, read_and_process, transform_output)
  
  #Save as RDS while renaming with _proclist suffix
  if (!dir.exists(batch_output_dir)) dir.create(batch_output_dir, recursive = TRUE)
  for(i in 1:length(document_list)){
    saveRDS(document_list[[i]], 
            file=paste0(batch_output_dir, '/', paste0(tools::file_path_sans_ext(document_list[[i]]$docname),'_proclist.rds')))
  }
  
  return(document_list)
}


#' read_and_process
#'
#' @param image_name The file path to an image
#' @param transform_output The type of transformation to perform on the output
#' @return A list of the processed image components
#'
#' @keywords ?
#' @export
read_and_process = function(image_name, transform_output){
  document = list()
  
  document$image = readPNGBinary(image_name)
  document$thin = thinImage(document$image)
  processList = processHandwriting(document$thin, dim(document$image))
  
  if(transform_output == 'document'){
    document$process = processList
    document$docname = basename(image_name)
    return(document)
  }
  
  processList$docname = basename(image_name)
  return(processList)
}



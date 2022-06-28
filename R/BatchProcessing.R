#' process_batch
#'
#'
#'
#' @param batch_input_dir Input directory
#' @param batch_output_dir Output directory
#' @param transform_output
#' @return 
#'
#' @keywords ?
#' @export
process_batch = function(batch_input_dir, batch_output_dir, transform_output = 'none'){
  file_list = list.files(batch_input_dir)
  document_list = lapply(paste0(batch_input_dir,'/', file_list), read_and_process, transform_output)
  names(document_list) <- names(file_list)
  
  #Save as RDS while renaming with _proclist suffix
  if (!dir.exists(batch_output_dir)) dir.create(batch_output_dir)
  for(i in 1:length(document_list)){
    saveRDS(document_list[[i]], file=paste0(batch_output_dir, '/', paste0(tools::file_path_sans_ext(document_list[[i]]$docname),'_proclist.rds')))
  }
  
  #Write to folder
  View(document_list)
}

#' process_batch
#'
#' @param image_name Input directory
#' @param transform_output
#' @return 
#'
#' @keywords ?
#' @export
read_and_process = function(image_name, transform_output = 'none'){
  document = list()
  
  document$image = readPNGBinary(image_name)
  document$thin = thinImage(document$image)
  processList = processHandwriting(csafe_document$thin, dim(csafe_document$image))
  
  if(transform_output == 'document'){
    document$docname = basename(image_name)
    document$process = processList
    return(document)
  }
  
  return(processList)
}



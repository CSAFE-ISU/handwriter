#' process_batch_list
#'
#' @param batch_input_dir Input directory as a list from shiny
#' @param transform_output
#' @return 
#'
#' @keywords ?
#' @export
process_batch_list = function(image_batch, name_list = NULL, transform_output = 'none'){
  document_list = lapply(image_batch, read_and_process, transform_output)
  
  #Fix Naming (with a cute lamda function to rip)
  if(!is.null(name_list)){
   names(document_list) =  (function(x) paste0(tools::file_path_sans_ext(name_list),'_proclist.rds')) (name_list)
  }
  
  return(document_list)
}

#' process_batch_dir
#'
#' @param batch_input_dir Input directory as a list of datapaths or a directory
#' @param batch_output_dir Output directory
#' @param transform_output
#' @return 
#'
#' @keywords ?
#' @export
process_batch_dir = function(image_batch, batch_output_dir = '.', transform_output = 'none'){
  file_list = list.files(image_batch)
  file_list = lapply(file_list, function(x) paste0(image_batch, '/', x))
  
  document_list = lapply(file_list, read_and_process, transform_output)
  
  #Save as RDS while renaming with _proclist suffix
  if (!dir.exists(batch_output_dir)) dir.create(batch_output_dir, recursive = TRUE)
  for(i in 1:length(document_list)){
    saveRDS(document_list[[i]], file=paste0(batch_output_dir, '/', paste0(tools::file_path_sans_ext(document_list[[i]]$docname),'_proclist.rds')))
  }
}


#' read_and_process
#'
#' @param image_name Input directory
#' @param transform_output
#' @return 
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



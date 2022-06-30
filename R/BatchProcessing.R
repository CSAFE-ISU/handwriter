#' process_batch
#'
#'
#'
#' @param batch_input_dir Input directory as a list of datapaths or a directory
#' @param batch_output_dir Output directory
#' @param transform_output
#' @return 
#'
#' @keywords ?
#' @export
process_batch = function(image_batch, batch_output_dir = '.', transform_output = 'none'){
  #Logic to separate image_batch as list of filenames or a directory
  print(typeof(image_batch))
  if(!is.list(image_batch)){
    file_list = list.files(image_batch)
    file_list = lapply(file_list, function(x) paste0(image_batch, '/', x))
  }else{ 
    file_list = image_batch
  }
  
  document_list = lapply(file_list, read_and_process, transform_output)
  names(document_list) <- names(file_list)
  
  #Save as RDS while renaming with _proclist suffix
  if (!dir.exists(batch_output_dir)) dir.create(batch_output_dir)
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
  processList = processHandwriting(csafe_document$thin, dim(csafe_document$image))
  
  if(transform_output == 'document'){
    document$docname = basename(image_name)
    document$process = processList
    return(document)
  }
  
  return(processList)
}



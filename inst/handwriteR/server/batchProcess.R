# ==================================================================
# ======================== BATCH PROCESSING ========================
# ==================================================================

values$processed_docs = NULL

#UPLOAD
observeEvent(input$process_batch, {
  datapath_list <- list(input$batch_input_dir$datapath)
  values$processed_docs = process_batch_list(datapath_list[[1]], "document")
})

#Download
output$save_batch <- downloadHandler(
  filename = function(){
    paste0("processed_batch-", Sys.Date(), ".rda")
  },
  content = function(file) {
    message(paste0("Writing file: ", "processed_batch-", Sys.Date(), ".rda"))
    proc_list = isolate(values$processed_docs)
    save(proc_list, file = file)
  }
)



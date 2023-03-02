# UPDATE: main directory on questioned documents page ----
shinyDirChoose(
  input,
  'q_main_dir_qd',
  roots = c(home = '~'),
  filetypes = c('', 'png', 'txt', 'bigWig', "tsv", "csv", "bw")
)
q_main_dir_qd <- reactive(input$q_main_dir_qd)
observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$q_main_dir_qd
             },
             handlerExpr = {
               if (!"path" %in% names(q_main_dir_qd())) return()
               home <- normalizePath("~")
               analysis$q_main_datapath <-
                 file.path(home, paste(unlist(q_main_dir_qd()$path[-1]), collapse = .Platform$file.sep))
             })

# RENDER: main directory on model qd page ----
output$dir_qd <- renderText({
  analysis$q_main_datapath
})


#======================= QUESTIONED DOCS =============================
# UPDATE: questioned documents directory ----
shinyDirChoose(
  input,
  'q_questioned_docs',
  roots = c(home = '~'),
  filetypes = c('png')
)
q_questioned_docs <- reactive(input$q_questioned_docs)
observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$q_questioned_docs
             },
             handlerExpr = {
               if (!"path" %in% names(q_questioned_docs())) return()
               home <- normalizePath("~")
               analysis$q_questioned_docs <-
                 file.path(home, paste(unlist(q_questioned_docs()$path[-1]), collapse = .Platform$file.sep))
             })

# UPDATE: current qd writer
observe({
  analysis$q_current_writer <- substr(input$q_select_qd, input$q_writer_start_qd, input$q_writer_end_qd)
})

# BUTTON: analyze questioned documents ----
observeEvent(input$q_analyze_docs, {
  if ( !file.exists(file.path(analysis$q_main_datapath, "data", "model.rds"))){
    showNotification("The main directory does not contain a trained model. Create a model with the Tools menu.", 
                     duration = analysis$msg_duration)
    
  } else {
    analysis$q_model <- readRDS(file.path(analysis$q_main_datapath, "data", "model.rds"))
    analysis$q_results <- analyze_questioned_documents(template_dir = analysis$q_main_datapath,
                                                       questioned_images_dir = analysis$q_questioned_docs,
                                                       model = analysis$q_model,
                                                       num_cores = as.integer(input$q_num_cores_qd),
                                                       num_graphs = "All",
                                                       writer_indices = c(input$q_writer_start_qd, input$q_writer_end_qd),
                                                       doc_indices = c(input$q_doc_start_qd, input$q_doc_end_qd))
    showNotification("Finished analyzing questioned document(s).", duration = analysis$msg_duration)
  }
})

# RENDER: questioned docs directory ----
output$q_questioned_docs <- renderText({
  analysis$q_questioned_docs
})

# RENDER: questioned documents file names ----
output$q_questioned_docs_list <- renderTable({ 
  if ( !is.null(analysis$q_questioned_docs) ){
    data.frame("filename"=list.files(analysis$q_questioned_docs))
  }
})

# RENDER: current qd writer ----
output$q_writer <- renderPrint({
  analysis$q_current_writer
})

# RENDER: questioned docs cluster fill counts
output$q_questioned_docs_cluster_fill_counts <- renderPlot({
  if ( !is.null(analysis$q_results)){
    plot_cluster_fill_counts(analysis$q_results, facet = TRUE)
  }
})

# RENDER: posterior probabilities plot
output$q_posterior_probabilities <- renderPlot({
  if ( !is.null(analysis$q_results)){
    plot_posterior_probabilities(analysis$q_results)
  }
})

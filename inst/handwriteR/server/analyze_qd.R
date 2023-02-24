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

# BUTTON: load model ----
observeEvent(input$q_load_model_qd, {
  if ( is.null(analysis$q_model) && file.exists(file.path(analysis$q_main_datapath, "data", "model.rds")) ){
    analysis$q_model <- readRDS(file.path(analysis$q_main_datapath, "data", "model.rds"))
    showNotification("Model loaded.", duration = analysis$msg_duration)
  } else {
    showNotification("The main directory does not contain a model.", duration = analysis$msg_duration)
  }
})

# RENDER: questioned docs directory ----
output$q_questioned_docs <- renderText({
  analysis$q_questioned_docs
})

# RENDER: questioned documents file names ----
output$q_questioned_docs_list <- renderPrint({ 
  if ( !is.null(analysis$q_questioned_docs) ){
    list.files(analysis$q_questioned_docs)
  }
})

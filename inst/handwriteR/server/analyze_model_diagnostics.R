# UPDATE: main directory on model diagnostics page ----
shinyDirChoose(
  input,
  'q_main_dir_diagnostics',
  roots = c(home = '~'),
  filetypes = c('', 'png', 'txt', 'bigWig', "tsv", "csv", "bw")
)
q_main_dir_diagnostics <- reactive(input$q_main_dir_diagnostics)
observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$q_main_dir_diagnostics
             },
             handlerExpr = {
               if (!"path" %in% names(q_main_dir_diagnostics())) return()
               home <- normalizePath("~")
               analysis$q_main_datapath <-
                 file.path(home, paste(unlist(q_main_dir_diagnostics()$path[-1]), collapse = .Platform$file.sep))
             })

# RENDER: main directory on model diagnostics page ----
output$dir_diagnostics <- renderText({
  analysis$q_main_datapath
})


#======================= MODEL DIAGNOSTICS =============================
# UPDATE: model variables for trace plot ----
observe({
  # trace plot variables
  if (!is.null(analysis$q_model)){
    updateSelectInput(session, "q_trace_variable", choices = names(as.data.frame(coda::as.mcmc(analysis$q_model$fitted_model[[1]]))))
  } else {
    updateSelectInput(session, "q_trace_variable", choices = c(NA))
  }
})

# BUTTON: load model ----
observeEvent(input$q_load_model, {
  if ( is.null(analysis$q_model) && file.exists(file.path(analysis$q_main_datapath, "data", "model.rds")) ){
    analysis$q_model <- readRDS(file.path(analysis$q_main_datapath, "data", "model.rds"))
    showNotification("Model loaded.", duration = analysis$msg_duration)
  } else {
    showNotification("The main directory does not contain a model.", duration = analysis$msg_duration)
  }
})

# RENDER: trace plot ----
output$q_trace_plot <- renderPlot({
  if (!is.null(analysis$q_model) && !is.na(input$q_trace_variable)){
    plot_trace(model=analysis$q_model, variable=input$q_trace_variable)
  }
})
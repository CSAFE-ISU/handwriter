# LOAD MODEL --------------------------------------------------------------
# BUTTON: load model ----
observeEvent(input$q_load_model, {
  file <- input$q_load_model
  ext <- tools::file_ext(file$datapath)
  req(file)
  validate(need(ext == "rds", "Please upload a rds file"))
  analysis$q_model <- readRDS(file$datapath)
})


# TRACE PLOT --------------------------------------------------------------
# UPDATE: model variables for trace plot ----
observe({
  # trace plot variables
  if (!is.null(analysis$q_model)){
    updateSelectInput(session, "q_trace_variable", choices = names(as.data.frame(coda::as.mcmc(analysis$q_model$fitted_model[[1]]))))
  } else {
    updateSelectInput(session, "q_trace_variable", choices = c(NA))
  }
})

# RENDER: trace plot ----
output$q_trace_plot <- renderPlot({
  if ( !is.null(analysis$q_model) ){
    plot_trace(model=analysis$q_model, variable=input$q_trace_variable)
  }
})


# BURN-IN -----------------------------------------------------------------
# BUTTON: drop burn-in ----
observeEvent(input$q_drop_burnin, {
  analysis$q_model <- drop_burnin(model = analysis$q_model, burn_in = input$q_select_burnin)
  shinyjs::enable("q_save_model");
})

output$q_save_model <- downloadHandler(
  filename = paste("model_", Sys.Date(), ".rds", sep=""),
  content = function(file) {
    saveRDS(analysis$q_model, file)
  }
)
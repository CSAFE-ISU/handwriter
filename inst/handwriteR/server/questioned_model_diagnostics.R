md_values <- reactiveValues()

observeEvent(input$md_load_model, {
  file <- input$md_load_model
  ext <- tools::file_ext(file$datapath)
  req(file)
  validate(need(ext == "rds", "Please upload a rds file"))
  md_values$model <- readRDS(file$datapath)
})

observe({
  # trace plot variables
  if ( !is.null(md_values$model) ){
    updateSelectInput(session, "md_param", choices = names(as.data.frame(coda::as.mcmc(md_values$model$fitted_model[[1]]))))
  } else {
    updateSelectInput(session, "md_param", choices = c(NA))
  }
})

output$md_trace <- renderPlot({
  if ( !is.null(md_values$model) ){
    plot_trace(input$md_param, model = md_values$model)
  }
})
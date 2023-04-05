poi_values <- reactiveValues()

# model training docs
poi_docs_path <- directoryServer("poi_docs")
directoryContentsServer("poi_docs_list", poi_docs_path)

# model settings
poi_writer_ind <- substringIndicesServer("poi_writer_indices")
substringsServer("poi_writers", poi_docs_path, poi_writer_ind, "writers")
poi_doccode_ind <- substringIndicesServer("poi_doccode_indices")
substringsServer("poi_doccodes", poi_docs_path, poi_doccode_ind, "doc code")

# output dir
poi_main_path <- directoryServer("poi_main_dir")

# fit model
observeEvent(input$poi_create, {
  # save model as reactive values so it can be used for other tasks
  poi_values$model <- fit_model(template_dir = poi_main_path(),
            model_images_dir = poi_docs_path(),
            num_iters = input$poi_num_iters,
            num_chains = input$poi_chains,
            num_cores = input$poi_cores,
            writer_indices = c(poi_writer_ind$start(), poi_writer_ind$stop()),
            doc_indices = c(poi_doccode_ind$start(), poi_doccode_ind$stop()))
  showNotification("Model saved as model.rds in Output Directory > data.")
})

# select and display image
displayImageServer("poi_image", poi_docs_path)

# display writer profiles from fitted model
output$poi_profiles <- renderPlot({
  if ( !is.null(poi_values$model) ){
    plot_credible_intervals(model=poi_values$model, facet=TRUE)
  }
})
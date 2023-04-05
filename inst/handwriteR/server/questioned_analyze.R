q_values <- reactiveValues()

# questioned docs
q_docs_path <- directoryServer("q_docs")
directoryContentsServer("q_docs_list", q_docs_path)

# output directory
q_main_path <- directoryServer("q_main_dir")

# load model
q_model <- loadServer("q_load_model")

# settings
q_writer_ind <- substringIndicesServer("q_writer_indices")
substringsServer("q_writers", q_docs_path, q_writer_ind, "writers")
q_doccode_ind <- substringIndicesServer("q_doccode_indices")
substringsServer("q_doccodes", q_docs_path, q_doccode_ind, "doc code")

# analyze
observeEvent(input$q_analyze, {
  q_values$analysis <- analyze_questioned_documents(
    template_dir = q_main_path(),
    questioned_images_dir = q_docs_path(),
    model = q_model(),
    num_cores = input$q_cores,
    num_graphs = "All",
    writer_indices = c(q_writer_ind$start(), q_writer_ind$stop()),
    doc_indices = c(q_doccode_ind$start(), q_doccode_ind$stop())
  )
  showNotification("Analysis saved as analysis.rds in Output Directory > data.")
})

# select and display image
displayImageServer("q_image", q_docs_path)

# questioned doc writer profiles
output$q_profiles <- renderPlot({
  if ( !is.null(q_values$analysis) ){
    plot_cluster_fill_counts(q_values$analysis, facet = TRUE)
  }
})

# POI writer profiles (same as plot on model tab)
output$q_poi_profiles <- renderPlot({
  if ( !is.null(q_model) ){
    plot_credible_intervals(q_model(), facet = TRUE)
  }
})

# Posterior probabilities of writership in dataframe
output$q_post_probs_df <- DT::renderDT({
  if ( !is.null(q_values$analysis) ){
    q_values$analysis$posterior_probabilities
  }
})

# Posterior probabilities of writership in plot
output$q_post_probs_plot <- renderPlot({
  if ( !is.null(q_values$analysis) ){
    plot_posterior_probabilities(q_values$analysis)
  }
})
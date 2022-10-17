analysis <- reactiveValues(q_main_dir = "/Users/stephanie/Documents/shiny_example",
                           q_template_images_dir = "/Users/stephanie/Documents/shiny_example/data/template_images",
                           q_template_graphs_dir = "/Users/stephanie/Documents/shiny_example/data/template_graphs",
                           q_templates = NULL,
                           q_template_file = NULL,
                           q_template_names = NULL,
                           q_template_current = NULL,
                           q_model_images_dir = "/Users/stephanie/Documents/shiny_example/data/model_images",
                           q_model_graphs_dir = "/Users/stephanie/Documents/shiny_example/data/model_graphs",
                           q_model_data = NULL,
                           q_questioned_images_dir = "/Users/stephanie/Documents/shiny_example/data/questioned_images",
                           q_questioned_graphs_dir = "/Users/stephanie/Documents/shiny_example/data/questioned_graphs",
                           q_questioned_data = NULL)

# ENABLE/DISABLE: get model data ----
observe({
  if (!is.null(analysis$q_templates)){
    shinyjs::enable("q_get_model_clusters")
  } else {
    shinyjs::disable("q_get_model_clusters")
  }
})

# ENABLE/DISABLE: save model data ----
observe({
  if (!is.null(analysis$q_model_data)){
    shinyjs::enable("q_save_model_clusters")
  } else {
    shinyjs::disable("q_save_model_clusters")
  }
})

# ENABLE/DISABLE: fit model ----
observe({
  if (!is.null(analysis$q_model_data)){
    shinyjs::enable("q_fit_model")
  } else {
    shinyjs::disable("q_fit_model")
  }
})

# ENABLE/DISABLE: save model ----
observe({
  if (!is.null(analysis$q_model)){
    shinyjs::enable("q_save_model")
  } else {
    shinyjs::disable("q_save_model")
  }
})

# ENABLE/DISABLE: get questioned data ----
observe({
  if (!is.null(analysis$q_templates) && !is.null(analysis$q_model_data)){
    shinyjs::enable("q_get_questioned_data")
  } else {
    shinyjs::disable("q_get_questioned_data")
  }
})

# ENABLE/DISABLE: save questioned data ----
observe({
  if (!is.null(analysis$q_questioned_data)){
    shinyjs::enable("q_save_questioned_data")
  } else {
    shinyjs::disable("q_save_questioned_data")
  }
})

# ENABLE/DISABLE: analyze questioned docs ----
observe({
  if (!is.null(analysis$q_questioned_data) && !is.null(analysis$q_model_data) && !is.null(analysis$q_model)){
    shinyjs::enable("q_analyze_questioned_docs")
  } else {
    shinyjs::disable("q_analyze_questioned_docs")
  }
})

# UPLOAD: templates ----
observeEvent(input$q_load_templates, {
  file <- input$q_load_templates
  analysis$q_template_file <- file$datapath
  analysis$q_templates <- readRDS(analysis$q_template_file)
})

# UPLOAD: model data ----
observeEvent(input$q_load_model_data, {
  file <- input$q_load_model_data
  analysis$q_model_data_file <- file$datapath
  analysis$q_model_data <- readRDS(analysis$q_model_data_file)
})

# UPLOAD: model ----
observeEvent(input$q_load_model, {
  file <- input$q_load_model
  analysis$q_model_file <- file$datapath
  analysis$q_model <- readRDS(analysis$q_model_file)
})

# UPLOAD: questioned data ----
observeEvent(input$q_load_questioned_data, {
  file <- input$q_load_questioned_data
  analysis$q_questioned_data_file <- file$datapath
  analysis$q_questioned_data <- readRDS(analysis$q_questioned_data_file)
})

# UPDATE: template directories, template_num, template_names ----
observe({
  analysis$q_main_dir <- input$q_main_dir
  
  # template directorys
  analysis$q_template_images_dir <- file.path(analysis$q_main_dir, "data", "template_images")
  analysis$q_template_graphs_dir <- file.path(analysis$q_main_dir, "data", "template_graphs")
  
  # template number selections
  if (!is.null(analysis$q_templates)){
    updateSelectInput(session, "q_loaded_template_num", choices = 1:length(analysis$q_templates))
    updateSelectInput(session, "q_created_template_num", choices = 1:length(analysis$q_templates))
  } else {
    updateSelectInput(session, "q_loaded_template_num", choices = c(NA))
    updateSelectInput(session, "q_created_template_num", choices = c(NA))
  }
  
  # names of loaded templates
  if (!is.null(analysis$q_templates)){
    template_names <- c()
    for (i in 1:length(analysis$q_templates)){
      temp <- paste0("seed", input$q_starting_seed + i - 1, "_run", i)
      template_names <- c(template_names, temp)
    }
    analysis$q_template_names <- template_names
  } else {
    analysis$q_template_names <- NULL
  }
})

# UPDATE: selected template ----
observeEvent(input$q_loaded_template_num, {
  analysis$q_template_current <- input$q_loaded_template_num
  # update choice in create templates panel
  updateSelectInput(session, "q_created_template_num", choices = 1:length(analysis$q_templates), selected = input$q_loaded_template_num)
})
observeEvent(input$q_created_template_num, {
  analysis$q_template_current <- input$q_created_template_num
  # update choice in load templates panel
  updateSelectInput(session, "q_loaded_template_num", choices = 1:length(analysis$q_templates), selected = input$q_created_template_num)
})

# UPDATE: model images and graphs directories, model variables for trace plot ----
observe({
  # model
  analysis$q_model_images_dir <- file.path(analysis$q_main_dir, "data", "model_images")
  analysis$q_model_graphs_dir <- file.path(analysis$q_main_dir, "data", "model_graphs")
  
  # trace plot variables
  if (!is.null(analysis$q_model)){
    updateSelectInput(session, "q_trace_variable", choices = names(as.data.frame(analysis$q_model[[1]])))
  } else {
    updateSelectInput(session, "q_trace_variable", choices = c(NA))
  }
})

# UPDATE: questioned images and graphs directories ----
observe({
  analysis$q_questioned_images_dir <- file.path(analysis$q_main_dir, "data", "questioned_images")
  analysis$q_questioned_graphs_dir <- file.path(analysis$q_main_dir, "data", "questioned_graphs")
})

# BUTTON: use default template ----
observeEvent(input$q_default_templates, {
  # use example template that comes with handwriter
  analysis$q_templates <- example_cluster_template
  
  # set template number
  analysis$q_template_current <- 1
})

# BUTTON: make templates ----
observeEvent(input$q_make_templates, {
  # process images if they haven't already been processed
  analysis$q_questioned_proc_list <- process_batch_dir(input_dir = analysis$q_template_images_dir,
                                                     output_dir = file.path(analysis$q_main_dir, "data", "template_graphs"),
                                                     transform_output = 'document')
  # update list of graphs
  analysis$q_questioned_graphs_docnames <- list.files(analysis$q_questioned_graphs_dir)
  
  # make templates
  analysis$q_templates <- make_clustering_templates(template_dir = analysis$q_main_dir,
                                                  writer_indices = c(2,5),
                                                  max_edges = 30,
                                                  starting_seed = input$q_starting_seed,
                                                  K = input$q_K,
                                                  num_runs = input$q_num_runs,
                                                  num_cores = 1,
                                                  num_dist_cores = input$q_num_cores,
                                                  num_path_cuts = 8,
                                                  max_iters = input$q_max_iters,
                                                  gamma = 3,
                                                  num_graphs = input$q_num_graphs)
})

# BUTTON: get model data ----
observeEvent(input$q_get_model_clusters, {
  # process images if they haven't already been processed
  analysis$q_model_proc_list <- process_batch_dir(input_dir = analysis$q_model_images_dir,
                                                     output_dir = analysis$q_model_graphs_dir,
                                                     transform_output = 'document')
  
  # get cluster assignments using current template
  analysis$q_model_clusters <- get_clusterassignment(clustertemplate = analysis$q_templates[[as.integer(analysis$q_template_current)]],
                                                     input_dir = analysis$q_model_graphs_dir)
  
  # format model data
  analysis$q_model_data <- format_model_data(model_proc_list=analysis$q_model_clusters, 
                                             writer_indices=c(2,5), 
                                             doc_indices=c(7,18), 
                                             a=2, b=0.25, c=2, d=2, e=0.5)
})

# BUTTON: save model data ----
#Download
output$q_save_model_clusters <- downloadHandler(
  filename = function(){
    paste0("model_clusters_", Sys.Date(), ".rds")
  },
  content = function(file) {
    message(paste0("Writing file: ", "model_clusters_", Sys.Date(), ".rds"))
    download = isolate(analysis$q_model_data)
    saveRDS(download, file = file)
  }
)

# BUTTON: fit model ----
observeEvent(input$q_fit_model, {
  analysis$q_model <- fit_model(model_data = analysis$q_model_data,
                                num_iters = input$q_num_mcmc_iters,
                                num_chains = input$q_num_chains)
})

# BUTTON: save model ----
#Download
output$q_save_model <- downloadHandler(
  filename = function(){
    paste0("model_", Sys.Date(), ".rds")
  },
  content = function(file) {
    message(paste0("Writing file: ", "model_", Sys.Date(), ".rds"))
    download = isolate(analysis$q_model)
    saveRDS(download, file = file)
  }
)

# BUTTON: get questioned data ----
observeEvent(input$q_get_questioned_data, {
  # process images if they haven't already been processed
  analysis$q_questioned_proc_list <- process_batch_dir(input_dir = analysis$q_questioned_images_dir,
                                                       output_dir = analysis$q_questioned_graphs_dir,
                                                       transform_output = 'document')
  
  # get cluster assignments using current template
  analysis$q_questioned_clusters <- get_clusterassignment(clustertemplate = analysis$q_templates[[as.integer(analysis$q_template_current)]],
                                                          input_dir = analysis$q_questioned_graphs_dir)
  
  # format questioned data
  analysis$q_questioned_data <- format_questioned_data(formatted_model_data = analysis$q_model_data,
                                                       questioned_proc_list = analysis$q_questioned_clusters,
                                                       writer_indices=c(2,5), 
                                                       doc_indices=c(7,18))
})

# BUTTON: save questioned data ----
#Download
output$q_save_questioned_data <- downloadHandler(
  filename = function(){
    paste0("questioned_data_", Sys.Date(), ".rds")
  },
  content = function(file) {
    message(paste0("Writing file: ", "questioned_data_", Sys.Date(), ".rds"))
    download = isolate(analysis$q_questioned_data)
    saveRDS(download, file = file)
  }
)

# BUTTON: analyze questioned documents ----
observeEvent(input$q_analyze_questioned_docs, {
  analysis$q_analysis <- analyze_questioned_documents(model_data = analysis$q_model_data, 
                                                      model = analysis$q_model, 
                                                      questioned_data = analysis$q_questioned_data, 
                                                      num_cores = input$q_questioned_num_cores)
})

# RENDER: template images directory and file names (from directory not current template) ----
output$q_template_images_dir <- renderText({ analysis$q_template_images_dir })
output$q_template_images_docnames <- renderPrint({ list.files(analysis$q_template_images_dir) })

# RENDER: template documents (from current template not directory) ----
output$q_template_docnames <- renderPrint({ 
  if (!is.null(analysis$q_templates)){
    unique(analysis$q_templates[[as.integer(analysis$q_template_current)]]$docnames)
  }
})

# RENDER: template graphs directory and file names ----
output$q_template_graphs_dir <- renderPrint({ analysis$q_template_graphs_dir })
output$q_template_graphs_docnames <- renderPrint({ 
  if (is.null(analysis$q_template_graphs_docnames)){
    list.files(analysis$q_template_graphs_dir)
  } else {
    analysis$q_template_graphs_docnames
  }
})

# RENDER: templates names ----
output$q_template_names <- renderPrint({ 
  if (!is.null(analysis$q_template_names)){
    analysis$q_template_names
  }
})

# RENDER: selected template name ----
output$q_selected_template <- renderPrint({ 
  if (!is.null(analysis$q_template_names) && !is.null(analysis$q_template_current)){
    analysis$q_template_names[as.integer(analysis$q_template_current)] 
  }
})

# RENDER: plot template cluster fill counts ----
output$q_template_cluster_fill_counts <- renderPlot({
  if (!is.null(analysis$q_templates)){
    clusters <- data.frame(cluster = analysis$q_templates[[as.integer(analysis$q_template_current)]]$cluster)
    df <- clusters %>% 
      mutate(cluster = as.factor(cluster)) %>%
      group_by(cluster) %>%
      summarize(count = n())
    
    ggplot2::ggplot(df, aes(x=cluster, y=count)) +
      geom_bar(stat = "identity")
  }
})

# RENDER: plot clusters ----
output$q_plot_clusters <- renderImage({
  # A temp file to save the output.
  # This file will be removed later by renderImage
  outfile <- 
    image_read("images/template.png") %>%
    image_write(tempfile(fileext = '.png'), format = "png")
  
  # Return a list containing the filename
  list(src = outfile,
       contentType = 'image/png',
       width = 600,
       alt = "This is alternate text")
}, deleteFile = TRUE)

# RENDER: model images file names ----
output$q_model_images_docnames <- renderPrint({ list.files(analysis$q_model_images_dir) })

# RENDER: model cluster fill counts table ----
output$q_cluster_fill_counts <- renderDT({ analysis$q_model_data$cluster_fill_counts })

# RENDER: check model ----
output$q_is_mcmc <- renderPrint({ coda::is.mcmc.list(analysis$q_model) })

# RENDER: trace plot ----
output$q_trace_plot <- renderPlot({
  if (!is.null(analysis$q_model) && !is.na(input$q_trace_variable)){
    plot_trace(model=analysis$q_model, variable=input$q_trace_variable)
  }
})

# RENDER: questioned images file names ----
output$q_questioned_images_docnames <- renderPrint({ list.files(analysis$q_questioned_images_dir) })

# RENDER: model cluster fill counts table ----
output$q_questioned_cluster_fill_counts <- renderDT({ analysis$q_questioned_data$cluster_fill_counts })

# RENDER: posterior probabilities table ----
output$q_post_probs_table <- renderDT({ 
  if (!is.null(analysis$q_analysis)){
    # transpose data frame so questioned docs are rows instead of columns
    df <- t(analysis$q_analysis$posterior_probabilities)
    # grab known writers from the first row (caused by transposing matrix)
    known_writer_colnames <- df[1,]
    # drop known writers rows
    df <- df[2:nrow(df), ]
    # change questioned docs from row names to column
    df <- cbind(rownames(df), data.frame(df, row.names=NULL))
    # rename columns
    colnames(df) <- c("questioned_doc", known_writer_colnames)
    df
  }
})

output$q_post_probs_plot <- renderPlot({
  if (!is.null(analysis$q_analysis)){ plot_posterior_probabilities(analysis$q_analysis) }
})

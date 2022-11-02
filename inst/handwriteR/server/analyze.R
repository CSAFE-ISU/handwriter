analysis <- reactiveValues(q_main_datapath = getwd())

#======================= MAIN DIRECTORY =============================
# UPDATE: main directory ----
shinyDirChoose(
  input,
  'q_main_dir',
  roots = c(home = '~'),
  filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
)
q_main_dir <- reactive(input$q_main_dir)
observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$q_main_dir
             },
             handlerExpr = {
               if (!"path" %in% names(q_main_dir())) return()
               home <- normalizePath("~")
               analysis$q_main_datapath <-
                 file.path(home, paste(unlist(q_main_dir()$path[-1]), collapse = .Platform$file.sep))
             })

# RENDER: main directory ----
output$dir <- renderText({
  analysis$q_main_datapath
})

#======================= TEMPLATES =============================

# UPDATE: template directories ----
observe({
  # template directories
  analysis$q_template_images_dir <- file.path(analysis$q_main_datapath, "data", "template_images")
})

# UPDATE: Load template in main dir ----
observeEvent(input$q_main_dir, {
  if ( file.exists(file.path(analysis$q_main_datapath, "data", "template.rds")) && (input$q_use_template == "main") ) {
    analysis$q_template <- readRDS(file.path(analysis$q_main_datapath, "data", "template.rds"))
  }
})

# UPDATE: Choose template ----
observeEvent(input$q_use_template, {
  if ( input$q_use_template == "default" ){
    analysis$q_template <- example_cluster_template
  } else if ( input$q_use_template == "main" && file.exists(file.path(analysis$q_main_datapath, "data", "template.rds")) ) {
    analysis$q_template <- readRDS(file.path(analysis$q_main_datapath, "data", "template.rds"))
  } else {
    analysis$q_template <- NULL
  }
})

# BUTTON: make templates ----
observeEvent(input$q_make_templates, {
  # make templates
  analysis$q_template <- make_clustering_templates(template_dir = analysis$q_main_datapath,
                                                   template_images_dir = analysis$q_template_images_dir,
                                                   writer_indices = c(2,5),
                                                   max_edges = 30,
                                                   seed = input$q_seed,
                                                   K = input$q_K,
                                                   num_dist_cores = input$q_num_cores,
                                                   num_path_cuts = 8,
                                                   max_iters = input$q_max_iters,
                                                   gamma = 3,
                                                   num_graphs = input$q_num_graphs)
})

# RENDER: current template
output$q_use_template <- renderPrint({
  if (input$q_use_template == "default"){
    "Default template"
  } else if ( input$q_use_template == "main" && file.exists(file.path(analysis$q_main_datapath, "data", "template.rds")) ) {
    "Template in main directory"
  } else {
    "Either select the default template or create a new template."
  }
})

# RENDER: template images directory and file names (from directory not current template) ----
output$q_template_images_docnames <- renderPrint({ 
  # list template images in template folder if template is null
  if (is.null(analysis$q_template)){
    list.files(analysis$q_template_images_dir)
  } else {
    unique(analysis$q_template$docnames)
  } 
})

# RENDER: plot template cluster fill counts ----
output$q_template_cluster_fill_counts <- renderPlot({
  if (!is.null(analysis$q_template)){
    template_data <- format_template_data(analysis$q_template)
    plot_cluster_fill_counts(template_data, facet = TRUE)
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


#======================= MODEL =============================

# ENABLE/DISABLE: fit model ----
observe({
  if (!is.null(analysis$q_template) && 
      !is.null(analysis$q_model_images_dir) &&
      dir.exists(analysis$q_model_images_dir) && 
      (length(list.files(analysis$q_model_images_dir)) > 0)) {
    shinyjs::enable("q_fit_model")
  } else {
    shinyjs::disable("q_fit_model")
  }
})

# UPDATE: model directories ----
observe({
  # model images directory
  analysis$q_model_images_dir <- file.path(analysis$q_main_datapath, "data", "model_images")
})

# UPDATE: model ----
observeEvent(input$q_main_dir, {
  if ( file.exists(file.path(analysis$q_main_datapath, "data", "model.rds")) ) {
    analysis$q_model <- readRDS(file.path(analysis$q_main_datapath, "data", "model.rds"))
  } 
})

# UPDATE: model variables for trace plot ----
observe({
  # trace plot variables
  if (!is.null(analysis$q_model)){
    updateSelectInput(session, "q_trace_variable", choices = names(as.data.frame(coda::as.mcmc(analysis$q_model$fitted_model[[1]]))))
  } else {
    updateSelectInput(session, "q_trace_variable", choices = c(NA))
  }
})

# BUTTON: fit model ----
observeEvent(input$q_fit_model, {
  analysis$q_model <- fit_model(template_dir = analysis$q_main_datapath,
                                model_images_dir = analysis$q_model_images_dir,
                                num_iters = input$q_num_mcmc_iters,
                                num_chains = input$q_num_chains,
                                writer_indices = c(2,5),
                                doc_indices = c(7,18))
})


# RENDER: model images file names ----
output$q_model_images_docnames <- renderPrint({ list.files(analysis$q_model_images_dir) })

# RENDER: model cluster fill counts plot ----
output$q_model_cluster_counts_plot <- renderPlot({
  if ( !is.null(analysis$q_model) ) {
    plot_cluster_fill_counts(analysis$q_model, facet = TRUE)
  }
})

# RENDER: check model ----
output$q_is_mcmc <- renderPrint({ coda::is.mcmc.list(analysis$q_model$fitted_model) })

# RENDER: trace plot ----
output$q_trace_plot <- renderPlot({
  if (!is.null(analysis$q_model) && !is.na(input$q_trace_variable)){
    plot_trace(model=analysis$q_model, variable=input$q_trace_variable)
  }
})


#======================= QUESTIONED DOCS ========================

# ENABLE/DISABLE: analyze questioned docs ----
observe({
  if ( !is.null(analysis$q_template) && !is.null(analysis$q_model) ){
    shinyjs::enable("q_analyze_questioned_docs")
  } else {
    shinyjs::disable("q_analyze_questioned_docs")
  }
})

# UPDATE: questioned images and graphs directories ----
observe({
  analysis$q_questioned_images_dir <- file.path(analysis$q_main_datapath, "data", "questioned_images")
})

# UPDATE: analysis ----
observeEvent(input$q_main_dir, {
  if ( file.exists(file.path(analysis$q_main_datapath, "data", "analysis.rds")) ) {
    analysis$q_analysis <- readRDS(file.path(analysis$q_main_datapath, "data", "analysis.rds"))
  } 
})

# BUTTON: analyze questioned documents ----
observeEvent(input$q_analyze_questioned_docs, {
  analysis$q_analysis <- analyze_questioned_documents(template_dir = analysis$q_main_datapath,
                                                      questioned_images_dir = analysis$q_questioned_images_dir,
                                                      model = analysis$q_model,
                                                      num_cores = input$q_questioned_num_cores,
                                                      writer_indices = c(2,5),
                                                      doc_indices = c(7,17))
})

# RENDER: questioned images file names ----
output$q_questioned_images_docnames <- renderPrint({ list.files(analysis$q_questioned_images_dir) })

# RENDER: questioned cluster fill counts plot ----
output$q_questioned_cluster_counts_plot <- renderPlot({
  if (!is.null(analysis$q_analysis)){
    plot_cluster_fill_counts(analysis$q_analysis, facet = TRUE)
  }
})

# RENDER: posterior probabilities table ----
output$q_post_probs_table <- renderTable({ 
  if (!is.null(analysis$q_analysis)){
    # transpose data frame so questioned docs are rows instead of columns
    df <- t(analysis$q_analysis$posterior_probabilities)
    # grab known writers from the first row (caused by transposing matrix)
    known_writer_colnames <- df[1,]
    # change known_writer_# to writer_#
    known_writer_colnames <- stringr::str_replace(known_writer_colnames, "known_", "")
    # drop known writers rows
    df <- df[2:nrow(df), ]
    # change questioned docs from row names to column
    df <- cbind(rownames(df), data.frame(df, row.names=NULL))
    # rename columns
    colnames(df) <- c("questioned_doc", known_writer_colnames)
    df
  }
})

# RENDER: posterior probabilities plot ----
output$q_post_probs_plot <- renderPlot({
  if (!is.null(analysis$q_analysis)){ 
    plot_posterior_probabilities(analysis$q_analysis) 
  }
})

# Main directory ----------------------------------------------------------
# UPDATE: main directory on create model page ----
shinyDirChoose(
  input,
  'q_main_dir_model',
  roots = c(home = '~'),
  filetypes = c('', 'png', 'txt', 'bigWig', "tsv", "csv", "bw")
)
q_main_dir_model <- reactive(input$q_main_dir_model)
observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$q_main_dir_model
             },
             handlerExpr = {
               if (!"path" %in% names(q_main_dir_model())) return()
               home <- normalizePath("~")
               analysis$q_main_datapath <-
                 file.path(home, paste(unlist(q_main_dir_model()$path[-1]), collapse = .Platform$file.sep))
             })

# RENDER: main directory on create model page ----
output$dir_model <- renderText({
  analysis$q_main_datapath
})


# Model -------------------------------------------------------------------
# ENABLE/DISABLE: fit model ----
observe({
  if (analysis$q_main_datapath != "" &&
      !is.null(analysis$q_template) && 
      !is.null(analysis$q_model_docs) &&
      dir.exists(analysis$q_model_docs) && 
      (length(list.files(analysis$q_model_docs)) > 0)) {
    shinyjs::enable("q_fit_model")
  } else {
    shinyjs::disable("q_fit_model")
  }
})

# UPDATE: model training documents directory ----
shinyDirChoose(
  input,
  'q_model_docs',
  roots = c(home = '~'),
  filetypes = c('png')
)
q_model_docs <- reactive(input$q_model_docs)
observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$q_model_docs
             },
             handlerExpr = {
               if (!"path" %in% names(q_model_docs())) return()
               home <- normalizePath("~")
               analysis$q_model_docs <-
                 file.path(home, paste(unlist(q_model_docs()$path[-1]), collapse = .Platform$file.sep))
             })

# BUTTON: load template ----
observeEvent(input$q_load_template, {
  if ( input$q_select_template == "default"){
    analysis$q_template = handwriter::example_cluster_template
    showNotification("Default template loaded.", duration = analysis$msg_duration)
  } else if ( file.exists(file.path(analysis$q_main_datapath, "data", "template.rds")) ){
    analysis$q_template = readRDS(file.path(analysis$q_main_datapath, "data", "template.rds"))
    showNotification("Template loaded.", duration = analysis$msg_duration)
  } else {
    showNotification("The main directory does not contain a template. Create a new template on the Cluster Template tab.",
                     duration = analysis$msg_duration)
  }
})

# BUTTON: fit model ----
observeEvent(input$q_fit_model, {
  analysis$q_model <- fit_model(template_dir = analysis$q_main_datapath,
                                model_images_dir = analysis$q_model_docs,
                                num_iters = input$q_num_mcmc_iters,
                                num_chains = input$q_num_chains,
                                num_cores = input$q_num_model_cores,
                                writer_indices = c(2,5),
                                doc_indices = c(7,18))
  showNotification("Model created and saved as model.rds in directory > data.", 
                   duration = analysis$msg_duration)
})

# RENDER: model training documents directory ----
output$q_model_docs <- renderText({
  if ( !is.null(analysis$q_model_docs) ) {
    analysis$q_model_docs
  }
})

# RENDER: model documents file names ----
output$q_model_docs_list <- renderTable({ 
  if ( !is.null(analysis$q_model_docs)) {
    data.frame("filenames" = list.files(analysis$q_model_docs))
  }
})

# RENDER: model cluster fill counts plot ----
output$q_model_cluster_fill_counts <- renderPlot({
  if ( !is.null(analysis$q_model) ) {
    plot_cluster_fill_counts(analysis$q_model, facet = TRUE)
  }
})

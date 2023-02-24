# UPDATE: main directory on create template page ----
shinyDirChoose(
  input,
  'q_main_dir',
  roots = c(home = '~'),
  filetypes = c('', 'png', 'txt', 'bigWig', "tsv", "csv", "bw")
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

# RENDER: main directory on create template page ----
output$dir <- renderText({
  analysis$q_main_datapath
})

#======================= TEMPLATE =============================
# UPDATE: template training documents directory ----
shinyDirChoose(
  input,
  'q_template_docs',
  roots = c(home = '~'),
  filetypes = c('png')
)
q_template_docs <- reactive(input$q_template_docs)
observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$q_template_docs
             },
             handlerExpr = {
               if (!"path" %in% names(q_template_docs())) return()
               home <- normalizePath("~")
               analysis$q_template_docs <-
                 file.path(home, paste(unlist(q_template_docs()$path[-1]), collapse = .Platform$file.sep))
             })

# RENDER: template docs directory ----
output$q_template_docs <- renderText({
  analysis$q_template_docs
})

observeEvent(input$q_make_templates, {
  # make templates
  analysis$q_template <- make_clustering_templates(template_dir = analysis$q_main_datapath,
                                                   template_images_dir = analysis$q_template_docs,
                                                   writer_indices = c(2,5),
                                                   max_edges = 30,
                                                   centers_seed = input$q_centers_seed,
                                                   graphs_seed = input$q_graphs_seed,
                                                   K = input$q_K,
                                                   num_dist_cores = input$q_num_cores,
                                                   num_path_cuts = 8,
                                                   max_iters = input$q_max_iters,
                                                   gamma = 3,
                                                   num_graphs = input$q_num_graphs)
})

# RENDER: template images directory and file names ----
output$q_template_docs_list <- renderPrint({ 
  # list template images in template folder if template is null
  if ( !is.null(analysis$q_template_docs) ){
    list.files(analysis$q_template_docs)
  }
})

# RENDER: plot template cluster fill counts ----
output$q_template_cluster_fill_counts <- renderPlot({
  if (!is.null(analysis$q_template)){
    template_data <- format_template_data(analysis$q_template)
    plot_cluster_fill_counts(template_data, facet = TRUE)
  }
})

#==================================================================
#======================= QUESTIONED DOC ANALYSIS ==================
#==================================================================

analysis <- reactiveValues(q_main_dir = "/Users/stephanie/Documents/shiny_example",
                           q_template_images_dir = "/Users/stephanie/Documents/shiny_example/data/template_images",
                           q_template_graphs_dir = "/Users/stephanie/Documents/shiny_example/data/template_graphs",
                           q_file = NULL,
                           q_templates = NULL)

# UPLOAD: templates
observeEvent(input$q_load_templates, {
  file <- input$q_load_templates
  analysis$q_file <- file$datapath
  analysis$q_templates <- readRDS(analysis$q_file)
})

# UPDATE:
observe({
  analysis$q_main_dir <- input$q_main_dir
  analysis$q_template_images_dir <- file.path(analysis$q_main_dir, "data", "template_images")
  analysis$q_template_graphs_dir <- file.path(analysis$q_main_dir, "data", "template_graphs")
})

# BUTTON:
observeEvent(input$q_make_templates, {
  # process images if they haven't already been processed
  analysis$q_template_proc_list <- process_batch_dir(input_dir = analysis$q_template_images_dir,
                                                     output_dir = file.path(analysis$q_main_dir, "data", "template_graphs"),
                                                     transform_output = 'document')
  # update list of graphs
  analysis$q_template_graphs_docnames <- list.files(analysis$q_template_graphs_dir)
  
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


# RENDER:
output$q_main_dir <- renderText({ analysis$q_main_dir })

# template images
output$q_template_images_dir <- renderText({ analysis$q_template_images_dir })
output$q_template_images_docnames <- renderPrint({ list.files(analysis$q_template_images_dir) })

# template graphs
output$q_template_graphs_dir <- renderPrint({ analysis$q_template_graphs_dir })
output$q_template_graphs_docnames <- renderPrint({ 
  if (is.null(analysis$q_template_graphs_docnames)){
    list.files(analysis$q_template_graphs_dir)
  } else {
    analysis$q_template_graphs_docnames
  }
})

# templates
output$q_templates <- renderPrint({ length(analysis$q_templates) })




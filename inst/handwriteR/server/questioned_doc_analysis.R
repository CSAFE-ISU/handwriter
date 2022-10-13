#==================================================================
#======================= QUESTIONED DOC ANALYSIS ==================
#==================================================================

analysis <- reactiveValues(q_main_dir = "/Users/stephanie/Documents/shiny_example",
                           q_template_images_dir = "/Users/stephanie/Documents/shiny_example/data/template_images")

# UPDATE:
observe({
  analysis$q_main_dir <- input$q_main_dir
  analysis$q_template_images_dir <- input$q_template_images_dir
})

# BUTTON: process template images
observeEvent(input$q_process_template_images, {
  analysis$q_template_proc_list <- process_batch_dir(input_dir = analysis$q_template_images_dir,
                                                     output_dir = file.path(analysis$q_main_dir, "data", "template_graphs"),
                                                     transform_output = 'document')
  analysis$q_template_proc_list_docnames <- sapply(analysis$q_template_proc_list, function(x) x$docname)
})

# BUTTON: create template(s)
observeEvent(input$q_create_templates, {
  # convert number of graphs to integer if necessary
  if (input$q_num_graphs != "All"){ q_num_graphs <- as.integer(input$q_num_graphs) }
  
  analysis$q_templates <- make_clustering_templates(template_dir = analysis$q_main_dir,
                                                    writer_indices = c(2,5),
                                                    K = input$q_K,
                                                    num_dist_cores = input$q_num_cores,
                                                    max_iters = input$q_max_iters,
                                                    num_graphs = q_num_graphs,
                                                    starting_seed = input$q_starting_seed,
                                                    num_runs = input$q_num_runs)
  analysis$q_num_templates <- length(analysis$q_templates)
})

# RENDER:
output$q_main_dir <- renderText({ analysis$q_main_dir })
output$q_main_dir_exists <- renderPrint({ dir.exists(analysis$q_main_dir) })

output$q_template_images_dir <- renderText({ analysis$q_template_images_dir })
output$q_template_images_dir_exists <- renderPrint({ dir.exists(analysis$q_template_images_dir) })
output$q_template_proc_list_docnames <- renderPrint({ analysis$q_template_proc_list_docnames })
output$q_num_templates <- renderPrint({ analysis$q_num_templates })
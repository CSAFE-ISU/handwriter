# template training docs
t_docs_path <- directoryServer("t_docs")
directoryContentsServer("t_docs_list", t_docs_path)

# output directory
t_main_path <- directoryServer("t_main_dir")

# template settings
t_writer_ind <- substringIndicesServer("t_writer_indices")
substringsServer("t_writers", t_docs_path, t_writer_ind, "writers")

# make template
observeEvent(input$t_create, {
  make_clustering_templates(
    template_dir = t_main_path(),
    template_images_dir = t_docs_path(),
    writer_indices = c(t_writer_ind$start(), t_writer_ind$stop()),
    centers_seed = input$t_centers_seed,
    graphs_seed = 100,
    K = input$t_K,
    num_dist_cores = input$t_cores,
    max_iters = input$t_max_iters,
    num_graphs = "All"
  )
  showNotification("Template saved as template.rds in Output Dir > data.")
})
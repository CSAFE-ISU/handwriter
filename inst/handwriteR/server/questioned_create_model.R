# model training docs
poi_docs_path <- directoryServer("poi_docs")
directoryContentsServer("poi_docs_list", poi_docs_path)

# output dir
poi_output_path <- directoryServer("poi_output")

# model settings
poi_writer_ind <- substringIndicesServer("poi_writer_indices")
substringsServer("poi_writers", poi_docs_path, poi_writer_ind, "writers")
poi_doccode_ind <- substringIndicesServer("poi_doccode_indices")
substringsServer("poi_doccodes", poi_docs_path, poi_doccode_ind, "doc code")

# fit model
observeEvent(input$poi_create, {
  fit_model(template_dir = poi_output_path(),
            model_images_dir = poi_docs_path(),
            num_iters = input$poi_num_iters,
            num_chains = input$poi_chains,
            num_cores = input$poi_cores,
            writer_indices = c(poi_writer_ind$start(), poi_writer_ind$stop()),
            doc_indices = c(poi_doccode_ind$start(), poi_doccode_ind$stop()))
})
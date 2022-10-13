#==================================================================
#======================= FEATURE EXTRACTION =======================
#==================================================================

output$features_image_name <- renderText({paste0("Name: ", values$image_name)})
output$features_dimensions <- renderText({paste0("Dimensions: ", values$dimensions)})

#UPLOAD
observeEvent(input$features_upload, {
  if (length(input$features_upload$datapath)){
    values$upload_path <- input$features_upload$datapath
    values$current_path <- values$upload_path
  }
  
  values$plot_type <- ''
  
  values$uploaded_image <- NULL
  processHandwriting_data <- reactiveValues()
  
  #Split depending on if png or RData is uploaded
  if(endsWith(input$features_upload$datapath, "png")){
    values$uploaded_image <- image_read(input$features_upload$datapath)
    
    values$image <- values$uploaded_image
    info <- image_info(values$image)
  }else if(endsWith(input$features_upload$datapath, "RData") || endsWith(input$features_upload$datapath, "rda")){
    image_with_mask <- load(input$features_upload$datapath)
    values$uploaded_image <- image_read(magick_image)
    
    values$image <- values$uploaded_image
    info <- image_info(values$image)
  }
  
  #Clean up
  values$crop_list <- list(values$image)
  values$mask_list_df <- values$mask_list_df[0,]
  
  shinyjs::disable('plotbinarized'); shinyjs::disable('plotthinned');
  shinyjs::disable("plotnodes"); shinyjs::disable("plotbreaks"); shinyjs::disable("plotline"); shinyjs::disable("linenum"); 
  shinyjs::disable("plotword"); shinyjs::disable("wordnum"); shinyjs::disable("plotgraph"); shinyjs::disable("graphnum");
  
  values$plot_type <- 'nothing. Process your document to be able to plot lines, words, and graphs'
})

#PROCESS HANDWRITING BUTTON
observeEvent(input$features_processhandwriting, {
  
  path <- values$current_path
  df = list()
  df$image = readPNGBinary(path)
  df$thin = thinImage(df$image)
  
  df_processList = processHandwriting(df$thin, dim(df$image))
  
  df$words = create_words(df_processList)
  df$words_after_processing = process_words(df$words, dim(df$image), TRUE)
  df$dims = dim(df$image)
  
  df$letterList = df_processList$letterList
  df$nodes = df_processList$nodes
  df$connectingNodes = df_processList$connectingNodes
  df$terminalNodes = df_processList$terminalNodes
  df$breaks = df_processList$breakPoints
  df$breakPoints = df_processList$breakPoints
  
  processHandwriting_data$df <- df
  
  shinyjs::enable('plotbinarized'); shinyjs::enable('plotthinned');
  shinyjs::enable("plotnodes"); shinyjs::enable("plotbreaks"); shinyjs::enable("plotline"); shinyjs::enable("linenum"); 
  shinyjs::enable("plotword"); shinyjs::enable("wordnum"); shinyjs::enable("plotgraph"); shinyjs::enable("graphnum"); 
})

#Download
output$save_document_extract <- downloadHandler(
  filename = function(){
    paste0(tools::file_path_sans_ext(values$image_name),'_proclist.rds')
  },
  content = function(file) {
    message(paste0("Writing file:", values$image_name))
    download = isolate(processHandwriting_data$df)
    saveRDS(download, file = file)
  }
)



#RENDER DOCUMENT DATA TABLE
output$document_dt = DT::renderDataTable({
  extensions="Responsive"
  req(processHandwriting_data$df)
  imgList = processHandwriting_data$df
  name <- c("image", "thin", "nodes", "connectingNodes", "terminalNodes", "breakPoints")
  type <- c(typeof(imgList$image), typeof(imgList$thin), typeof(imgList$nodes), typeof(imgList$connectingNodes), typeof(imgList$terminalNodes), typeof(imgList$breakPoints))
  size <- c(toString(dim(imgList$image)), toString(dim(imgList$thin)), toString(length(imgList$nodes)), toString(length(imgList$connectingNodes)), toString(length(imgList$terminalNodes)), toString(length(imgList$breakPoints)))
  value <- c(paste0(stringr::str_trunc(toString(imgList$image), 50)), 
             paste0(stringr::str_trunc(toString(imgList$thin), 50)),  
             paste0(stringr::str_trunc(toString(imgList$nodes), 50)), 
             paste0(stringr::str_trunc(toString(imgList$connectingNodes), 50)),  
             paste0(stringr::str_trunc(toString(imgList$terminalNodes), 50)), 
             paste0(stringr::str_trunc(toString(imgList$breakPoints), 50))) 
  
  document_table <- data.frame(name, type, size, value)
  document_table 
})


#FILL OUT THE DOCUMENT & GRAPH INFORMATION
check_for_processHandwriting_data <- reactive(processHandwriting_data$df)

observeEvent(check_for_processHandwriting_data(), {
  req(processHandwriting_data$df)
  imgList = processHandwriting_data$df
  
  #DOCUMENT LEVEL
  
  #image
  output$features_document_image_type <- renderText({paste(stringr::str_trunc(toString(typeof(imgList$image)), 50))})
  output$features_document_image_size <- renderText({paste(stringr::str_trunc(toString(dim(imgList$image)), 50))})
  output$features_document_image <- renderText({paste(stringr::str_trunc(toString(imgList$image), 100))})
  
  #thin
  output$features_document_thin_type <- renderText({paste(stringr::str_trunc(toString(typeof(imgList$thin)), 50))})
  output$features_document_thin_size <- renderText({paste(stringr::str_trunc(toString(dim(imgList$thin)), 50))})
  output$features_document_thin <- renderText({paste(stringr::str_trunc(toString(imgList$thin), 100))})
  
  #nodes
  output$features_document_nodes_type <- renderText({paste(stringr::str_trunc(toString(typeof(imgList$nodes)), 50))})
  output$features_document_nodes_size <- renderText({paste(stringr::str_trunc(toString(length(imgList$nodes)), 50))})
  output$features_document_nodes <- renderText({paste(stringr::str_trunc(toString(imgList$nodes), 100))})
  
  #connectingNodes
  output$features_document_connectingNodes_type <- renderText({paste(stringr::str_trunc(toString(typeof(imgList$connectingNodes)), 50))})
  output$features_document_connectingNodes_size <- renderText({paste(stringr::str_trunc(toString(length(imgList$connectingNodes)), 50))})
  output$features_document_connectingNodes <- renderText({paste(stringr::str_trunc(toString(imgList$connectingNodes), 100))})
  
  #terminalNodes
  output$features_document_terminalNodes_type <- renderText({paste(stringr::str_trunc(toString(typeof(imgList$terminalNodes)), 50))})
  output$features_document_terminalNodes_size <- renderText({paste(stringr::str_trunc(toString(length(imgList$terminalNodes)), 50))})
  output$features_document_terminalNodes <- renderText({paste(stringr::str_trunc(toString(imgList$terminalNodes), 100))})
  
  #breakPoints
  output$features_document_breakPoints_type <- renderText({paste(stringr::str_trunc(toString(typeof(imgList$breakPoints)), 50))})
  output$features_document_breakPoints_size <- renderText({paste(stringr::str_trunc(toString(length(imgList$breakPoints)), 50))})
  output$features_document_breakPoints <- renderText({paste(stringr::str_trunc(toString(imgList$breakPoints), 100))})
  
  #GRAPHS
  output$features_graph_aspect_ratio <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$aspect_ratio), 10))})
  output$features_graph_height <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$height), 10))})
  
  output$features_graph_width <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$width), 10))})
  output$features_graph_topmost_row <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$topmost_row), 10))})
  
  output$features_graph_bottom_row <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$bottom_row), 10))})
  output$features_graph_leftmost_col <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$leftmost_col), 10))})
  
  output$features_graph_rightmost_col <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$rightmost_col), 10))})
  output$features_graph_centroid_index <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$centroid_index), 10))})
  
  output$features_graph_centroid_y <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$centroid_y), 10))})
  output$features_graph_centroid_x <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$centroid_x), 10))})
  
  output$features_graph_centroid_horiz_location <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$centroid_horiz_location), 10))})
  output$features_graph_centroid_vert_location <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$centroid_vert_location), 10))})
  
  output$features_graph_lHalf <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$lHalf), 20))})
  output$features_graph_rHalf <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$rHalf), 20))})
  
  output$features_graph_disjoint_centroids_left <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$disjoint_centroids$left), 10))})
  output$features_graph_disjoint_centroids_right <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$disjoint_centroids$right), 10))})
  
  output$features_graph_slope <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$slope), 10))})
  output$features_graph_pixel_density <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$pixel_density), 10))})
  
  output$features_graph_box_density <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$box_density), 10))})
  output$features_graph_uniqueid <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$uniqueid), 10))})
  
  output$features_graph_down_dist <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$down_dist), 10))})
  output$features_graph_line_number <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$line_number), 10))})
  
  output$features_graph_order_within_line <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$order_within_line), 10))})
  output$features_graph_l_neighbor_dist <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$l_neighbor_dist), 10))})
  
  output$features_graph_r_neighbor_dist <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$r_neighbor_dist), 10))})
  output$features_graph_xvar <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$xvar), 10))})
  
  output$features_graph_yvar <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$yvar), 10))})
  output$features_graph_covar <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$covar), 10))})
  
  output$features_graph_wordIndex <- renderText({paste(stringr::str_trunc(toString(imgList$letterList[[input$features_graphnum]]$characterFeatures$wordIndex), 10))})
  
  #Jon - WORD INFO PASTING HERE
  
  #Plot Graph & Document (with nodes)
  #Jon - WORD PLOT HERE
  output$features_graph <- renderPlot({plotLetter(imgList$letterList, input$features_graphnum, imgList$dims )})
  output$features_document <- renderPlot({plotNodes(imgList$image, imgList$thin, imgList$nodes)})
})


#Jon - WORD DATA TABLE HERE
output$word_dt = DT::renderDataTable({
  
})

output$graph_dt = DT::renderDataTable({
  extensions="Responsive"
  req(processHandwriting_data$df)
  imgList = processHandwriting_data$df
  graph_table <- data.frame(matrix(ncol=8, nrow=0, dimnames=list(NULL, c("path", "nodes", "allPaths", "adjMatrix", "letterCode", "connectingNodes", "terminalNodes", "characterFeatures"))))
  
  for(i in 1:length(imgList$letterList)){
    graph = imgList$letterList[[i]]
    graph_table[i, ] <- c((stringr::str_trunc(toString(graph$path), 30)),
                          (stringr::str_trunc(toString(graph$nodes), 30)),
                          #(stringr::str_trunc(psate0('List of ', toString(length(graph$allPaths), 30)),
                          (stringr::str_trunc(paste0('List of ', (toString(length(graph$allPaths))), ' paths'), 30)),
                          (stringr::str_trunc(toString(graph$adjMatrix), 30)),
                          (stringr::str_trunc(toString(graph$letterCode), 30)),
                          (stringr::str_trunc(toString(graph$connectingNodes), 30)),
                          (stringr::str_trunc(toString(graph$terminalNodes), 30)),
                          (stringr::str_trunc(paste0('List of ', (toString(length(graph$characterFeatures))), ' features'), 30))
                          
    )
  }
  graph_table }, options = list(pageLength = 5))


# ==================================================================
# ======================== BATCH PROCESSING ========================
# ==================================================================

values$processed_docs = NULL

#UPLOAD
observeEvent(input$process_batch, {
  datapath_list <- list(input$batch_input_dir$datapath)
  name_list <- list(input$batch_input_dir$name)
  values$processed_docs = process_batch_list(datapath_list[[1]], name_list[[1]], input$batch_select)
})

#Download
output$save_batch <- downloadHandler(
  filename = function(){
    paste0("processed_batch-", Sys.Date(), ".rda")
  },
  content = function(file) {
    message(paste0("Writing file: ", "processed_batch-", Sys.Date(), ".rda"))
    download = isolate(values$processed_docs)
    save(download, file = file)
  }
)

output$batch_select_text <- renderText({
  if(input$batch_select == 'none'){
    paste0("Apply no change, direct output of processHandwriting will be returned")
  }
  else if(input$batch_select == 'document'){
    paste0("Change to document level hierarchy - used for clustering")
  }
})



server <- function(input, output, session) {
  
  #========================================================
  #============= ADDING TOOL TIPS TO UI ===================
  #========================================================
  
  #PRE PROCESSING 
  addTooltip(session, id = 'rotation', title = "Slide to rotate document. Use arrows to adjust by 1 degree", options = list(delay = list(show=500)))
  addTooltip(session, id = 'reset_crop', title = "Reset to originally uploaded image.", options = list(delay = list(show=500)))
  addTooltip(session, id = 'undo_crop', title = "Undo previous crop", options = list(delay = list(show=500)))
  addTooltip(session, id = 'crop', title = "Crop the highlighted area", options = list(delay = list(show=500)))
  addTooltip(session, id = 'mask', title = "Mask the highlighted area", options = list(delay = list(show=500)))
  addTooltip(session, id = 'undo_mask', title = "Undo previously applied mask", options = list(delay = list(show=500)))
  addTooltip(session, id = 'reset_mask', title = "Undo all mask areas", options = list(delay = list(show=500)))
  addTooltip(session, id = 'save_mask', title = "Save masks as an RData Object. The object will have the document and the mask, and must be saved with the .RData file type", options = list(delay = list(show=500)))

  #PLOTTING
  addTooltip(session, id = 'plot_processhandwriting', title = "Must be done before plotting. Can take up to a minute depending on the complexity of the document",
             placement = "bottom", trigger = "hover", options = list(delay = list(show=500)))
  addTooltip(session, id = 'plotbinarized', title = "Plot the binarized (black and white) document", options = list(delay = list(show=500)))
  addTooltip(session, id = 'plotthinned', title = "Plot the thinned version of the document", options = list(delay = list(show=500)))
  addTooltip(session, id = 'plotnodes', title = "Plot the important nodes of the document", options = list(delay = list(show=500)))
  addTooltip(session, id = 'plotbreaks', title = "Plot the break nodes of the document", options = list(delay = list(show=500)))
  addTooltip(session, id = 'plotline', title = "Plot a given line of the document", options = list(delay = list(show=500)))
  addTooltip(session, id = 'linenum', title = "Line number to plot", options = list(delay = list(show=500)))
  addTooltip(session, id = 'plotword', title = "Plot a given word of the document", options = list(delay = list(show=500)))
  addTooltip(session, id = 'wordnum', title = "Word number to plot", options = list(delay = list(show=500)))
  addTooltip(session, id = 'plotgraph', title = "Plot a given graph of a document. Graphs are often, but not always, letters", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graphnum', title = "Graph number to plot", options = list(delay = list(show=500)))
  
  #FEATURE EXPLORATION 
  addTooltip(session, id = 'document_image', title = "A matrix reprsenting the cropped image, where 1s are the 'whitespace' and 0s represent the writing", options = list(delay = list(show=500)))
  addTooltip(session, id = 'document_thin', title = "A list of the index values that are taken out during the thinning process", options = list(delay = list(show=500)))
  addTooltip(session, id = 'document_nodes', title = "All points of importance to breaking apart graphs", options = list(delay = list(show=500)))
  addTooltip(session, id = 'document_connectingNodes', title = "Nodes that are deemed to be connecting two graphs", options = list(delay = list(show=500)))
  addTooltip(session, id = 'document_terminalNodes', title = "Nodes that are terminating on graphs", options = list(delay = list(show=500)))
  addTooltip(session, id = 'document_breakPoints', title = "Based on nodes, points that are used to split apart two graphs", options = list(delay = list(show=500)))
  
  addTooltip(session, id = 'graph_aspect_ratio', title = "Height to width ratio", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_height', title = "Height of the graph, measured in pixels", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_width', title = "Width of the graph, measured in pixels", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_topmost_row', title = "The top-most row, as its y coordinate", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_bottom_row', title = "The bottom-most row, as its y coordinate", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_leftmost_col', title = "The left-most column, as its x coordinate", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_rightmost_col', title = "The left-most column, as its x coordinate", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_centroid_index', title = "The centroid of the graph, as its index", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_centroid_y', title = "The y coordinate of the centroid", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_centroid_x', title = "The x coordinate of the centroid", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_horiz_location', title = "x", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_vert_location', title = "x", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_lHalf', title = "List of all points on the left half of the graph", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_rHalf', title = "List of all points on the right half of the graph", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_disjoint_centroids_left', title = "The centroids of the left half, as its index", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_disjoint_centroids_right', title = "The centroids of the right half, as its index", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_slope', title = "The slope of the graph as it runs through the centroid", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_pixel_density', title = "x", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_box_density', title = "x", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_uniqueid', title = "A unique numerical identifier for the graph", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_down_dist', title = "Distance from the lowest point of a graph to the next graph, measured in pixels", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_line_number', title = "The position of the graph in the line", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_order_within_line', title = "The ordered within the line the graph falls in", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_l_neighbor_dist', title = "Distance from the left-most point in the graph to its left neighbor, measured in pixels", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_r_neighbor_dist', title = "Distance from the right-most point in the graph to its left neighbor, measured in pixels", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_xvar', title = "Variance of X, used to calculate the covariance in covar", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_yvar', title = "Variance of Y, used to calculate the covariance in covar", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_covar', title = "Covarience of the graph", options = list(delay = list(show=500)))
  addTooltip(session, id = 'graph_wordIndex', title = "Word number the graph belongs to", options = list(delay = list(show=500)))
  
  #========================================================
  #======================= SET UP =========================
  #========================================================
  
  #Disable buttons
  shinyjs::disable("reset_crop"); shinyjs::disable("undo_crop"); shinyjs::disable("save_mask"); shinyjs::disable("reset_mask"); shinyjs::disable("undo_mask")
  
  global <- reactiveValues(datapath = getwd())
  
  #Create Reactive Values
  values <- reactiveValues()
  
  #Read in sample image & Set up basic values
  image <- image_read("samplewriting.png")
  values$upload_path <- "samplewriting.png"
  values$current_path <- "samplewriting.png"
  values$image_name <- 'samplewriting.png'
  
  info <- image_info(image)
  mask_list_df <- data.frame(matrix(ncol=6, nrow=0))
  colnames(mask_list_df) <- c('xmin', 'xmax', 'ymin', 'ymax', 'xrange', 'yrange')
  
  values$image <- image
  values$uploaded_image <- image
  values$info <- info
  
  values$crop_list <- list(image)
  values$mask_list_df <- mask_list_df
  
  values$dimensions <- paste0(info$width, 'x', info$height)
  
  
  
  
  
  #=============================================================
  #======================= PREPROCESSING =======================
  #=============================================================
  
  #UPLOAD BOX
  observeEvent(input$upload, {
    if (length(input$upload$datapath)){
     values$upload_path <- input$upload$datapath
     #message(paste0('input$upload$datapath in preprocessing upload box: ', input$upload$datapath))
     #message(paste0('values$path in preprocessing upload box: ', values$path))
    }
    values$plot_type <- ''
    
    values$uploaded_image <- NULL
    
    if(endsWith(input$upload$datapath, "png")){
      values$uploaded_image <- image_read(input$upload$datapath)
      
      values$image <- values$uploaded_image
      info <- image_info(values$image)
    }else if(endsWith(input$plot_upload$datapath, "RData") || endsWith(input$plot_upload$datapath, "rda")){
      image_with_mask <- load(input$upload$datapath)
      values$uploaded_image <- image_read(magick_image)
      
      values$image <- values$uploaded_image
      info <- image_info(values$image)
    }
    
    values$dimensions <- paste0(info$width, 'x', info$height)
    values$image_name <- input$upload$name
    #message(paste0('values$image_name in preprocessing upload box:  ', values$image_name))
    
    #Clean up
    values$crop_list <- list(values$image)
    values$mask_list_df <- values$mask_list_df[0,]
  })
  
  #DOCUMENT NAME AND DIMS DISPLAYED
  output$image_name <- renderText({paste0("Name: ", values$image_name)})
  output$dimensions <- renderText({paste0("Dimensions: ", values$dimensions)})
  
  #ROTATE LEFT
  observeEvent(input$left, {
    output$error <- renderText({""})
    updateSliderInput(session, "rotation", value = input$rotation - 1)
  })
  
  #ROTATE RIGHT
  observeEvent(input$right, {
    output$error <- renderText({""})
    updateSliderInput(session, "rotation", value = input$rotation + 1)
  })
  
  #BUTTON: RESET CROP
  observeEvent(input$reset_crop, {
    output$error <- renderText({""})
    values$image <- values$uploaded_image
    values$crop_list <- list(values$image)
    
    #Reset dimensions
    info <- image_info(values$image)
    values$dimensions <- paste0(info$width, 'x', info$height)
    shinyjs::disable("reset_crop"); shinyjs::disable("undo_crop")
    values$current_path <- values$upload_path
  })
  
  #BUTTON: UNDO CROP
  observeEvent(input$undo_crop, {
    output$error <- renderText({""})
    values$image <- tail(values$crop_list, 2)[[1]]
    values$crop_list <- head(values$crop_list, -1)
    
    #message(paste0("crop list after undo crop:", values$crop_list))
    #Reset dimensions
    info <- image_info(values$image)
    values$dimensions <- paste0(info$width, 'x', info$height)
    if(length(values$crop_list) == 1){
      shinyjs::disable("reset_crop"); shinyjs::disable("undo_crop")
    }
    
    image_write(values$image, "tmp.png"); values$current_path <- "tmp.png"
    
  })
  
  #BUTTON: CROP
  observeEvent(input$crop, {
    
    if(is.null(input$preprocess_plot_brush)){
      output$error <- renderText({"Please select an area prior to cropping."})
    }else{ 
      output$error <- renderText({""})
      
      xmin = input$preprocess_plot_brush$xmin
      xmax = input$preprocess_plot_brush$xmax
      ymin = input$preprocess_plot_brush$ymin
      ymax = input$preprocess_plot_brush$ymax
      
      xrange = xmax - xmin
      yrange = ymax - ymin
      
      if(!is.null(xrange)){
        values$image = image_crop(values$image, paste(xrange,'x', yrange, '+', xmin, '+', ymin))
      }
      
      values$info <- image_info(values$image)
      info <- image_info(values$image)
      values$dimensions <- paste0(info$width, 'x', info$height)
      
      values$crop_list <- append(values$crop_list, values$image)
      message(paste0('crop_list:', values$crop_list, '\n'))
      
      shinyjs::enable("reset_crop"); shinyjs::enable("undo_crop")
      image_write(values$image, "tmp.png"); values$current_path <- "tmp.png"
    }})
  
  #BUTTON: RESET MASK
  observeEvent(input$reset_mask, {
    if(nrow(values$mask_list_df) == 0){
      output$error <- renderText({"No mask to remove"})
    }else{
      output$error <- renderText({""})
      values$mask_list_df <- values$mask_list_df[0,]
      shinyjs::disable("save_mask"); shinyjs::disable("undo_mask"); shinyjs::disable("reset_mask")
    }
  })
  
  #BUTTON: UNDO MASK
  observeEvent(input$undo_mask, {
    if(nrow(values$mask_list_df) == 0){
      output$error <- renderText({"No mask to undo"})
    }else{
      output$error <- renderText({""})
      values$mask_list_df <- head(values$mask_list_df,-1)
      
      #Disable Mask
      if(nrow(values$mask_list_df) == 0){
        shinyjs::disable("save_mask"); shinyjs::disable("undo_mask"); shinyjs::disable("reset_mask")
      }
    }})
  
  #BUTTON: MASK
  #Adds mask coordinates to mask_list
  observeEvent(input$mask, {
    if(is.null(input$preprocess_plot_brush)){
      output$error <- renderText({"Please select an area prior to masking."})
      
    }else{
      output$error <- renderText({""})
      
      xmin = input$preprocess_plot_brush$xmin
      xmax = input$preprocess_plot_brush$xmax
      ymin = input$preprocess_plot_brush$ymin
      ymax = input$preprocess_plot_brush$ymax
      
      xrange = xmax - xmin
      yrange = ymax - ymin
      
      values$mask_list_df[nrow(values$mask_list_df) + 1,] = c(xmin, xmax, ymin, ymax, xrange, yrange)
      shinyjs::enable("save_mask"); shinyjs::enable("undo_mask"); shinyjs::enable("reset_mask")
      
      message(values$mask_list_df)
    }})
  
  #RENDER PLOT WITH MASK
  output$preprocess_plot_masked <- renderImage({
    tmp = values$image
    if(nrow(values$mask_list_df) == 0){
      #output$error <- renderText({"No mask to plot"})
      
    }else{
      #do a loop through all the masks and add to image
      for (i in 1:nrow(values$mask_list_df)) {
        tmp = image_composite(
          tmp, 
          image_blank(values$mask_list_df[i, "xrange"], values$mask_list_df[i, "yrange"],  color="#ffffff80"), 
          operator = "atop", compose_args="70", 
          offset = paste0("+", values$mask_list_df[i,"xmin"], "+", values$mask_list_df[i, "ymin"])
        )
      }
    }
    
    tmp <- tmp %>%
      image_rotate(input$rotation) %>%
      image_resize(input$size) %>%
      image_write(tempfile(fileext='png'), format = 'png')
    
    # Return a list
    list(src = tmp, contentType = "image/png")
  }, deleteFile = FALSE)
  
  #RENDER PLOT
  output$preprocess_plot <- renderImage({
    output$error <- renderText({""})
    
    tmp <- values$image %>%
      image_rotate(input$rotation) %>%
      image_resize(input$size) %>%
      image_write(tempfile(fileext='png'), format = 'png')
    
    # Return a list
    list(src = tmp, contentType = "image/png")
  }, deleteFile = FALSE)
  
  
  #SAVE MASK 
  output$save_mask <- downloadHandler(
    filename <- function(){
      paste("image_masked.RData")
    },
    content = function(file) {
      if(nrow(values$mask_list_df) == 0){
        output$error <- renderText({"There is no mask to save"})
      }else{
        
        #Create the mask matrix: 0 = unmasked, 1 = masked
        info <- image_info(values$image)
        mask <- matrix(0, info$height, info$width)
        
        for (i in 1:nrow(values$mask_list_df)) {
          xmin = round(values$mask_list_df[i, "xmin"])
          ymin = round(values$mask_list_df[i, "ymin"])
          xmax = round(values$mask_list_df[i, "xmax"])
          ymax = round(values$mask_list_df[i, "ymax"])
          
          for (n in ymin:ymax){
            for(m in xmin:xmax){
              mask[n,m] = 1
            }
          }
        }
        
        img <- values$image %>% image_rotate(input$rotation)
        magick_image <- image_data(img, 'rgba')
        
        save(magick_image, mask, file = file)
      }
    }
  )
  
  
  #SAVE DOCUMENT
  output$save_document <- downloadHandler(
    filename = paste0("preprocessed_", values$image_name), #THIS DOES NOT USE AN UPDATED IMAGE_NAME VARIABLE. IDK WHY
    contentType = "image/png",
    content = function(file) {
      message(values$image_name)
      file.copy(tmpfile <- values$image %>% image_rotate(input$rotation) %>% image_write(tempfile(fileext='png'), format = 'png'), file)
    }
  )
  
  
  
  #=============================================================
  #======================= PLOTTING ============================
  #=============================================================
  processHandwriting_data <- reactiveValues()
  values$plot_type <- 'nothing. Process your document to be able to plot lines, words, and graphs'
  number = ''
  
  #Disable buttons until document is processed
  shinyjs::disable('plotbinarized'); shinyjs::disable('plotthinned');
  shinyjs::disable("plotnodes"); shinyjs::disable("plotbreaks"); shinyjs::disable("plotline"); shinyjs::disable("linenum"); 
  shinyjs::disable("plotword"); shinyjs::disable("wordnum"); shinyjs::disable("plotgraph"); shinyjs::disable("graphnum"); 
  
  #UPLOAD
  observeEvent(input$plot_upload, {
    if (length(input$plot_upload$datapath)){
      values$upload_path <- input$plot_upload$datapath
      values$current_path <- values$upload_path
    }
    
    values$plot_type <- ''
    
    values$uploaded_image <- NULL
    processHandwriting_data <- reactiveValues()
    
    #Split depending on if png or RData is uploaded
    if(endsWith(input$plot_upload$datapath, "png")){
      values$uploaded_image <- image_read(input$plot_upload$datapath)
      
      values$image <- values$uploaded_image
      info <- image_info(values$image)
    }else if(endsWith(input$plot_upload$datapath, "RData") || endsWith(input$plot_upload$datapath, "rda")){
      image_with_mask <- load(input$plot_upload$datapath)
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
  
  output$plot_image_name <- renderText({paste0("Name: ", values$image_name)})
  output$plot_dimensions <- renderText({paste0("Dimensions: ", values$dimensions)})
  
  #Plot original, un-thinned (bottom) image
  output$plot_image <- renderImage({
    #output$error <- renderText({""})
    
    tmp <- values$image %>%
      image_rotate(input$rotation) %>%
      image_resize(input$size) %>%
      image_write(tempfile(fileext='png'), format = 'png')
    
    # Return a list
    list(src = tmp, contentType = "image/png")
  }, deleteFile = FALSE)
  

  #PROCESS HANDWRITING BUTTON
  observeEvent(input$plot_processhandwriting, {
    
    path <- values$current_path
    df = list()
    message(paste0('path after plot_processhandwriting button:  ', path))
    message(paste0('values$path after plot_processhandwriting button:  ', values$path))
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
  
  #Set plot type based on button click
  observeEvent(input$plotbinarized, {values$plot_type = 'binarized image'})
  observeEvent(input$plotthinned, {values$plot_type = 'thinned image'})
  observeEvent(input$plotnodes, {values$plot_type = 'nodes'})
  observeEvent(input$plotbreaks, {values$plot_type = 'breaks'})
  observeEvent(input$plotline, {values$plot_type = 'line'})
  observeEvent(input$plotword, {values$plot_type = 'word'})
  observeEvent(input$plotgraph, {values$plot_type = 'graph'})
  
  #Plot specific plot based on button pressed
  output$plot_output <- renderPlot({
    
    req(processHandwriting_data$df)
    imgList = processHandwriting_data$df
    
    if(values$plot_type == 'line'){values$number = input$linenum}
    if(values$plot_type == 'word'){values$number = input$wordnum}
    if(values$plot_type == 'graph'){values$number = input$graphnum}
    
    if (values$plot_type == 'binarized image'){plotImage(imgList$image)}
    else if (values$plot_type == 'thinned image'){ plotImageThinned(imgList$image, imgList$thin)}
    else if (values$plot_type == 'nodes'){plotNodes(imgList$image, imgList$thin, imgList$nodes)}
    else if (values$plot_type == 'breaks'){plotNodes(imgList$image, imgList$thin, imgList$breaks)}
    else if (values$plot_type == 'line'){plotLine(imgList$letterList, input$linenum, imgList$dims)} 
    else if (values$plot_type == 'word'){plotWord(imgList$letterList, input$wordnum, imgList$dims)} 
    else if (values$plot_type == 'graph'){plotLetter(imgList$letterList, input$graphnum, imgList$dims)} 
    else {values$plot_type = 'thinned image'; plotImageThinned(imgList$image, imgList$thin)}
  })
  
    output$plot_output_title <- renderText({paste0("Showing ", values$plot_type, " ", values$number)})
  
  
  
  #==================================================================
  #======================= FEATURE EXPLORATION =======================
  #==================================================================
   
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
      message(paste0('path after plot_processhandwriting button:  ', path))
      message(paste0('values$path after plot_processhandwriting button:  ', values$path))
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
    
    output$features_image_name <- renderText({paste0("Name: ", values$image_name)})
    output$features_dimensions <- renderText({paste0("Dimensions: ", values$dimensions)})
    
    
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
      graph_table}, options = list(pageLength = 5))
    
    
    
    
    
  #==================================================================
  #======================== BATCH PROCESSING ========================
  #==================================================================
  
    shinyDirChoose(
      input,
      'batch_input_dir',
      roots = c(home = '.'),
      filetypes = c('')
    )
    
    batch_input_dir <- reactive(input$batch_input_dir)
    
    output$batch_input_dir <- renderText({
      global$datapath
    })
    
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$batch_input_dir
                 },
                 handlerExpr = {
                   if (!"path" %in% names(batch_input_dir())) return()
                   home <- normalizePath("~")
                   global$datapath <-
                     file.path(home, paste(unlist(batch_input_dir()$path[-1]), collapse = .Platform$file.sep))
                 })
    
    
    shinyDirChoose(
      input,
      'batch_output_dir',
      roots = c(home = '.'),
      filetypes = c('')
    )
    
    batch_output_dir <- reactive(input$batch_output_dir)
    
    output$batch_output_dir <- renderText({
      global$datapath
    })
    
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$batch_output_dir
                 },
                 handlerExpr = {
                   if (!"path" %in% names(batch_output_dir())) return()
                   home <- normalizePath("~")
                   global$datapath <-
                     file.path(home, paste(unlist(batch_output_dir()$path[-1]), collapse = .Platform$file.sep))
                 })
  
  #==================================================================
  #======================= K-MEANS CLUSTERING =======================
  #==================================================================
  
    shinyDirChoose(
      input,
      'cluster_template_input_dir',
      roots = c(home = '.'),
      filetypes = c('')
    )
    
    cluster_template_input_dir <- reactive(input$cluster_template_input_dir)
    
    output$cluster_template_input_dir <- renderText({
      global$datapath
    })
    
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$cluster_template_input_dir
                 },
                 handlerExpr = {
                   if (!"path" %in% names(cluster_template_input_dir())) return()
                   home <- normalizePath("~")
                   global$datapath <-
                     file.path(home, paste(unlist(cluster_template_input_dir()$path[-1]), collapse = .Platform$file.sep))
                 })
    
    
    
    
    shinyDirChoose(
      input,
      'cluster_closed_input_dir',
      roots = c(home = '.'),
      filetypes = c('')
    )
    
    cluster_closed_input_dir <- reactive(cluster_closed_input_dir)
    output$cluster_closed_input_dir <- renderText({global$datapath})
    
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$cluster_closed_input_dir
                 },
                 handlerExpr = {
                   if (!"path" %in% names(cluster_closed_input_dir())) return()
                   home <- normalizePath("~")
                   global$datapath <-
                     file.path(home, paste(unlist(cluster_closed_input_dir()$path[-1]), collapse = .Platform$file.sep))
                 })
    
    shinyDirChoose(
      input,
      'cluster_output_dir',
      roots = c(home = '.'),
      filetypes = c('')
    )
    
    cluster_output_dir <- reactive(input$cluster_output_dir)
    
    output$cluster_output_dir <- renderText({
      global$datapath
    })
    
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$cluster_output_dir
                 },
                 handlerExpr = {
                   if (!"path" %in% names(cluster_output_dir())) return()
                   home <- normalizePath("~")
                   global$datapath <-
                     file.path(home, paste(unlist(cluster_output_dir()$path[-1]), collapse = .Platform$file.sep))
                 })
    
  
  
  #======================================================================
  #======================= TRIANGLE DECOMPOSITION =======================
  #======================================================================
    shinyDirChoose(
      input,
      'triangle_input_dir',
      roots = c(home = '.'),
      filetypes = c('')
    )
    
    triangle_input_dir <- reactive(triangle_input_dir)
    output$triangle_input_dir <- renderText({global$datapath})
    
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$triangle_input_dir
                 },
                 handlerExpr = {
                   if (!"path" %in% names(triangle_input_dir())) return()
                   home <- normalizePath("~")
                   global$datapath <-
                     file.path(home, paste(unlist(triangle_input_dir()$path[-1]), collapse = .Platform$file.sep))
                 })
    
    output$cluster_graphs <- renderImage({list(src = file.path("graphs.png"), contentType = "image/png")}, deleteFile = FALSE)
    output$cluster_unknown <- renderImage({list(src = file.path("unknown_writer_cluster_counts.png"), width = 1200, height = 375, contentType = "image/png")}, deleteFile = FALSE)
    output$cluster_1 <- renderImage({list(src = file.path("writer1_cluster_counts.png"), width = 1200, height = 375, contentType = "image/png")}, deleteFile = FALSE)
    output$cluster_2 <- renderImage({list(src = file.path("writer2_cluster_counts.png"), width = 1200, height = 375, contentType = "image/png")}, deleteFile = FALSE)
    output$cluster_3 <- renderImage({list(src = file.path("writer3_cluster_counts.png"), width = 1200, height = 375, contentType = "image/png")}, deleteFile = FALSE)

}

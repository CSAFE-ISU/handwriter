server <- function(input, output, session) {
  
  #========================================================
  #============= ADDING TOOL TIPS TO UI ===================
  #========================================================
  addTooltip(session, id = 'processhandwriting', title = "Must be done before plotting. Can take up to a minute depending on the complexity of the document",
             placement = "bottom", trigger = "hover", options = list(delay = list(show=500)))
  addTooltip(session, id = 'rotation', title = "Slide to rotate document. Use arrows to adjust by 1 degree", options = list(delay = list(show=500)))
  addTooltip(session, id = 'reset_crop', title = "Reset to originally uploaded image.", options = list(delay = list(show=500)))
  addTooltip(session, id = 'undo_crop', title = "Undo previous crop", options = list(delay = list(show=500)))
  addTooltip(session, id = 'crop', title = "Crop the highlighted area", options = list(delay = list(show=500)))
  addTooltip(session, id = 'mask', title = "Mask the highlighted area", options = list(delay = list(show=500)))
  addTooltip(session, id = 'undo_mask', title = "Undo previously applied mask", options = list(delay = list(show=500)))
  addTooltip(session, id = 'reset_mask', title = "Undo all mask areas", options = list(delay = list(show=500)))
  addTooltip(session, id = 'save_mask', title = "Save masks as an RData Object. The object will have the document and the mask, and must be saved with the .RData file type", options = list(delay = list(show=500)))

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
  
  #========================================================
  #======================= SET UP =========================
  #========================================================
  
  #Disable buttons
  shinyjs::disable("reset_crop"); shinyjs::disable("undo_crop"); shinyjs::disable("save_mask"); shinyjs::disable("reset_mask"); shinyjs::disable("undo_mask")
  
  #Create Reactive Values
  values <- reactiveValues()
  
  #Read in sample image & Set up basic values
  image <- image_read("CSAFE_Sample.png")
  values$upload_path <- "CSAFE_Sample.png"
  values$current_path <- "CSAFE_Sample.png"
  values$image_name <- 'CSAFE_Sample.png'
  
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
  observeEvent(input$processhandwriting, {
    
    path <- values$current_path
    df = list()
    message(paste0('path after processhandwriting button:  ', path))
    message(paste0('values$path after processhandwriting button:  ', values$path))
    df$image = readPNGBinary(path)
    df$thin = thinImage(df$image)
    
    df_processList = processHandwriting(df$thin, dim(df$image))
    
    df$words = create_words(df_processList)
    df$words_after_processing = process_words(df$words, dim(df$image), TRUE)
    df$dims = dim(df$image)
    
    df$letterList = df_processList$letterList
    df$nodes = df_processList$nodes
    df$breaks = df_processList$breakPoints
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
    else { values$plot_type = 'thinned image'; plotImageThinned(imgList$image, imgList$thin) }
  })
  
    output$plot_output_title <- renderText({paste0("Showing ", values$plot_type, " ", values$number)})
  
  
  
  
  
  
  
  
  #==================================================================
  #======================= FEATURE EXTRACTION =======================
  #==================================================================
  
  
  
  
  #==================================================================
  #======================= K-MEANS CLUSTERING =======================
  #==================================================================
  
  
  
  
  #======================================================================
  #======================= TRIANGLE DECOMPOSITION =======================
  #======================================================================
  
}

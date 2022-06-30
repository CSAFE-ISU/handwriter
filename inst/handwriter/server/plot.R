#=============================================================
#======================= PLOTTING ============================
#=============================================================

processHandwriting_data <- reactiveValues()
output$plot_image_name <- renderText({paste0("Name: ", values$image_name)})
output$plot_dimensions <- renderText({paste0("Dimensions: ", values$dimensions)})
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
#=========================================================
#=================== PREPROCESSING =======================
#=========================================================

#UPLOAD BOX
observeEvent(input$upload, {
  if (length(input$upload$datapath)){
    values$upload_path <- input$upload$datapath
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
#Shiny HandwritR
# devtools::install_github("CSAFE-ISU/handwriter")
# Rcpp::sourceCpp(file = "~/src/ThinImageCpp.cpp")
#install.packages("shinybusy")
library(handwriter)
library(magick)
library(shiny)
library(shinyjs)
library(shinybusy)
print(paste0('working in: ', getwd()))


#UI
ui <- fluidPage(
  shinyjs::useShinyjs(), 
  add_busy_bar(color = "#0E86D4"),
  #span(textOutput("error"), style="color:red"),
  tags$head(tags$script(src = "message-handler.js"), 
            tags$style(HTML("input[type=\"number\"] {width: 80px;}")),
            tags$style(HTML("hr {border-top: 1px solid #000000;}")),
            tags$style(HTML('#save_document{background-color:#33ADFF} #save_document:hover{background-color:#3398FF} #save_document{color:white}')),
            tags$style(HTML('#save_mask{background-color:#33ADFF} #save_mask:hover{background-color:#3398FF} #save_mask{color:white}'))),
  
  navbarPage(
    title = "Shiny HandwritR",
    tabPanel("Intro",
            h1("Welcome to shiny handwriter!"),
            h3("This app has the following features:"),
            tags$div(tags$ul(
              tags$li("Preprocessing | Clean up your data including rotation, resizing, and cropping"),
              tags$li("Plotting | Plot your document as well as its lines, words, or graphs"),
              tags$li("Explore Features | Extract valuable features from your documents"),
              tags$li("k-means Clustering | Perform k-means clustering on a set of documents"),
              tags$li("Triangle Decomposition | Do Kniser Triangle Decomposition & compare on your documents"),
              ),  style = "font-size: 15px"),
      ),
    
    #PREPROCESS
    tabPanel("Pre-process", 
             sidebarLayout(
               sidebarPanel(width = 3,
                            h3("Pre-process"),
                            br(),
                            fileInput("upload", "Choose document to pre-process", accept = c('image/png')),
                            fluidRow(textOutput("image_name")),
                            fluidRow(textOutput("dimensions")),
                            hr(),
                            fluidRow(
                                  column(width = 1, br(), actionButton("left", label = icon("angle-double-left", "fa-2xs"))),
                                  column(width = 8, offset = 1, sliderInput("rotation", "Rotate:", min = -180, max = 180, value = 0)),
                                  column(width = 1, br(), actionButton("right", label = icon("angle-double-right", "fa-2xs"))),
                            ), 
                            hr(),
                            fluidRow(
                                  column(width = 4, actionButton("reset_crop", "Reset Crop")),
                                  column(width = 4, actionButton("undo_crop", "Undo Last Crop")),
                                  column(width = 4, actionButton("crop", "Crop Area")),
                            ),
                            hr(),
                            fluidRow(
                              column(width = 6, offset = 6, downloadButton("save_document", "Save Document"))
                            )),
               mainPanel(width = 9,
                         span(textOutput("error"), style="color:red"),
                         br(),
                         tabsetPanel(id = "plotset",
                                     tabPanel("Current Document",
                                              br(),
                                              imageOutput("preprocess_plot", brush = brushOpts(id = "preprocess_plot_brush", resetOnNew = TRUE))
                                     ),
                                     tabPanel("Apply Mask",
                                              br(),
                                              fluidRow(
                                                column(width = 2, actionButton("mask", "Mask Area")),
                                                column(width = 2, actionButton("undo_mask", "Undo Last Mask")),
                                                column(width = 2, actionButton("reset_mask", "Remove Mask")),
                                                column(width = 2, downloadButton("save_mask", "Save Mask")),

                                              ),
                                              hr(),
                                              imageOutput("preprocess_plot_masked", brush = brushOpts(id = "preprocess_plot_brush", resetOnNew = TRUE))
                                     )
                         ),

               )
             ),
    ),
    
    #PLOT
    tabPanel("Plot",
             sidebarLayout(
               sidebarPanel(width = 3,
                            h3("Plotting"),
                            br(),
                            fileInput("plot_upload", "Choose document to plot", accept = c('image/png')),
                            fluidRow(
                              column(6, offset = 2, actionButton('processhandwriting', "Process Handwriting"))
                            ), br(), br(),
                            fluidRow(
                              column(4, offset = 1, actionButton("plotnodes", "Plot Nodes")),
                              column(4, actionButton("plotbreaks", "Plot Breaks"))
                            ), br(),
                            fluidRow(
                              column(4, offset = 1, br(), actionButton("plotline", "Plot Line")),
                              column(4, numericInput("linenum", "Line Number", 1))
                            ), br(),
                            fluidRow(
                              column(4, offset = 1, br(), actionButton("plotword", "Plot Word")),
                              column(4, numericInput("wordnum", "Word Number", 1))
                            ), br(),
                            fluidRow(
                              column(4, offset = 1, br() , actionButton("plotgraph", "Plot Graph")),
                              column(4, numericInput("graphnum", "Graph Number", 1))
                            ),
                            
               ),
               mainPanel(width = 9, plotOutput("plot_output"))
              ),
      
               
             
    ),
    
    #FEATURE EXPLORATION
    tabPanel("Explore Features",
              sidebarLayout(
                sidebarPanel(width = 3,
                            h3("Explore Features"),
                            br(),
                            fileInput("upload", "Choose document to explore", accept = c('image/png')),
                            
               ),
               mainPanel(width = 9, plotOutput("features_output"))
             ),
            ),
    
    #K-MEANS CLUSTERING
    tabPanel("k-means Clustering", 
             sidebarLayout(
               sidebarPanel(width = 3,
                            h3("Explore Features"),
                            br(),
                            fileInput("upload", "Choose document to explore", accept = c('image/png')),
                            
               ),
               mainPanel(width = 9, plotOutput("kmeans_output"))
             ),
            ),
    
    #TRIANGLE DECOMPOSITION
    tabPanel("Triangle Decomposition", 
             sidebarLayout(
               sidebarPanel(width = 3,
                            h3("Explore Features"),
                            br(),
                            fileInput("upload", "Choose a document or directory to decompose", accept = c('image/png')),
                            
               ),
               mainPanel(width = 9, plotOutput("triangle_output"))),
    )),
)


#SERVER
server <- function(input, output, session) {
  #There is no way this is the right way to set this up
  image <- image_read("https://csafe-isu.github.io/handwriter/resources/images/Writing_csafe_single.png")
  info <- image_info(image)
  #mask <- matrix(0, info$height, info$width)
  mask_list_df <- data.frame(matrix(ncol=6, nrow=0))
  colnames(mask_list_df) <- c('xmin', 'xmax', 'ymin', 'ymax', 'xrange', 'yrange')
  
  shinyjs::disable("save_mask")
  
  #Create Reactive Values
  values <- reactiveValues()
  processHandwriting_data <- reactiveValues()
  
  values$image <- image
  values$uploaded_image <- image
  values$info <- info

  values$mask_list_df <- mask_list_df
  values$plot_type <- ''
  values$image_name <- 'CSAFE_Sample.png'
  values$dimensions <- paste0(info$width, 'x', info$height)

  
  #======================= PREPROCESSING =======================
  
  #UPLOAD BOX
  observeEvent(input$upload, {
    if (length(input$upload$datapath))
      
    values$uploaded_image <- NULL
    values$uploaded_image <- image_read(input$upload$datapath)
    
    values$image <- values$uploaded_image 
    
    info <- image_info(values$image)
    values$dimensions <- paste0(info$width, 'x', info$height)

    values$image_name <- input$upload$name
    print(input$upload$name)
    
    #Clean up masks
    values$mask_list_df <- values$mask_list_df[0,]
  })
  
  #DOCUMENT NAME AND DIMS DISPLAYED
  output$image_name <- renderText({paste0("Image Name: ", values$image_name)})
  output$dimensions <- renderText({paste0("Dimensions:", values$dimensions)})
  
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
    
    #Reset dimensions
    info <- image_info(values$image)
    values$dimensions <- paste0(info$width, 'x', info$height)
  })
  
  #BUTTON: UNDO CROP
  observeEvent(input$undo_crop, {
    output$error <- renderText({""})
    values$image <- values$previous_image
    
    #Reset dimensions
    info <- image_info(values$image)
    values$dimensions <- paste0(info$width, 'x', info$height)
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
        values$previous_image = values$image
        values$image = image_crop(values$image, paste(xrange,'x', yrange, '+', xmin, '+', ymin))
      }
      
      values$info <- image_info(values$image)
      
      info <- image_info(values$image)
      values$dimensions <- paste0(info$width, 'x', info$height)
    }})
  
  #BUTTON: RESET MASK
  observeEvent(input$reset_mask, {
    if(nrow(values$mask_list_df) == 0){
      output$error <- renderText({"No mask to remove"})
    }else{
      output$error <- renderText({""})
      values$mask_list_df <- values$mask_list_df[0,]
      shinyjs::disable("save_mask")
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
        shinyjs::disable("save_mask")
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
      shinyjs::enable("save_mask")
      
      print(values$mask_list_df)
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
  
  #output$x_crop_info <- renderText({paste('x-range: ', input$preprocess_plot_brush$xmin, ', ', input$preprocess_plot_brush$xmax)})
  #output$y_crop_info <- renderText({paste('y-range: ', input$preprocess_plot_brush$ymin, ', ', input$preprocess_plot_brush$ymax)})
  
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
      print(values$image_name)
      file.copy(tmpfile <- values$image %>% image_rotate(input$rotation) %>% image_write(tempfile(fileext='png'), format = 'png'), file)
    }
  )
  

  
  #======================= PLOTTING =======================
  observeEvent(input$plot_upload, {
    if (length(input$plot_upload$datapath))

      values$path <- NULL
      values$path <- input$plot_upload$datapath
      
      values$uploaded_image <- NULL
      values$uploaded_image <- image_read(input$plot_upload$datapath)

      values$image <- values$uploaded_image

      info <- image_info(values$image)
      #updateTextInput(session, "size", value = paste(info$width, info$height, sep = "x"))

      #Clean up masks
      values$mask_list_df <- values$mask_list_df[0,]
  })
  
  
  output$plot_output <- renderImage({
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
      path <- values$path
      df = list()

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
      
    })

  #observe(data)
  
  #Set plot type based on button click
  observeEvent(input$plotnodes, {values$plot_type = 'nodes'; print(paste0('plot type changed to:', values$plot_type))})
  observeEvent(input$plotbreaks, {values$plot_type = 'breaks'; print(paste0('plot type changed to:', values$plot_type))})
  observeEvent(input$plotline, {values$plot_type = 'line'; print(paste0('plot type changed to:', values$plot_type))})
  observeEvent(input$plotword, {values$plot_type = 'word'; print(paste0('plot type changed to:', values$plot_type))})
  observeEvent(input$plotgraph, {values$plot_type = 'graph'; })
  
  #Plot specific plot based on button pressed
  output$plot_output <- renderPlot({
    req(processHandwriting_data$df)
    imgList = processHandwriting_data$df
    if (values$plot_type == 'nodes'){ plotNodes(imgList$image, imgList$thin, imgList$nodes) }
    else if (values$plot_type == 'breaks'){ plotNodes(imgList$image, imgList$thin, imgList$breaks) }
    else if (values$plot_type == 'line'){ plotLine(imgList$letterList, input$linenum, imgList$dims) }
    else if (values$plot_type == 'word'){ plotWord(imgList$letterList, input$wordnum, imgList$dims) }
    else if (values$plot_type == 'graph'){ plotLetter(imgList$letterList, input$graphnum, imgList$dims) }
    else { plotImageThinned(imgList$image, imgList$thin) }
  })
  
  

  
  #======================= FEATURE EXTRACTION =======================
  
  
  #======================= K-MEANS CLUSTERING =======================
  
  
  #======================= TRIANGLE DECOMPOSITION =======================
  
  
}

runGadget(ui, server, viewer = dialogViewer("Shiny HandwritR", width = 1800, height = 900))
#runApp(launch.browser = TRUE)
#shinyApp(ui, server)


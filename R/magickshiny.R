

#UI
ui <- fluidPage(
  add_busy_bar(color = "#0E86D4"),
  tags$head(tags$script(src = "message-handler.js"), 
            tags$style(HTML("
              input[type=\"number\"] {
                width: 80px;
              }"))),
  sidebarLayout(
    sidebarPanel(width = 4,
                 h3("Shiny handwritR"),
                 p("Edit, process, and plot handwriting"),
                 br(),
                 fileInput("upload", "Upload new document", accept = c('image/png')),
    ),
    mainPanel(width = 8,
              h3("Current Document:"),
              imageOutput("topPlot"),
              br(),
              fluidRow(
                column(width = 4, sliderInput("rotation", "Rotate:", min = -180, max = 180, value = 0)),
                column(width = 4, textInput("size", "Size", value = "0x0")),
                column(width = 4, p("To crop, click and drag over the area - then hit 'crop'"), actionButton("crop", "Crop")) 
              ),
    )
  ),
  tabsetPanel(
    tabPanel("Plotting", br(), "Plotting to come soon"),
    tabPanel("Feature Extractions", br(), "Feature Extraction to come soon"),
    tabPanel("k-means Clustering", br(), "k-means Clustering to come soon"),
    tabPanel("Triangle Decomposition", br(), "Triangle Decomposition to come soon")
  ),        
)


#SERVER
server <- function(input, output, session) {
  
  library(magick)
  
  # Start with placeholder img
  image <- image_read("https://csafe-isu.github.io/handwriter/resources/images/Writing_csafe_single.png")
  
  # A plot of fixed size
  output$topPlot <- renderImage({

    
    # Numeric operators
    tmpfile <- image %>%
      image_rotate(input$rotation) %>%
      image_resize(input$size) %>%
      image_write(tempfile(fileext='jpg'), format = 'jpg')
    
    # Return a list
    list(src = tmpfile, contentType = "image/jpeg")
  }, deleteFile = FALSE)
  
  observeEvent(input$upload, {
    if (length(input$upload$datapath))
      image <<- image_read(input$upload$datapath)
    info <- image_info(image)
    updateTextInput(session, "size", value = paste(info$width, info$height, sep = "x"))
  })
  
  
}

runGadget(ui, server, viewer = dialogViewer("Shiny HandwritR", width = 1400, height = 900))
shinyApp(ui, server)
library(magick)


ui <- fluidPage(
  titlePanel("Magick Shiny Demo"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("upload", "Upload new image", accept = c('image/png', 'image/jpeg')),
      textInput("size", "Size", value = "500x500!"),
      sliderInput("rotation", "Rotation", 0, 360, 0),
      sliderInput("blur", "Blur", 0, 20, 0),
      sliderInput("implode", "Implode", -1, 1, 0, step = 0.01),
      
      checkboxGroupInput("effects", "Effects",
                         choices = list("edge", "charcoal", "negate", "flip", "flop")),
      downloadButton('downloadImage', 'Download modified image')
      
    ),
    mainPanel(
      imageOutput("img")
    )
  )
)

server <- function(input, output, session) {
  
  
  # Start with placeholder img
  imageLoc <- reactiveVal("https://raw.githubusercontent.com/ThinkR-open/collage/master/inst/tigrou/tigrou.jpg")
  ## convert the img location to an img value
  imageVal <- reactive({
    image_convert(image_read(imageLoc()), "jpeg")
  })
  
  # When uploading new image
  observeEvent(input$upload, {
    if (length(input$upload$datapath)) {
      ## set the image location
      imageLoc(input$upload$datapath)
    }
    updateCheckboxGroupInput(session, "effects", selected = "")
  })
  
  ## if the image information ever updates, set the info values
  observe({
    info <- image_info(imageVal())
    updateTextInput(session, "size", value = paste0(info$width, "x", info$height, "!"))
  })
  
  updatedImageLoc <- reactive({
    ## retrieve the imageVal
    image <- imageVal()
    
    # Boolean operators
    if("edge" %in% input$effects)
      image <- image_edge(image)
    
    if("charcoal" %in% input$effects)
      image <- image_charcoal(image)
    
    if("negate" %in% input$effects)
      image <- image_negate(image)    
    
    if("flip" %in% input$effects)
      image <- image_flip(image)
    
    if("flop" %in% input$effects)
      image <- image_flop(image)
    
    # Numeric operators
    tmpfile <- image %>%
      image_resize(input$size) %>%
      image_implode(input$implode) %>%
      image_blur(input$blur, input$blur) %>%
      image_rotate(input$rotation) %>%
      image_write(tempfile(fileext='jpg'), format = 'jpg')
    
    ## return only the tmp file location
    tmpfile
  })
  
  # A plot of fixed size
  output$img <- renderImage(
    {
      # Return a list
      list(src = updatedImageLoc(), contentType = "image/jpeg")
    }, 
    ## DO NOT DELETE THE FILE!
    deleteFile = FALSE
  )
  
  output$downloadImage <- downloadHandler(
    filename = "Modified_image.jpeg",
    contentType = "image/jpeg",
    content = function(file) {
      ## copy the file from the updated image location to the final download location
      file.copy(updatedImageLoc(), file)
    }
  )  
}

shinyApp(ui, server)
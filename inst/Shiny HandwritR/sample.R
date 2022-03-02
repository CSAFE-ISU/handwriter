# Demo of clicking, hovering, brushing with imageOutput
# Note that coordinates are in pixels
shinyApp(
  ui = basicPage(
    fluidRow(
      column(width = 4,
             imageOutput("image", height=300,
                         click = "image_click",
                         hover = hoverOpts(
                           id = "image_hover",
                           delay = 500,
                           delayType = "throttle"
                         ),
                         brush = brushOpts(id = "image_brush")
             )
      ),
      column(width = 4,
             verbatimTextOutput("image_clickinfo"),
             verbatimTextOutput("image_hoverinfo")
      ),
      column(width = 4,
             wellPanel(actionButton("newimage", "New image")),
             verbatimTextOutput("image_brushinfo")
      )
    )
  ),
  server = function(input, output, session) {
    output$image <- renderImage({
      input$newimage
      
      # Get width and height of image output
      width  <- session$clientData$output_image_width
      height <- session$clientData$output_image_height
      
      # Write to a temporary PNG file
      outfile <- tempfile(fileext = ".png")
      
      png(outfile, width=width, height=height)
      plot(rnorm(200), rnorm(200))
      dev.off()
      
      # Return a list containing information about the image
      list(
        src = outfile,
        contentType = "image/png",
        width = width,
        height = height,
        alt = "This is alternate text"
      )
    })
    output$image_clickinfo <- renderPrint({
      cat("Click:\n")
      str(input$image_click)
    })
    output$image_hoverinfo <- renderPrint({
      cat("Hover (throttled):\n")
      str(input$image_hover)
    })
    output$image_brushinfo <- renderPrint({
      cat("Brush (debounced):\n")
      str(input$image_brush)
    })
  }
)



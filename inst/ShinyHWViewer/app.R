#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  fluidRow(
    column(5, offset = 1, h1("Handwriting Viewer")),
    column(5, offset = 1, fileInput("filePath", "Choose handwriter R object:"))),
  hr(),
    fluidRow(
       column(7, align="center",
           plotOutput("letterPlot",
                brush = brushOpts(
                  id = "letterPlot_brush",
                  resetOnNew = TRUE
                )
          )
      ),
      column(5, align="center", style="margin-top:100px;",
        plotOutput("zoomedPlot")
      )
    )
)

server <- function(input, output) {
  if(!require(handwriter))
  {
    devtools::install_github("CSAFE-ISU/handwriter")
    require(handwriter)
  }

  data <- reactive({
    req(input$filePath)
    path <- input$filePath$datapath
    df <- readRDS(file = path)
    return(df)
  })
  output$letterPlot <- renderPlot({
    plotImageThinned(data()$image, data()$thin)
  })
  letterRanges <- reactiveValues(x = NULL, y = NULL)

  output$zoomedPlot <- renderPlot({
    if(!is.null(letterRanges$x) & !is.null(letterRanges$y))
    {
      for(i in 1:2){
        if(letterRanges$x[i] < 0) letterRanges$x[i] = 1
        else if(letterRanges$x[i] > dim(data()$image)[2] - 1) letterRanges$x[i] = dim(data()$image)[2]- 1
      }
      for(i in 1:2){
        if(letterRanges$y[i] < 0) letterRanges$y[i] = 1
        else if(letterRanges$y[i] > dim(data()$image)[1] - 1) letterRanges$y[i] = dim(data()$image)[1] - 1
      }

      letterRanges$x = round(letterRanges$x)
      letterRanges$y = round(letterRanges$y)
      subimage = data()$image[(dim(data()$image)[1] - letterRanges$y[2] + 1):(dim(data()$image)[1] - letterRanges$y[1] + 1),][,(letterRanges$x[1] + 1):(letterRanges$x[2] + 1)]
      subthindf = data.frame(X = ((data()$thin - 1) %/% dim(data()$image)[1]) + 1, Y = ((data()$thin - 1) %% dim(data()$image)[1]) + 1)
      subthindf = as.data.frame(t(t(subthindf) - c(letterRanges$x[1], dim(data()$image)[1] - letterRanges$y[2])))
      subthindf = subthindf[between(subthindf$X, 1, letterRanges$x[2] - letterRanges$x[1] + 1) & between(subthindf$Y, 1, letterRanges$y[2] - letterRanges$y[1] + 1),]
      subthin = (subthindf$X - 1)*dim(subimage)[1] + subthindf$Y + 1
   
      points = data.frame(X = ((data()$nodes - 1) %/% dim(data()$image)[1]) + 1, Y = ((data()$nodes - 1) %% dim(data()$image)[1]) + 1)
      points = as.data.frame(t(t(points) - c(letterRanges$x[1], dim(data()$image)[1] - letterRanges$y[2] - 1)))
      points = points[between(points$X, 1, letterRanges$x[2] - letterRanges$x[1] + 1) & between(points$Y, 1, letterRanges$y[2] - letterRanges$y[1] + 1),]
   
      yheight = letterRanges$y[2] - letterRanges$y[1] + 2
      points$Y = yheight - points$Y
      plotImageThinned(subimage, subthin) + geom_point(data = points, aes(X, Y), shape = I("o"), size = I(6), color = I("red"))
    }
  })

  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$letterPlot_brush
    if (!is.null(brush)) {
      letterRanges$x <- c(brush$xmin, brush$xmax)
      letterRanges$y <- c(brush$ymin, brush$ymax)

    } else {
      letterRanges$x <- NULL
      letterRanges$y <- NULL
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)


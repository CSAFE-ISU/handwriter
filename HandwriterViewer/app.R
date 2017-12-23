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
  library(handwriter)
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
    points = data.frame(X = ((data()$nodes - 1) %/% dim(data()$image)[1]) + 1, Y = dim(data()$image)[1] - ((data()$nodes - 1) %% dim(data()$image)[1]))
    plotImageThinned(data()$image, data()$thin) +
      geom_point(data = points, aes(X, Y), shape = I("o"), size = I(6), color = I("red")) +
      coord_cartesian(xlim = letterRanges$x, ylim = letterRanges$y, expand = FALSE)
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


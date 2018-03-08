#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# devtools::install_github("CSAFE-ISU/handwriter")
# Rcpp::sourceCpp(file = "~/src/ThinImageCpp.cpp")
library(shiny)
options(shiny.sanitize.errors = FALSE)

between = function(x, left, right)
{
  x <= max(left, right) & x >= min(left, right)
}
index2subindex = function(index, x1, x2, y1, y2, d1)
{
  index = index[between(((index - 1) %/% d1 + 1), x1, x2) & between(((index - 1) %% d1 + 1), d1-y2+1, d1-y1+1)]
  index - (x1-1)*d1 - (d1-y2)*(((index-1) %/% d1 + 1) - x1 + 1) - (y1 - 1)*((index - 1) %/% d1 - x1 + 1)
}

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
      column(5, align="center", style="margin-top:10px;",
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
    df = list()
    df$image = crop(readPNGBinary(path))
    df$thin = thinImage(df$image)
    df$nodes = getNodes(df$thin, dim(df$image))
    return(df)
  })
  output$letterPlot <- renderPlot({
    imgList = data()
    plotImageThinned(imgList$image, imgList$thin)
  })
  letterRanges <- reactiveValues(x = NULL, y = NULL)

  output$zoomedPlot <- renderPlot({
    imgList = data()

    if(!is.null(letterRanges$x) & !is.null(letterRanges$y))
    {
      xlims = floor(letterRanges$x)+1
      ylims = floor(letterRanges$y)+1
      
      for(i in 1:2){
        if(xlims[i] <= 0) xlims[i] = 1
        else if(xlims[i] > dim(imgList$image)[2]) xlims[i] = dim(imgList$image)[2]
      }
      for(i in 1:2){
        if(ylims[i] <= 0) ylims[i] = 1
        else if(ylims[i] > dim(imgList$image)[1]) ylims[i] = dim(imgList$image)[1]
      }
      
      subimage = imgList$image[(dim(imgList$image)[1] - ylims[2] + 1):(dim(imgList$image)[1] - ylims[1] + 1),(xlims[1]):(xlims[2])]
      subthin = index2subindex(imgList$thin, xlims[1], xlims[2], ylims[1], ylims[2], dim(imgList$image)[1])
      subnodes = index2subindex(imgList$nodes, xlims[1], xlims[2], ylims[1], ylims[2], dim(imgList$image)[1])
      
      plotNodes(subimage, subthin, subnodes, nodeSize = 6, nodeColor = "red") + ggplot2::theme(panel.border = ggplot2::element_rect(colour = "gray", fill=NA, size=.3))
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


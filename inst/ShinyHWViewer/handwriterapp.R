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
{F
  index = index[between(((index - 1) %/% d1 + 1), x1, x2) & between(((index - 1) %% d1 + 1), d1-y2+1, d1-y1+1)]
  index - (x1-1)*d1 - (d1-y2)*(((index-1) %/% d1 + 1) - x1 + 1) - (y1 - 1)*((index - 1) %/% d1 - x1 + 1)
}

ui <- fluidPage(
  tags$head(tags$script(src = "message-handler.js"), 
            tags$style(HTML("
              input[type=\"number\"] {
                width: 80px;
              }"))),
  fluidRow(
    column(3, offset = 2, h1("Handwriter")),
    column(5, offset = 1, fileInput("filePath", "Choose handwriting (.png) to process:"))),
  
  fluidRow(
    column(12, align="center", plotOutput("letterPlot", dblclick = "letterPlot",
      brush = brushOpts(id = "letterPlot_brush", resetOnNew = TRUE)
    ))
  ),
 hr(),  
 fluidRow(column(3,
       fluidRow(
        splitLayout(cellWidths = c("50%", "50%"),
           actionButton("plotnodes", "Plot Nodes"), 
           actionButton("plotbreaks", "Plot Breaks"))),
       br(),
       fluidRow(
         splitLayout(cellWidths = c("50%", "50%"),
           actionButton("plotline", "Plot Line"), 
           numericInput("linenum", "Line Number", 1))),
       
       fluidRow(
         splitLayout(cellWidths = c("50%", "50%"),
           actionButton("plotword", "Plot Word"), 
           numericInput("wordnum", "Word Number", 1))),
       
       fluidRow(
         splitLayout(cellWidths = c("50%", "50%"),
           actionButton("plotletter", "Plot Letter"), 
           numericInput("letternum", "Letter Number", 1))),
   ),
     column(9, align="center", plotOutput("outputPlot", dblclick = "outputPlot",
       brush = brushOpts(id = "outputPlot_brush", resetOnNew = TRUE)
     )),
  ),
  
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
  
  #basic output
  output$letterPlot <- renderPlot({
    imgList = data()
    plotImageThinned(imgList$image, imgList$thin)
  })
}

# Run the application
shinyApp(ui = ui, server = server)


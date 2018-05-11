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
# For data storage via a shiny app, I used 
# https://shiny.rstudio.com/articles/persistent-data-storage.html#gsheets
library(googlesheets)
library(shiny)
options(shiny.sanitize.errors = FALSE)

key <- "1myKHYX1YTJHbiyDhQ_YjnuO5Ft0kPrB7DQ-OG3go_sM"

saveData <- function(data) {
  # Grab the Google Sheet
  sheet <- gs_key(key)
  # Add the data as a new row
  gs_add_row(sheet, input = data)
}


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
    column(5, offset = 1, textInput("user", label = "User: (Enter your initials.)", value = "")),
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
        plotOutput("zoomedPlot", 
                   brush = brushOpts(
                     id = "idPlot_brush",
                     resetOnNew = TRUE
                   ))
      )
    ),
    fluidRow(
      column(7, align = "center",
             textInput("letter", label = h3("What letter is written? (Enter a single letter, no spaces)"), value = ""),
             actionButton("save", label = "Save Data")
             ),
      column(5, align = "center",
             plotOutput("dblzoomPlot")
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
    req(data())
    imgList = data()
    plotImageThinned(imgList$image, imgList$thin)
  })
  
  letterRanges <- reactiveValues(x = NULL, y = NULL)

  subdata <- reactive({
    req(data())
    imgList <- data() 
    if(!is.null(letterRanges$x) & !is.null(letterRanges$y)){
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

      sub_df <- list() 
      sub_df$image = imgList$image[(dim(imgList$image)[1] - ylims[2] + 1):(dim(imgList$image)[1] - ylims[1] + 1),(xlims[1]):(xlims[2])]
      sub_df$thin = index2subindex(imgList$thin, xlims[1], xlims[2], ylims[1], ylims[2], dim(imgList$image)[1])
      sub_df$nodes = index2subindex(imgList$nodes, xlims[1], xlims[2], ylims[1], ylims[2], dim(imgList$image)[1])
      return(sub_df)
    }  
  })  
  
  letterRanges2 <- reactiveValues(x = NULL, y = NULL)
  
  subsubdata <- reactive({
    req(subdata())
    subimgList <- subdata() 
    if(!is.null(letterRanges2$x) & !is.null(letterRanges2$y)){
      xlims = floor(letterRanges2$x)+1
      ylims = floor(letterRanges2$y)+1
      for(i in 1:2){
        if(xlims[i] <= 0) xlims[i] = 1
        else if(xlims[i] > dim(subimgList$image)[2]) xlims[i] = dim(subimgList$image)[2]
      }
      for(i in 1:2){
        if(ylims[i] <= 0) ylims[i] = 1
        else if(ylims[i] > dim(subimgList$image)[1]) ylims[i] = dim(subimgList$image)[1]
      }
      
      subsub_df <- list() 
      subsub_df$image = subimgList$image[(dim(subimgList$image)[1] - ylims[2] + 1):(dim(subimgList$image)[1] - ylims[1] + 1),(xlims[1]):(xlims[2])]
      subsub_df$thin = index2subindex(subimgList$thin, xlims[1], xlims[2], ylims[1], ylims[2], dim(subimgList$image)[1])
      subsub_df$nodes = index2subindex(subimgList$nodes, xlims[1], xlims[2], ylims[1], ylims[2], dim(subimgList$image)[1])
      return(subsub_df)
    }  
  })  
  
  
  output$zoomedPlot <- renderPlot({
      req(subdata())
      subimgList = subdata()
      
      plotNodes(subimgList$image, subimgList$thin, subimgList$nodes, nodeSize = 6, nodeColor = "red") + ggplot2::theme(panel.border = ggplot2::element_rect(colour = "gray", fill=NA, size=.3))
  })
  
  # double zoom plot
  output$dblzoomPlot <- renderPlot({
      req(subsubdata())
      subsubimgList = subsubdata()
    
      plotNodes(subsubimgList$image, subsubimgList$thin, subsubimgList$nodes, nodeSize = 6, nodeColor = "red") + ggplot2::theme(panel.border = ggplot2::element_rect(colour = "gray", fill=NA, size=.3))
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
    brush2 <- input$idPlot_brush
    if (!is.null(brush2)) {
      letterRanges2$x <- c(brush2$xmin, brush2$xmax)
      letterRanges2$y <- c(brush2$ymin, brush2$ymax)
      
    } else {
      letterRanges2$x <- NULL
      letterRanges2$y <- NULL
    }
    
  })
  
  obs_letter_info <- reactive({
  letterData <- data.frame(filename = input$filePath$name,
                           letter = input$letter,
                           data_xmin = letterRanges$x[1],
                           data_xmax = letterRanges$x[2],
                           data_ymin = letterRanges$y[1],
                           data_ymax = letterRanges$y[2],
                           subdata_xmin = letterRanges2$x[1],
                           subdata_xmax = letterRanges2$x[2],
                           subdata_ymin = letterRanges2$y[1],
                           subdata_ymax = letterRanges2$y[2],
                           time = Sys.time(), 
                           user = input$user)
      letterData
  }) 
  observeEvent(input$save, {
      saveData(obs_letter_info()[1,])
  })
}

# Run the application
shinyApp(ui = ui, server = server)


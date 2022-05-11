ui <- shinyUI({
  fluidPage(
    shinyjs::useShinyjs(),
    shinyBS:::shinyBSDep,
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
                              h2("Pre-process"),
                              br(),
                              h4("Current document:"),
                              fluidRow(column(width=11, offset=1, textOutput("image_name"))),
                              fluidRow(column(width=11, offset=1, textOutput("dimensions"))),
                              br(),
                              fileInput("upload", "Choose a new document to pre-process", accept = c('image/png')),
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
                                column(width = 6, offset = 6, downloadButton("save_document", "Save Document")),
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
                                                  column(width = 2, downloadButton("save_mask", "Save Mask"))
                                                  
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
                              h2("Plotting"),
                              br(),
                              h4("Current document:"),
                              fluidRow(column(width=11, offset=1, textOutput("plot_image_name"))),
                              fluidRow(column(width=11, offset=1, textOutput("plot_dimensions"))),
                              br(),
                              fileInput("plot_upload", "Choose a new document to plot", accept = c('image/png')),
                              br(),
                              fluidRow(
                                column(6, offset = 2, actionButton("processhandwriting", "Process Handwriting"))
                              ), br(), br(),
                              fluidRow(
                                column(4, offset = 1, actionButton("plotbinarized", "Plot Binarized")),
                                column(4, actionButton("plotthinned", "Plot Thinned"))
                              ), br(),
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
                 mainPanel(width = 9, 
                           textOutput("plot_output_title"),
                           plotOutput("plot_output"),
                           hr(),
                           h3("Current image"),
                           plotOutput("plot_image")),
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
                              h3("k-means Clustering"),
                              br(),
                              fileInput("upload", "Choose document to explore", accept = c('image/png')),
                              
                 ),
                 mainPanel(width = 9, h1("Placeholder Tab"), ("kmeans_output"))
               ),
      ),
      
      #TRIANGLE DECOMPOSITION
      tabPanel("Triangle Decomposition", 
               sidebarLayout(
                 sidebarPanel(width = 3,
                              h3("Triangle Decomposition"),
                              br(),
                              fileInput("upload", "Choose a document or directory to decompose", accept = c('image/png')),
                              
                 ),
                 mainPanel(width = 9, h1("Placeholder Tab"), plotOutput("triangle_output"))),
      )),
)})

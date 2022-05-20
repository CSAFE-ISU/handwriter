ui <- shinyUI({
  fluidPage(
    shinyjs::useShinyjs(),
    shinyBS:::shinyBSDep,
    add_busy_spinner(spin = "fading-circle"),
    #span(textOutput("error"), style="color:red"),
    tags$head(tags$script(src = "message-handler.js"), 
              tags$style(HTML("input[type=\"number\"] {width: 80px;}")),
              tags$style(HTML("hr {border-top: 1px solid #000000;}")),
              tags$style(HTML('#save_document{background-color:#33ADFF} #save_document:hover{background-color:#3398FF} #save_document{color:white}')),
              tags$style(HTML('#save_mask{background-color:#33ADFF} #save_mask:hover{background-color:#3398FF} #save_mask{color:white}'))),
    
    navbarPage(
      title = "Shiny handwriter",
      
      tabPanel("Intro",
                 h1("Welcome to Shiny handwriter!"),
                 h3("Shiny handwriter is a handwritten document analysis tool created by CSAFE"),
                 br(), br(),
                 h3("Whats avaialble:"),
                 tags$div(tags$ul(
                   tags$li("Preproces | Use rotation, resizing, cropping, and masking to clean up your data"),
                   tags$li("Plot | Plot your document as well as its lines, words, or graphs"),
                   tags$li("Explore Features | Extract and explore valuable features from your documents")
                 ), style = "font-size: 15px"), hr(),
                 h4("COMING SOON: Two statistical analysis tools to determine probability of writership"),
                 tags$div(tags$ul(
                   tags$li("k-means Clustering | Perform k-means clustering on a set of documents"),
                   tags$li("Triangle Decomposition | Do Kniser Triangle Decomposition & compare on your documents"),
                 ),  style = "font-size: 15px"),
                 br(),br(),
                 HTML('<p> Learn more about handwriter on our <a href = "https://csafe-isu.github.io/handwriter/index.html"> website </a> or at our <a href = "https://github.com/CSAFE-ISU/handwriter"> Github </a></p>')
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
                                column(6, offset = 4, actionButton("plot_processhandwriting", "Process Handwriting"))
                              ), br(), br(),
                              fluidRow(
                                column(4, offset = 2, actionButton("plotbinarized", "Plot Binarized")),
                                column(4, actionButton("plotthinned", "Plot Thinned"))
                              ), br(),
                              fluidRow(
                                column(4, offset = 2, actionButton("plotnodes", "Plot Nodes")),
                                column(4, actionButton("plotbreaks", "Plot Breaks"))
                              ), br(),
                              fluidRow(
                                column(4, offset = 2, br(), actionButton("plotline", "Plot Line")),
                                column(4, numericInput("linenum", "Line Number", 1))
                              ), br(),
                              fluidRow(
                                column(4, offset = 2, br(), actionButton("plotword", "Plot Word")),
                                column(4, numericInput("wordnum", "Word Number", 1))
                              ), br(),
                              fluidRow(
                                column(4, offset = 2, br() , actionButton("plotgraph", "Plot Graph")),
                                column(4, numericInput("graphnum", "Graph Number", 1))
                              ),
                              
                 ),
                 mainPanel(width = 9, 
                           textOutput("plot_output_title"),
                           plotOutput("plot_output"),
                           hr(),
                           h3("Current image"),
                           plotOutput("plot_image"))
               ),
      ),
      
      #FEATURE EXPLORATION
      tabPanel("Explore Features",
               sidebarLayout(
                 sidebarPanel(width = 3,
                              h3("Explore Features"),
                              br(),
                              h4("Current document:"),
                              fluidRow(column(width=11, offset=1, textOutput("features_image_name"))),
                              fluidRow(column(width=11, offset=1, textOutput("features_dimensions"))),
                              br(),
                              fileInput("features_upload", "Choose document to explore", accept = c('image/png')),
                              fluidRow(column(width=4, offset=7, actionButton("features_processhandwriting", "Process Handwriting"))),
                              br(), hr(), hr(),
                              h3("Batch Processing"),
                              shinyDirButton("batch_input_dir", "Input directory", "Choose an input directory"),
                              verbatimTextOutput("batch_input_dir", placeholder = TRUE),
                              shinyDirButton("batch_output_dir", "Output directory", "Choose an output directory"),
                              verbatimTextOutput("batch_output_dir", placeholder = TRUE),
                              fluidRow(column(width=3, offset=8, actionButton("process_batch", "Process Batch")))
                              
                 ),
                 mainPanel(width = 9, 
                           tabsetPanel(id = "processset",
                                       tabPanel("Document",
                                                h2("Document information after processing"),
                                                fluidRow(column(2, h4('Name')),
                                                         column(1, h4('Type')),
                                                         column(1, h4('Size')),
                                                         column(8, h4('Value'))),
                                                hr(), br(),
                                                fluidRow(style = "background-color:#ECECEC;", column(2, strong(id = 'document_image', "image")),
                                                         column(1, textOutput("features_document_image_type")),
                                                         column(1, textOutput("features_document_image_size")),
                                                         column(8, textOutput("features_document_image"))),
                                                fluidRow(column(2, strong(id = 'document_thin', "thin")),
                                                         column(1, textOutput("features_document_thin_type")),
                                                         column(1, textOutput("features_document_thin_size")),
                                                         column(8, textOutput("features_document_thin"))),
                                                hr(),
                                                fluidRow(style = "background-color:#ECECEC;", column(2, strong(id = 'document_nodes', "nodes")),
                                                         column(1, textOutput("features_document_nodes_type")),
                                                         column(1, textOutput("features_document_nodes_size")),
                                                         column(8, textOutput("features_document_nodes"))),
                                                fluidRow(column(2, strong(id = 'document_connectingNodes', "connectingNodes")),
                                                         column(1, textOutput("features_document_connectingNodes_type")),
                                                         column(1, textOutput("features_document_connectingNodes_size")),
                                                         column(8, textOutput("features_document_connectingNodes"))),
                                                fluidRow(style = "background-color:#ECECEC;", column(2, strong(id = 'document_terminalNodes', "terminalNodes")),
                                                         column(1, textOutput("features_document_terminalNodes_type")),
                                                         column(1, textOutput("features_document_terminalNodes_size")),
                                                         column(8, textOutput("features_document_terminalNodes"))),
                                                fluidRow(column(2, strong(id = 'document_breakPoints', "breakPoints")),
                                                         column(1, textOutput("features_document_breakPoints_type")),
                                                         column(1, textOutput("features_document_breakPoints_size")),
                                                         column(8, textOutput("features_document_breakPoints"))),
                                                hr(),
                                                imageOutput("features_document")
                                       ),
                                                
                            
                                       tabPanel("Word",
                                                h2("Word information after processing"),
                                                h1("PLACEHOLDER"), br(),
                                                h1("PLACEHOLDER"), br(),
                                                h1("PLACEHOLDER"), br(),
                                                h1("PLACEHOLDER"), br(),
                                                h1("PLACEHOLDER"), br(),
                                                h1("PLACEHOLDER"), br(),
                                                hr(), br(),
                                                DT::dataTableOutput("word_dt")

                                       ),
                                       
                                       tabPanel("Graph",
                                                h2("Graph information after processing"),
                                                hr(), br(),
                                                DT::dataTableOutput("graph_dt"),
                                                hr(),
                                                
                                                fluidRow(
                                                  column(2,
                                                    fluidRow(
                                                      numericInput("features_graphnum", "Enter a graph number to investigate further", 1),
                                                      fluidRow(imageOutput("features_graph"))
                                                    )
                                                  ),
                                                  column(1, ),
                                                  column(9, 
                                                     fluidRow(style = "background-color:#ECECEC;", 
                                                              column(1, offset = 1, strong(id = 'graph_aspect_ratio', "aspect_ratio")), column(2, textOutput("features_graph_aspect_ratio")),
                                                              column(1, strong(id = 'graph_height', "height")), column(2, textOutput("features_graph_height")),
                                                              column(1, strong(id = 'graph_width', "width")), column(2, textOutput("features_graph_width")),
                                                     ),
                                                     fluidRow( 
                                                              column(1, offset = 1, strong(id = 'graph_topmost_row', "topmost_row")), column(2, textOutput("features_graph_topmost_row")),
                                                              column(1, strong(id = 'graph_bottom_row', "bottom_row")), column(2, textOutput("features_graph_bottom_row")),
                                                              column(1, strong(id = 'graph_leftmost_col', "leftmost_col")), column(2, textOutput("features_graph_leftmost_col")),     
                                                     ),
                                                     fluidRow(style = "background-color:#ECECEC;", 
                                                              column(1, offset = 1, strong(id = 'graph_leftmost_col', "rightmost_col")), column(2, textOutput("features_graph_rightmost_col")),
                                                              column(1, strong(id = 'graph_centroid_index', "centroid_index")), column(2, textOutput("features_graph_centroid_index")),
                                                              column(1, strong(id = 'graph_centroid_y', "centroid_y")), column(2, textOutput("features_graph_centroid_y")),     
                                                     ),
                                                     fluidRow(
                                                              column(1, offset = 1, strong(id = 'graph_centroid_x', "centroid_x")), column(2, textOutput("features_graph_centroid_x")),
                                                              column(1, strong(id = 'graph_horiz_location', "centroid_horiz_location")), column(2, textOutput("features_graph_horiz_location")),
                                                              column(1, strong(id = 'graph_vert_location', "centroid_vert_location")), column(2, textOutput("features_graph_vert_location"))      
                                                     ),
                                                     fluidRow(style = "background-color:#ECECEC;", 
                                                              
                                                     )
                                                )  
                                              )
                                      )
                                ),
                    ),
      )
      ),
      
      
      
      #K-MEANS CLUSTERING
      tabPanel("k-means Clustering", 
               sidebarLayout(
                 sidebarPanel(width = 3,
                              h3("k-means Clustering"),
                              br(),
                              h5("Choose a template from the drop down, or create a new one with a new set of documents and the provided options"),
                              selectInput("download", "Select premade template for clustering", choices = c("CVL50", "IAM50", "CVL100")),
                              shinyDirButton("cluster_template_input_dir", "Template Directory", "Choose a template input directory"),
                              verbatimTextOutput("cluster_template_input_dir", placeholder = TRUE),
                              fluidRow(
                                column(4, numericInput("k", "K", 40)),
                                column(4, numericInput("numPathCuts", "Path Cuts", 8))
                              ),
                              fluidRow(
                                column(4, numericInput("iter.max", "Maximum Iterations", 10)),
                                column(4, numericInput("numOutliers", "numOutliers", 10))
                              ),
                              fluidRow(column(width=3, offset=8, actionButton("cluster_create_template", "Create Template"))),
                              hr(),
                              h5('Now choose a closed set directory and question document'),
                              shinyDirButton("cluster_closed_input_dir", "Closed Set Directory", "Choose a closed set directory"),
                              verbatimTextOutput("cluster_closed_input_dir", placeholder = TRUE),
                              fileInput("cluster_question_document", "Choose question document", accept = c('image/png')),
                              h5('Optionally, you can choose an output directory for results'),
                              shinyDirButton("cluster_output_dir", "Output Directory", "Choose an output directory"),
                              verbatimTextOutput("cluster_output_dir", placeholder = TRUE),
                              fluidRow(column(width=3, offset=9, actionButton("cluster_analyze", "Analyze"))),
                              
                 ),
                 mainPanel(width = 9, h1("Sample Tab -- Sample Tab"),
                           tabsetPanel(id = "cluster_set",
                                       tabPanel("Graphs", imageOutput("cluster_graphs")),
                                       tabPanel("Writer Profiles", imageOutput("cluster_profiles"))
                                       )
                           ),
               ),
      ),
      
      
      
      #TRIANGLE DECOMPOSITION
      tabPanel("Triangle Decomposition", 
               sidebarLayout(
                 sidebarPanel(width = 3,
                              h3("Triangle Decomposition"),
                              br(),
                              h5("Upload known documents and question document"),
                              shinyDirButton("triangle_input_dir", "Set Directory", "Choose a set directory"),
                              verbatimTextOutput("triangle_input_dir", placeholder = TRUE),
                              fileInput("upload", "Choose question document", accept = c('image/png')),
                              fluidRow(column(width=3, offset=9, actionButton("cluster_analyze", "Analyze"))),
                              
                 ),
                 mainPanel(width = 9, h1("PLACEHOLDER"), br(), h1("PLACEHOLDER"), br(), h1("PLACEHOLDER"), br(), h1("PLACEHOLDER"), br(), h1("PLACEHOLDER"), br(), h1("PLACEHOLDER"), br(),
                           h1("PLACEHOLDER"), br(), h1("PLACEHOLDER"), br(), h1("PLACEHOLDER"), br(), h1("PLACEHOLDER"), br(), h1("PLACEHOLDER"), br(), h1("PLACEHOLDER"), br())),
      )),
)})

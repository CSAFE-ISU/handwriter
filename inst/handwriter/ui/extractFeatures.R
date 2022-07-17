#EXTRACT FEATURES TAB

tabPanel("Extract Features",
         sidebarLayout(
           sidebarPanel(width = 4,
                        h3("Extract Features"),
                        br(),
                        h4("Current document:"),
                        fluidRow(column(width=11, offset=1, textOutput("features_image_name"))),
                        fluidRow(column(width=11, offset=1, textOutput("features_dimensions"))),
                        br(),
                        fileInput("features_upload", "Choose document to explore", accept = c('image/png')),
                        fluidRow(column(width=2, offset=1, actionButton("features_processhandwriting", "Process Document")),
                                 column(width = 2, offset = 1, downloadButton("save_document_extract", "Save Document"))),
                        br(), hr(), hr(),
                        h3("Batch Processing"),
                        br(),
                        fileInput("batch_input_dir", "Choose an input directory", multiple = TRUE, accept = c('image/png')),
                        selectInput("batch_select", "Select a transformation for data once processed", choices = c("none", "document")),
                        textOutput("batch_select_text"),
                        br(),
                        fluidRow(column(width = 2, offset = 1, actionButton("process_batch", "Process Batch")),
                                 column(width = 2, offset = 1, downloadButton("save_batch", "Save Batch"))),
                        
                        
                        
           ),
           mainPanel(width = 8, 
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
                                                   h2('characterFeatures:'),
                                                   fluidRow(style = "background-color:#ECECEC;", 
                                                            column(3, offset = 1, strong(id = 'graph_aspect_ratio', "aspect_ratio")), column(2, textOutput("features_graph_aspect_ratio")),
                                                            column(3, strong(id = 'graph_height', "height")), column(2, textOutput("features_graph_height"))),
                                                   fluidRow( 
                                                     column(3, offset = 1, strong(id = 'graph_width', "width")), column(2, textOutput("features_graph_width")),
                                                     column(3, strong(id = 'graph_topmost_row', "topmost_row")), column(2, textOutput("features_graph_topmost_row"))),
                                                   fluidRow(style = "background-color:#ECECEC;", 
                                                            column(3, offset = 1, strong(id = 'graph_bottom_row', "bottom_row")), column(2, textOutput("features_graph_bottom_row")),
                                                            column(3, strong(id = 'graph_leftmost_col', "leftmost_col")), column(2, textOutput("features_graph_leftmost_col"))),
                                                   fluidRow(
                                                     column(3, offset = 1, strong(id = 'graph_rightmost_col', "rightmost_col")), column(2, textOutput("features_graph_rightmost_col")),
                                                     column(3, strong(id = 'graph_centroid_index', "centroid_index")), column(2, textOutput("features_graph_centroid_index"))),
                                                   fluidRow(style = "background-color:#ECECEC;", 
                                                            column(3, offset = 1, strong(id = 'graph_centroid_y', "centroid_y")), column(2, textOutput("features_graph_centroid_y")), 
                                                            column(3, strong(id = 'graph_centroid_x', "centroid_x")), column(2, textOutput("features_graph_centroid_x"))),
                                                   fluidRow(
                                                     column(3, offset = 1, strong(id = 'graph_horiz_location', "centroid_horiz_location")), column(2, textOutput("features_graph_centroid_horiz_location")),
                                                     column(3, strong(id = 'graph_vert_location', "centroid_vert_location")), column(2, textOutput("features_graph_centroid_vert_location"))),
                                                   fluidRow(style = "background-color:#ECECEC;",
                                                            column(3, offset = 1, strong(id = 'graph_lHalf', "lHalf")), column(2, textOutput("features_graph_lHalf")),
                                                            column(3, strong(id = 'graph_rHalf', "rHalf")), column(2, textOutput("features_graph_rHalf"))),
                                                   fluidRow(
                                                     column(3, offset = 1, strong(id = 'graph_disjoint_centroids_left', "disjoint_centroids$left")), column(2, textOutput("features_graph_disjoint_centroids_left")),
                                                     column(3, strong(id = 'graph_disjoint_centroids_right', "disjoint_centroids$right")), column(2, textOutput("features_graph_disjoint_centroids_right"))),
                                                   fluidRow(style = "background-color:#ECECEC;",
                                                            column(3, offset = 1, strong(id = 'graph_slope', "slope")), column(2, textOutput("features_graph_slope")),
                                                            column(3, strong(id = 'graph_pixel_density', "pixel_density")), column(2, textOutput("features_graph_pixel_density"))),
                                                   fluidRow(
                                                     column(3, offset = 1, strong(id = 'graph_box_density', "box_density")), column(2, textOutput("features_graph_box_density")),
                                                     column(3, strong(id = 'graph_uniqueid', "uniqueid")), column(2, textOutput("features_graph_uniqueid"))),
                                                   fluidRow(style = "background-color:#ECECEC;",
                                                            column(3, offset = 1, strong(id = 'graph_down_dist', "down_dist")), column(2, textOutput("features_graph_down_dist")),
                                                            column(3, strong(id = 'graph_line_number', "line_number")), column(2, textOutput("features_graph_line_number"))),
                                                   fluidRow(
                                                     column(3, offset = 1, strong(id = 'graph_order_within_line', "order_within_line")), column(2, textOutput("features_graph_order_within_line")),
                                                     column(3, strong(id = 'graph_l_neighbor_dist', "l_neighbor_dist")), column(2, textOutput("features_graph_l_neighbor_dist"))),
                                                   fluidRow(style = "background-color:#ECECEC;",
                                                            column(3, offset = 1, strong(id = 'graph_r_neighbor_dist', "r_neighbor_dist")), column(2, textOutput("features_graph_r_neighbor_dist")),
                                                            column(3, strong(id = 'graph_xvar', "xvar")), column(2, textOutput("features_graph_xvar"))),
                                                   fluidRow(
                                                     column(3, offset = 1, strong(id = 'graph_yvar', "yvar")), column(2, textOutput("features_graph_yvar")),
                                                     column(3, strong(id = 'graph_covar', "covar")), column(2, textOutput("features_graph_covar"))),
                                                   fluidRow(style = "background-color:#ECECEC;",
                                                            column(3, offset = 1, strong(id = 'graph_wordIndex', "wordIndex")), column(2, textOutput("features_graph_wordIndex"))
                                                            #column(2, strong(id = 'graph_line_number', "line_number")), column(2, textOutput("features_graph_line_number"))
                                                   )
                                            )  
                                          )
                                 )
                     ),
           ),
         )
)
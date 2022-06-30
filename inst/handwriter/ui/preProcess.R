#PREPROCESS TAB

tabPanel("Pre-process", 
         sidebarLayout(
           sidebarPanel(width = 4,
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
                          column(width = 2, offset = 6, downloadButton("save_document", "Save Document")),
                        )),
           mainPanel(width = 8,
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
)
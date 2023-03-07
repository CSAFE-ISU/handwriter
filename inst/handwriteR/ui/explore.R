# EXPLORE TAB

tabPanel("Explore How Handwriter Works",
         sidebarLayout(
           sidebarPanel(width = 4,
                        h2("Explore How Handwriter Works"),
                        p("handwriter breaks handwriting into component shapes called graphs."),
                        br(),
                        h4("Current document:"),
                        fluidRow(column(width=11, offset=1, textOutput("plot_image_name"))),
                        fluidRow(column(width=11, offset=1, textOutput("plot_dimensions"))),
                        br(),
                        fileInput("plot_upload", "Choose a new document", accept = c('image/png')),
                        br(),
                        fluidRow(
                          column(6, offset = 4, actionButton("plot_processhandwriting", "Process Handwriting"))
                        ), br(), br(),
                        fluidRow(
                          column(4, offset = 2, actionButton("plotbinarized", "Show Binarized")),
                          column(4, actionButton("plotthinned", "Show Thinned"))
                        ), br(),
                        fluidRow(
                          column(4, offset = 2, actionButton("plotnodes", "Show Nodes")),
                          column(4, actionButton("plotbreaks", "Show Breaks"))
                        ), br(),
                        fluidRow(
                          column(4, offset = 2, br(), actionButton("plotline", "Show Line")),
                          column(4, numericInput("linenum", "Line Number", 1))
                        ), br(),
                        fluidRow(
                          column(4, offset = 2, br(), actionButton("plotword", "Show Word")),
                          column(4, numericInput("wordnum", "Word Number", 1))
                        ), br(),
                        fluidRow(
                          column(4, offset = 2, br() , actionButton("plotgraph", "Show Graph")),
                          column(4, numericInput("graphnum", "Graph Number", 1))
                        ),
                        
           ),
           mainPanel(width = 8, 
                     textOutput("plot_output_title"),
                     plotOutput("plot_output"),
                     hr(),
                     h3("Current image"),
                     plotOutput("plot_image"))
         ),
)
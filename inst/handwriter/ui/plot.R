#PLOT TAB

tabPanel("Plot",
         sidebarLayout(
           sidebarPanel(width = 4,
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
           mainPanel(width = 8, 
                     textOutput("plot_output_title"),
                     plotOutput("plot_output"),
                     hr(),
                     h3("Current image"),
                     plotOutput("plot_image"))
         ),
)
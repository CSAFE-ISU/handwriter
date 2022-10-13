tabPanel("k-means Clustering", 
         sidebarLayout(
           sidebarPanel(width = 4,
                        h3("k-means Clustering"),
                        br(),
                        h5("Choose a template from the drop down, or create a new one with a new set of documents and the provided options"),
                        selectInput("cluster_template_select", "Select premade template for clustering", choices = c("CVL50", "IAM50", "CVL100")),
                        fileInput("cluster_template_input_dir", "Choose a template input directory", multiple = TRUE, accept = c('image/png')),
                        fluidRow(
                          column(4, numericInput("k", "K", 40)),
                          column(4, numericInput("iter.max", "Maximum Iterations", 10)),
                        ),
                        fluidRow(
                          column(4, numericInput("starting_seed", "Starting Seed", 10)),
                          
                        ),
                        br(),
                        fluidRow(column(width=3, offset=0, actionButton("cluster_create_template", "Create Template"))),
                        hr(),
                        h5('Now choose a closed set directory and question document'),
                        fileInput("cluster_closed_input_dir", "Choose closed set images", multiple = TRUE, accept = c('image/png')),
                        fileInput("cluster_question_document", "Choose question document", accept = c('image/png')),
                        h5('Once Completed, you can download your results'),
                        fluidRow(
                          column(width = 2, offset = 1, downloadButton("cluster_download", "Save Results")),
                          column(width = 2, offset = 1, actionButton("cluster_analyze", "Analyze"))
                        )
                        
                        
           ),
           mainPanel(width = 8, h1("Sample Tab -- Sample Tab"),
                     tabsetPanel(id = "cluster_set",
                                 tabPanel("Graphs",
                                          br(),
                                          h3('Sample clustering template (K = 40)'),
                                          imageOutput("cluster_graphs")),
                                 tabPanel("Writer Profiles", 
                                          br(),
                                          imageOutput("cluster_unknown"),
                                          tabsetPanel(id = "writer_set",
                                                      tabPanel("Writer 1", br(), imageOutput("cluster_1")),
                                                      tabPanel("Writer 2", br(), imageOutput("cluster_2")),
                                                      tabPanel("Writer 3", br(), imageOutput("cluster_3"))
                                          )
                                 )
                                 
                     )
           ),
         ),
)
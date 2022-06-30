#TRIANGLE DECOMPOSITION

tabPanel("Triangle Decomposition", 
         h1("TRIANGLE DECOMPOSITION IS A WORK IN PROGRESS. EXCUSE OUR MESS!"), hr(), hr(), hr(),
         sidebarLayout(
           sidebarPanel(width = 4,
                        h3("Triangle Decomposition"),
                        br(),
                        h5("Upload known documents and question document"),
                        fileInput("triangle_input_dir", "Choose a set of images", multiple = TRUE, accept = c('image/png')),
                        fileInput("upload", "Choose question document", accept = c('image/png')),
                        fluidRow(column(width=3, offset=9, actionButton("cluster_analyze", "Analyze"))),
                        
           ),
           mainPanel(width = 8, )
         ),
)
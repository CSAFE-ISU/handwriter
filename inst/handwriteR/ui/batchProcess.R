#EXTRACT FEATURES TAB

tabPanel("Batch Process",
         sidebarLayout(
           sidebarPanel(width = 4,
                        h3("Batch Process"),
                        br(),
                        fileInput("batch_input_dir", "Choose an input directory", multiple = TRUE, accept = c('image/png')),
                        br(),
                        fluidRow(column(width = 2, offset = 1, actionButton("process_batch", "Process Batch")),
                                 column(width = 2, offset = 3, downloadButton("save_batch", "Save Batch"))),
                        
                        
                        
           ),
           mainPanel(width = 8, 
           
           ),
         )
)
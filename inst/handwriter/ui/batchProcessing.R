#Batch Processing

tabPanel("Batch Processing", 
         sidebarLayout(
           sidebarPanel(width = 4,
                        h3("Batch Processing"),
                        br(),
                        fileInput("batch_input_dir", "Choose a Choose an input directory", multiple = TRUE, accept = c('image/png')),
                        fluidRow(column(width = 2, offset = 1, actionButton("process_batch", "Process Batch")),
                                 column(width = 2, offset = 1, downloadButton("save_batch", "Save Batch"))),
                        
                        
           ),
           mainPanel(width = 8, 
                     
           )
         )
)
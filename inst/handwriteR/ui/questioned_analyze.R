tabPanel("Analyze documents",
         h2("Analyze Questioned Documents"),
         sidebarLayout(
           sidebarPanel(
             # questioned docs
             directoryUI("q_docs", label="Questioned Documents"),
             
             # settings
             substringIndicesUI("q_writer_indices", "writer start character", "writer stop character"),
             substringIndicesUI("q_doccode_indices", "doc start character", "doc stop character"),
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Documents",
                        fluidRow(column(width=3, directoryContentsUI("q_docs_list")),
                                 column(width=2, substringsUI("q_writers")),
                                 column(width=3, substringsUI("q_doccodes"))),
                        hr(),
                        
                        displayImageUI("q_image", "Document")
               ),
               tabPanel("Writer Profiles",
                        # h4("Persons of Interest Writer Profiles"),
                        # helpText("This plot shows the median of the model parameters that estimate the true cluster fill rate for 
                        #          each person of interest and each cluster. The error bars show the 95% credible intervals."),
                        # plotOutput("q_profiles"),                         
               ),
             )
           )
         )
)
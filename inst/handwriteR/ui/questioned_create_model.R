tabPanel("Persons of Interest",
         h2("Persons of Interest"),
         sidebarLayout(
           sidebarPanel(
             # model docs
             directoryUI("poi_docs", label="Persons of Interest Documents"),
             hr(),
             
             # output dir
             directoryUI("poi_output", label="Output Directory"),
             helpText("The model and supporting files will be saved as model.rds in Output Directory > data."),
             hr(),
             
             h4("Model Settings"),
             substringIndicesUI("poi_writer_indices", "writer start character", "writer stop character"),
             substringIndicesUI("poi_doccode_indices", "doc start character", "doc stop character"),
             fluidRow(column(width=3, numericInput("poi_num_iters", "# MCMC iters", value=4000, min=100, step=1)),
                      column(width=3, numericInput("poi_chains", "# MCMC chains", value=1, min=1, max=10, step=1))),
             fluidRow(column(width=3, numericInput("poi_cores", "# cores", value=2, min=1, max=100, step=1))),
             
             # fit model
             actionButton("poi_create", "Fit model")
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Documents",
                        fluidRow(column(width=3, directoryContentsUI("poi_docs_list")),
                                 column(width=2, substringsUI("poi_writers")),
                                 column(width=3, substringsUI("poi_doccodes"))),
                        hr(),
                        
                        displayImageUI("poi_image", "Document")
               ),
               tabPanel("Writer Profiles",
                        h4("Persons of Interest Writer Profiles"),
                        helpText("This plot shows the median of the model parameters that estimate the true cluster fill rate for 
                                 each person of interest and each cluster. The error bars show the 95% credible intervals."),
                        plotOutput("poi_profiles"),                         
                        ),
             )
           )
         )
)
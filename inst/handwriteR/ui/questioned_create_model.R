tabPanel("STEP 3: Persons of Interest",
         h2("Persons of Interest"),
         p("handwriter will analyze known writing samples from the persons of interest (POI) and fit a statistical model to estimate each
           POI's writer profile."),
         sidebarLayout(
           sidebarPanel(
             h4("Main Directory"),
             directoryUI("poi_main_dir", label="Choose Directory"),
             helpText("The model and supporting files will be saved in Directory > data."),
             hr(),
             
             h4("Handwriting Samples from Persons of Interest"),
             directoryUI("poi_docs", label="Choose Directory"),
             helpText("Select a directory that contains handwriting samples from each person of interest."),
             hr(),
             
             h4("Statistical Model Settings"),
             substringIndicesUI("poi_writer_indices", "writer start character", "writer stop character"),
             substringIndicesUI("poi_doccode_indices", "doc start character", "doc stop character"),
             fluidRow(column(width=3, numericInput("poi_num_iters", "# MCMC iters", value=4000, min=100, step=1)),
                      column(width=3, numericInput("poi_chains", "# MCMC chains", value=1, min=1, max=10, step=1))),
             fluidRow(column(width=3, numericInput("poi_cores", "# cores", value=2, min=1, max=100, step=1))),
             hr(),
             
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
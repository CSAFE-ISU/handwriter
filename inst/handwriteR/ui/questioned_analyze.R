tabPanel("Analyze documents",
         h2("STEP 4: Analyze Questioned Documents"),
         sidebarLayout(
           sidebarPanel(
             # output directory
             directoryUI("q_main_dir", label="Main Directory"),
             helpText("Select the main directory you used to create the template and fit the model. The analysis and supporting 
                      files will be saved in Directory > data."),
             hr(),
             
             # load model
             loadUI("q_load_model", label="Load Model"),
             helpText("Load the model you created for handwriting samples from the persons of interest."),
             hr(),
             
             # questioned docs
             directoryUI("q_docs", label="Questioned Documents"),
             hr(),
             
             # settings
             substringIndicesUI("q_writer_indices", "writer start character", "writer stop character"),
             substringIndicesUI("q_doccode_indices", "doc start character", "doc stop character"),
             numericInput("q_cores", "# cores", value=1, min=1, max=100),
             hr(),
             
             # analyze button
             actionButton("q_analyze", "Analyze")
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Documents",
                        fluidRow(column(width=4, directoryContentsUI("q_docs_list")),
                                 column(width=2, substringsUI("q_writers")),
                                 column(width=3, substringsUI("q_doccodes"))),
                        hr(),
                        
                        displayImageUI("q_image", "Document")
               ),
               tabPanel("Writer Profiles",
                        h4("Writer Profiles in Questioned Documents"),
                        helpText("This plot shows the median of the model parameters that estimate the true cluster fill rate the writer
                                 of each questioned document and each cluster. The error bars show the 95% credible intervals."),
                        plotOutput("q_profiles"),
                        h4("Persons of Interest Writer Profiles"),
                        helpText("This plot shows the median of the model parameters that estimate the true cluster fill rate for 
                                 each person of interest and each cluster. The error bars show the 95% credible intervals."),
                        plotOutput("q_poi_profiles")
               ),
               tabPanel("Results",
                        h4("Posterior Probabilities of Writership"),
                        helpText("This table shows the posterior probability that each person of interest is the 
                                 writer of each questioned document."),
                        DT::DTOutput("q_post_probs_df"),
                        br(),
                        helpText("This plot shows the posterior probability that each person of interest is the 
                                 writer of each questioned document."),
                        plotOutput("q_post_probs_plot")
               )
             )
           )
         )
)
tabPanel("Analyze Questioned Documents",
         h3("Analyze Questioned Documents"),
         sidebarLayout(
           sidebarPanel(
             # questioned documents
             shinyDirButton("q_questioned_docs", "Questioned Documents", "Choose the questioned documents directory"),
             verbatimTextOutput("q_questioned_docs", placeholder = TRUE),  
             helpText("Select a directory that contains one or more questioned documents."),
             
             # analyze
             fluidRow(column(width = 6, numericInput("q_writer_start_qd", "Writer ID starting character", value=2, min=1, step=1)),
                      column(width = 6, numericInput("q_writer_end_qd", "Writer ID ending character", value=5, min=1, step=1))
             ),
             fluidRow(column(width = 6, numericInput("q_doc_start_qd", "Document name starting character", value=7, min=1, step=1)),
                      column(width = 6, numericInput("q_doc_end_qd", "Document name ending character", value=17, min=1, step=1))
             ),
             numericInput("q_num_cores_qd", "num cores", value=2, min=1, step=1),
             hr(),
             
             # model
             fileInput("q_load_model_qd", label="Load model", accept = c('rds')),
             helpText("Choose a model that was trained using handwriting samples from the person's of interest. You 
                      can create a new model by selecting create new model under the tools menu."),
             hr(),
             
             # output directory - AKA main directory
             shinyDirButton("q_main_dir_qd", "Output Directory", "Choose main directory"),
             verbatimTextOutput("dir_qd", placeholder = TRUE),
             helpText("The analysis results will be saved as analysis.rds in Output Directory > data."),
             hr(),
             
             actionButton("q_analyze_docs", "Analyze")
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Questioned Documents",
                  tableOutput("q_questioned_docs_list"),
                  selectInput("q_select_qd", "Display Questioned Document", choice = NA),
                  imageOutput("q_qd_image")
               ),
               tabPanel("Writer Profiles",
                        h4("Questioned writer profiles"),
                        helpText("Estimates of the writer profiles of the writers of the questioned documents. The 
                                 plot shows the cluster fill rates, the percentage of graphs from a questioned document that 
                                 are assigned to each cluster in the clustering template."),
                        plotOutput("q_questioned_docs_cluster_fill_rates"),
                        h4("Persons of interest writer profiles"),
                        helpText("Estimates of the writer profiles of the persons of interest. The plot shows the median
                                 of the model parameter that estimates the true cluster fill rate for a person of interest, and 
                                 the bars show the 95% credible intervals."),
                        plotOutput("q_model_profiles"),
                        ),
               tabPanel("Results",
                        h4("Posterior probabilities of writership:"),
                        DT::DTOutput("q_posterior_probabilities_table", width = "100%"),
                        br(),
                        plotOutput("q_posterior_probabilities")
                        )
             )
         )
  )
)
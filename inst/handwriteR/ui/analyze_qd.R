tabPanel("Analyze Questioned Documents",
         h3("Analyze Questioned Documents"),
         sidebarLayout(
           sidebarPanel(
             # main directory
             shinyDirButton("q_main_dir_qd", "Main Directory", "Choose main directory"),
             verbatimTextOutput("dir_qd", placeholder = TRUE),
             helpText("The cluster template, the fitted model, and other related files will be stored in
                                                     this directory."),
             
             # questioned documents
             shinyDirButton("q_questioned_docs", "Questioned Document(s)", "Choose the questioned document(s) directory"),
             verbatimTextOutput("q_questioned_docs", placeholder = TRUE),  
             helpText("Select the folder that contains the questioned document(s)."),
             
             # analyze
             fluidRow(column(width = 6, numericInput("q_writer_start_qd", "Writer ID starting character", value=2, min=1, step=1)),
                      column(width = 6, numericInput("q_writer_end_qd", "Writer ID ending character", value=5, min=1, step=1))
                      ),
             fluidRow(column(width = 6, numericInput("q_doc_start_qd", "Document name starting character", value=7, min=1, step=1)),
                      column(width = 6, numericInput("q_doc_end_qd", "Document name ending character", value=17, min=1, step=1))
                      ),
             numericInput("q_num_cores_qd", "num cores", value=2, min=1, step=1),
             actionButton("q_analyze_docs", "Analyze")
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Single Questioned Document",
                        selectInput("q_select_qd", "Select Questioned Document", choices=c(NA), selected=NULL),
                        h4("Results:"),
                        verbatimTextOutput("q_writer")
                        ),
               tabPanel("All Questioned Documents",
                        h4("Questioned Documents:"),
                        tableOutput("q_questioned_docs_list"),
                        h4("Writer profile estimates for each questioned document"),
                        plotOutput("q_questioned_docs_cluster_fill_counts"),
                        h4("Posterior probabilities of writership for each questioned document:"),
                        plotOutput("q_posterior_probabilities")
                        )
             )
           )
         )
)
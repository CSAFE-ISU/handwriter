tabPanel("Create New",
         h2("Create a New Model"),
         sidebarLayout(
           sidebarPanel(
             # main directory
             shinyDirButton("q_main_dir_model", "Main Directory", "Choose main directory"),
             verbatimTextOutput("dir_model", placeholder = TRUE),
             helpText("The cluster template, the fitted model, and other related files will be stored in
                                                     this directory."),
             hr(),
             
             # select template
             radioButtons("q_select_template", 
                          label = "Select Template", 
                          choiceNames = c("Default template", "Template in main directory"),
                          choiceValues = c("default", "main"),
                          selected = "default"),
             
             # load template
             actionButton("q_load_template", "Load Template"),
             hr(),
             
             # model
             shinyDirButton("q_model_docs", "Model Documents", "Choose model training documents' directory"),
             verbatimTextOutput("q_model_docs", placeholder = TRUE),  
             fluidRow(column(width = 4, numericInput("q_num_mcmc_iters", "# iterations", value=50, step=1, min=1)),
                      column(width = 4, numericInput("q_num_chains", "# chains", value=1, step=1, min=1)),
                      column(width = 4, numericInput("q_num_model_cores", "# cores", value=2, step=1, min=1))),
             actionButton("q_fit_model", "Fit model"),
           ),
           mainPanel(
             h3("Training Data for Model"),
             helpText("Model Training Documents:"),
             verbatimTextOutput("q_model_docs_list"),
             helpText("Cluster Fill Counts for Model Training Documents"),
             plotOutput("q_model_cluster_fill_counts"))))
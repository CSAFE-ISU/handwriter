tabPanel("Analyze Questioned Documents", 
         sidebarLayout(
           sidebarPanel(width = 4,
                        shinyDirButton("q_main_dir", "Main Directory", "Choose main template directory"),
                        verbatimTextOutput("dir", placeholder = TRUE),  
                        helpText("The cluster template, the fitted model, and other related files will be stored in
                                 this directory."),
                        hr(),
                        
                        # templates ----
                        h4("Cluster Template"),
                        bsCollapse(id = "collapseTemplates",
                                   # use template ----
                                   bsCollapsePanel("Choose Template",
                                                   radioButtons("q_use_template", "Use template", choiceValues = c("default", "main"),
                                                                choiceNames = c("Use default template", "Use template in main directory")),
                                                   helpText("The default template has K=10 clusters."),
                                                   style = "default"),
                                   # create templates ----
                                   bsCollapsePanel("Create Templates",
                                                   shinyDirButton("q_template_docs", "Template Documents", "Choose template training documents' directory"),
                                                   verbatimTextOutput("q_template_docs", placeholder = TRUE),  
                                                   fluidRow(column(width = 4, numericInput("q_K", "# clusters", value=5, min=3, max=60, step=1)),
                                                            column(width = 4, numericInput("q_max_iters", "max iterations", value=1, min=1, max=500, step=1)),
                                                            column(width = 4, radioButtons("q_num_graphs", "# graphs", selected = 1000,
                                                                                           choices = c(1000, 5000, "All")))
                                                            ),
                                                   fluidRow(column(width = 4, numericInput("q_centers_seed", "cluster centers seed", value=100, min=1, step=1)),
                                                            column(width = 4, numericInput("q_graphs_seed", "training graphs seed", value=100, min=1, step=1)),
                                                            column(width = 4, numericInput("q_num_cores", "# cores", value=5, min=1, step=1)),
                                                   ),
                                                   actionButton("q_make_templates", "Create new template(s)"), 
                                                   style = "default")),
                        hr(),
                        
                        # model ----
                        h4("Fit Hierarchical Model"),
                        bsCollapse(id="collapseModels",
                                   # fit model ----
                                   bsCollapsePanel("Fit Model",
                                                   shinyDirButton("q_model_docs", "Model Documents", "Choose model training documents' directory"),
                                                   verbatimTextOutput("q_model_docs", placeholder = TRUE),  
                                                   fluidRow(column(width = 4, numericInput("q_num_mcmc_iters", "# iterations", value=50, step=1, min=1)),
                                                            column(width = 4, numericInput("q_num_chains", "# chains", value=1, step=1, min=1)),
                                                            column(width = 4, numericInput("q_num_model_cores", "# cores", value=2, step=1, min=1))),
                                                   actionButton("q_fit_model", "Fit model"),
                                                   br(),
                                                   hr(),
                                                   h4("Model Diagnostics"),
                                                   h5("Trace Plots"),
                                                   selectInput("q_trace_variable", "Select variable to view trace plot", choices = c(NA)),
                                                   style = "default")
                        ),
                        hr(),
                        
                        # questioned docs ----
                        h4("Questioned Documents"),
                        bsCollapse(id="collapseQuestioned",
                                   # Analyze Documents ----
                                   bsCollapsePanel("Analyze Documents",
                                                   shinyDirButton("q_questioned_images_dir", "Questioned Documents", "Choose questioned documents' directory"),
                                                   verbatimTextOutput("q_questioned_images_dir", placeholder = TRUE),  
                                                   numericInput("q_questioned_num_cores", "# cores", value=5, step=1, min=1, max=10),
                                                   actionButton("q_analyze_questioned_docs", "Analyze questioned document(s)"),
                                                   style = "default")
                        ),
           ),
           mainPanel(width = 8, 
                     
                     bsCollapse(id="collapseDetails",
                                bsCollapsePanel(title="Template Details",
                                                helpText("Selected Template:"),
                                                verbatimTextOutput("q_use_template"),
                                                helpText("Template Training Documents:"),
                                                verbatimTextOutput("q_template_docs_list"),
                                                style="default"),
                                bsCollapsePanel(title="Model Details",
                                                helpText("Model Training Documents:"),
                                                verbatimTextOutput("q_model_docs_list"),
                                                helpText("Cluster Fill Counts of Model Training Documents by Writer:"),
                                                plotOutput("q_model_cluster_counts_plot"),
                                                helpText("Writer Profiles as Estimated by the Model"),
                                                style="default"),
                                open="Template Details")
           ),
         ),
)

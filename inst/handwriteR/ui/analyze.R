tabPanel("Analyze Questioned Documents", 
         sidebarLayout(
           sidebarPanel(width = 4,
                        shinyDirButton("q_main_dir", "Main Directory", "Choose main template directory"),
                        verbatimTextOutput("dir", placeholder = TRUE),  
                        helpText("All cluster template, fitted models, and other related documents will be stored in
                                 this directory."),
                        hr(),
                        
                        # templates ----
                        h4("Cluster Template"),
                        bsCollapse(id = "collapseTemplates",
                                   # use template ----
                                   bsCollapsePanel("Choose Template",
                                                   radioButtons("q_use_template", "Use template", choiceValues = c("main", "default"),
                                                                choiceNames = c("Use template in main directory", "Use default template")),
                                                   helpText("The default template has K=10 clusters."),
                                                   style = "default"),
                                   # create templates ----
                                   bsCollapsePanel("Create Templates",
                                                   fluidRow(column(width = 4, numericInput("q_seed", "seed", value=100, min=1, step=1)),
                                                            column(width = 4, numericInput("q_num_cores", "# cores", value=5, min=1, step=1)),
                                                            column(width = 4, numericInput("q_K", "# clusters", value=5, min=3, max=60, step=1))),
                                                   fluidRow(column(width = 4, numericInput("q_max_iters", "max iterations", value=1, min=1, max=500, step=1)),
                                                            column(width = 4, radioButtons("q_num_graphs", "# graphs", selected = 1000,
                                                                                           choices = c(1000, 5000, "All")))),
                                                   actionButton("q_make_templates", "Create new template(s)"), 
                                                   style = "default")),
                        hr(),
                        
                        # model ----
                        h4("Fit Hierarchical Model"),
                        bsCollapse(id="collapseModels",
                                   # fit model ----
                                   bsCollapsePanel("Fit Model",
                                                   fluidRow(column(width = 4, numericInput("q_num_mcmc_iters", "# iterations", value=50, step=1, min=1)),
                                                            column(width = 4, numericInput("q_num_chains", "# chains", value=1, step=1, min=1))),
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
                                                   numericInput("q_questioned_num_cores", "# cores", value=5, step=1, min=1, max=10),
                                                   actionButton("q_analyze_questioned_docs", "Analyze questioned document(s)"),
                                                   style = "default")
                        ),
           ),
           mainPanel(width = 8, 
                     
                     tabsetPanel(type = "tabs",
                                 tabPanel("Cluster Templates", 
                                          bsCollapse(id="collapseDisplayTemplates",
                                                     # document information ----
                                                     bsCollapsePanel("Details",
                                                                     helpText("Current template:"),
                                                                     verbatimTextOutput("q_use_template"),
                                                                     helpText("Template training documents:"),
                                                                     verbatimTextOutput("q_template_images_docnames"),
                                                                     style = "default"),
                                                     # plots ----
                                                     bsCollapsePanel("Plots",
                                                                     helpText("Cluster fill counts for template training documents:"),
                                                                     plotOutput("q_template_cluster_fill_counts"),
                                                                     helpText("Plot clusters (Placeholder):"),
                                                                     imageOutput("q_plot_clusters"),
                                                                     style = "default")
                                          ),
                                 ),
                                 tabPanel("Model",
                                          bsCollapse(id="collapseDisplayModel",
                                                     bsCollapsePanel("Details",
                                                                     helpText("Model training documents"),
                                                                     verbatimTextOutput("q_model_images_docnames"),
                                                                     style = "default"),
                                                     bsCollapsePanel("Plots",
                                                                     helpText("Cluster Fill Counts:"),
                                                                     plotOutput("q_model_cluster_counts_plot"),
                                                                     helpText("Trace Plot"),
                                                                     plotOutput("q_trace_plot"),
                                                                     style = "default"
                                                     )
                                          ),
                                 ),
                                 tabPanel("Questioned Documents",
                                          bsCollapse(id="collapseDisplayQuestioned",
                                                     bsCollapsePanel("Details",
                                                                     helpText("Questioned documents:"),
                                                                     verbatimTextOutput("q_questioned_images_docnames"),
                                                                     style = "default"),
                                                     bsCollapsePanel("Plots",
                                                                     helpText("Cluster Fill Counts:"),
                                                                     plotOutput("q_questioned_cluster_counts_plot"),
                                                                     helpText("Posterior Probabilities of Writership"),
                                                                     tableOutput("q_post_probs_table"),
                                                                     br(),
                                                                     br(),
                                                                     plotOutput("q_post_probs_plot"),
                                                                     style = "default"
                                                     )
                                          )
                                 )
                     )
           ),
         ),
)
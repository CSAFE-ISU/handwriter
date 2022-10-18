tabPanel("Analyze Questioned Documents", 
         sidebarLayout(
           sidebarPanel(width = 4,
                        textInput("q_main_dir", "Main directory", "/Users/stephanie/Documents/shiny_example"),
                        helpText("All cluster template(s), fitted models, and other related documents will be stored in
                                 this directory."),
                        hr(),
                        
                        # templates ----
                        h4("Cluster Template"),
                        bsCollapse(id = "collapseTemplates",
                                   # use default template ----
                                   bsCollapsePanel("Default Template", 
                                                   helpText("The default template has K=10 clusters."),
                                                   actionButton("q_default_templates", "Use default template"),
                                                   style = "default"),
                                   # load templates ----
                                   bsCollapsePanel("Load Templates", 
                                                   fileInput("q_load_templates", "Load template(s)", multiple = FALSE, accept = c('rds')),
                                                   selectInput("q_loaded_template_num", "Select template number", choices = c(NA)),
                                                   style = "default"),
                                   # create templates ----
                                   bsCollapsePanel("Create Templates",
                                                   fluidRow(column(width = 4, numericInput("q_num_runs", "# templates", value=1, min=1, max=20, step=1)),
                                                            column(width = 4, numericInput("q_starting_seed", "seed", value=100, min=1, step=1)),
                                                            column(width = 4, numericInput("q_num_cores", "# cores", value=5, min=1, step=1))),
                                                   fluidRow(column(width = 4, numericInput("q_K", "# clusters", value=5, min=3, max=60, step=1)),
                                                            column(width = 4, numericInput("q_max_iters", "max iterations", value=1, min=1, max=500, step=1)),
                                                            column(width = 4, radioButtons("q_num_graphs", "# graphs", selected = 1000,
                                                                                           choices = c(1000, 5000, "All")))),
                                                   actionButton("q_make_templates", "Create new template(s)"), 
                                                   br(),
                                                   br(),
                                                   selectInput("q_created_template_num", "Select template number", choices = c(NA)),
                                                   style = "default")),
                        hr(),

                        # model ----
                        h4("Fit Hierarchical Model"),
                        bsCollapse(id="collapseModels",
                                   # cluster assignments ----
                                   bsCollapsePanel("Cluster Assignments",
                                                   actionButton("q_get_model_clusters", "Get cluster assignments"),
                                                   br(),
                                                   br(),
                                                   downloadButton("q_save_model_clusters", "Save cluster assignments"),
                                                   br(),
                                                   br(),
                                                   fileInput("q_load_model_data", "Load cluster assignments", multiple = FALSE, accept = c('rds')),
                                                   style = "default"),
                                   # fit model ----
                                   bsCollapsePanel("Fit Model",
                                                   fluidRow(column(width = 4, numericInput("q_num_mcmc_iters", "# iterations", value=50, step=1, min=1)),
                                                            column(width = 4, numericInput("q_num_chains", "# chains", value=1, step=1, min=1))),
                                                   actionButton("q_fit_model", "Fit model"),
                                                   downloadButton("q_save_model", "Save model"),
                                                   br(),
                                                   br(),
                                                   fileInput("q_load_model", "Load model", multiple = FALSE, accept = c('rds')),
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
                                   # cluster assignments ----
                                   bsCollapsePanel("Cluster Assignments",
                                                   actionButton("q_get_questioned_data", "Get cluster assignments"),
                                                   br(),
                                                   br(),
                                                   downloadButton("q_save_questioned_data", "Save cluster assignments"),
                                                   br(),
                                                   br(),
                                                   fileInput("q_load_questioned_data", "Load cluster assignments", multiple = FALSE, accept = c('rds')),
                                                   style = "default"),
                                   # fit model ----
                                   bsCollapsePanel("Analyze Documents",
                                                   numericInput("q_questioned_num_cores", "# cores", value=5, step=1, min=1, max=10),
                                                   actionButton("q_analyze_questioned_docs", "Analyze questioned document(s)"),
                                                   style = "default")
                        ),
           ),
           mainPanel(width = 8, 
                     
                     tabsetPanel(type = "tabs",
                                 tabPanel("Documents", 
                                          helpText("Cluster template training documents:"),
                                          verbatimTextOutput("q_template_images_docnames"),
                                          helpText("Model training documents:"),
                                          verbatimTextOutput("q_model_images_docnames"),
                                          helpText("Questioned documents:"),
                                          verbatimTextOutput("q_questioned_images_docnames")
                                          ),
                                 tabPanel("Cluster Templates", 
                                          bsCollapse(id="collapseDisplayTemplates",
                                                     # document information ----
                                                     bsCollapsePanel("Details",
                                                                     helpText("Loaded templates:"),
                                                                     verbatimTextOutput("q_template_names"),
                                                                     helpText("Selected template:"),
                                                                     verbatimTextOutput("q_selected_template"),
                                                                     helpText("Template training documents"),
                                                                     verbatimTextOutput("q_template_docnames"),
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
                                                     bsCollapsePanel("Cluster Assignments",
                                                                     helpText("Cluster Fill Counts:"),
                                                                     DTOutput("q_cluster_fill_counts"),
                                                                     style = "default"
                                                     ),
                                            bsCollapsePanel("Hierarchical Model",
                                                     helpText("Trace Plot"),
                                                     plotOutput("q_trace_plot"),
                                                     style = "default"
                                                     )
                                            ),
                                          ),
                                 tabPanel("Questioned Documents",
                                          tabsetPanel(
                                            tabPanel("Cluster Assignments",
                                                     helpText("Cluster Fill Counts:"),
                                                     DTOutput("q_questioned_cluster_fill_counts")
                                            ),
                                            tabPanel("Analysis",
                                                     helpText("Posterior Probabilities of Writership"),
                                                     plotOutput("q_post_probs_plot"),
                                                     br(),
                                                     br(),
                                                     DTOutput("q_post_probs_table"),
                                                     
                                            )
                                          )
                              )
                     )
           ),
         ),
)
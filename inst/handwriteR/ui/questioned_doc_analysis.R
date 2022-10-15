tabPanel("Analyze Questioned Documents", 
         sidebarLayout(
           sidebarPanel(width = 4,
                        textInput("q_main_dir", "Main directory", "/Users/stephanie/Documents/shiny_example"),
                        helpText("All cluster template(s), fitted models, and other related documents will be stored in
                                 this directory."),
                        hr(),
                        
                        # make template
                        h4("Cluster Template"),
                        bsCollapse(id = "collapseTemplates",
                                   bsCollapsePanel("Load Templates", 
                                                   fileInput("q_load_templates", "Load template(s)", multiple = FALSE, accept = c('rds')),
                                                   selectInput("q_loaded_template_num", "Select template number", choices = c(NA)),
                                                   style = "default"),
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

                        # fit model
                        h4("Fit Hierarchical Model"),
                        bsCollapse(id="collapseModels",
                                   bsCollapsePanel("Cluster Assignments",
                                                   actionButton("q_get_model_clusters", "Get cluster assignments"),
                                                   br(),
                                                   br(),
                                                   downloadButton("q_save_model_clusters", "Save cluster assignments"),
                                                   br(),
                                                   br(),
                                                   fileInput("q_load_model_data", "Load cluster assignments", multiple = FALSE, accept = c('rds')),
                                                   style = "default")),
                        
                        

           ),
           mainPanel(width = 8, 
                     
                     tabsetPanel(type = "tabs",
                                 tabPanel("Overview", h4("Main directory:"),
                                          verbatimTextOutput("q_main_dir")),
                                 tabPanel("Cluster Templates", 
                                          h4("Template Training Documents:"),
                                          helpText("Directory:"),
                                          verbatimTextOutput("q_template_images_dir"),
                                          helpText("Files:"),
                                          verbatimTextOutput("q_template_images_docnames"),
                                          helpText("Processed Directory:"),
                                          verbatimTextOutput("q_template_graphs_dir"),
                                          helpText("Processed Files:"),
                                          verbatimTextOutput("q_template_graphs_docnames"),
                                          helpText("Loaded templates:"),
                                          verbatimTextOutput("q_template_names"),
                                          helpText("Selected template:"),
                                          verbatimTextOutput("q_selected_template")),
                                 tabPanel("Model",
                                          tabsetPanel(
                                            tabPanel("Model Training Documents",
                                                     verbatimTextOutput("q_model_images_docnames")
                                                     ), 
                                            tabPanel("Cluster Assignments",
                                                     helpText("Cluster Fill Counts:"),
                                                     DTOutput("q_cluster_fill_counts")
                                                     )
                                            ),
                                          )
                     )
           ),
         ),
)
tabPanel("Create New",
         h2("Create a New Template"),
         sidebarLayout(
           sidebarPanel(width = 4,
                        # main directory
                        shinyDirButton("q_main_dir", "Main Directory", "Choose main directory"),
                        verbatimTextOutput("dir", placeholder = TRUE),
                        helpText("The cluster template, the fitted model, and other related files will be stored in
                                                     this directory."),
                        
                        # template
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
                        actionButton("q_make_templates", "Create new template")
           ),
           mainPanel(
             helpText("Template Training Documents:"),
             verbatimTextOutput("q_template_docs_list"),
             helpText("Cluster Fill Counts for Template Training Documents:"),
             plotOutput("q_template_cluster_fill_counts")
             )
           )
         )
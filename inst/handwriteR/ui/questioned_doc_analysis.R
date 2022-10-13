tabPanel("Analyze Questioned Documents", 
         sidebarLayout(
           sidebarPanel(width = 4,
                        textInput("q_main_dir", "Main directory", "/Users/stephanie/Documents/shiny_example"),
                        br(),
                        
                        # make template
                        h4("Cluster Template"),
                        fluidRow(column(width = 3, numericInput("q_num_runs", "number of templates", value=1, min=1, max=20, step=1))),
                        fluidRow(column(width = 3, numericInput("q_starting_seed", "seed", value=100, min=1, step=1))),
                        fluidRow(column(width = 3, numericInput("q_K", "number of clusters", value=5, min=3, max=60, step=1))),
                        fluidRow(column(width = 3,numericInput("q_max_iters", "max iterations", value=1, min=1, max=500, step=1))),
                        fluidRow(column(width = 3,numericInput("q_num_cores", "number of cores", value=5, min=1, step=1))),
                        radioButtons("q_num_graphs", "number of graphs", selected = 1000,
                                     choices = c(1000, 5000, "All")),
                        actionButton("q_make_templates", "Make template(s)")
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
                                          verbatimTextOutput("q_template_graphs_docnames")),
                     )
           ),
         ),
)
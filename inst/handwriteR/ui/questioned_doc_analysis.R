tabPanel("Analyze Questioned Documents", 
         sidebarLayout(
           sidebarPanel(width = 4,
                        textInput("q_main_dir", "Main directory", "/Users/stephanie/Documents/shiny_example"),
                        br(),
                        
                        # Cluster template
                        textInput("q_template_images_dir", "Template images directory", "/Users/stephanie/Documents/shiny_example/data/template_images"),
                        actionButton("q_process_template_images", "Process template images"),
                        helpText("K-Means algorithm parameters"),
                        numericInput("q_num_runs", "Number of templates", value=1, min=1, max=20, step=1),
                        numericInput("q_K", "Number of clusters", value=10, min=5, max=60, step=1),
                        numericInput("q_num_cores", "Number of cores", value=2, min=1, max=10, step=1),
                        numericInput("q_max_iters", "Maximum number of iterations", value=3, min=1, max=50, step=1),
                        radioButtons("q_num_graphs", "Number of training graphs", 
                                     selected = 1000, choices = c(1000, 5000, 10000, "All") ),
                        numericInput("q_starting_seed", "Seed for random number generator", value=100, min=1, step=1),
                        actionButton("q_create_templates", "Create template(s)")
           ),
           mainPanel(width = 8, 
                     h4("Main directory:"),
                     verbatimTextOutput("q_main_dir"),
                     helpText("Directory exists:"),
                     verbatimTextOutput("q_main_dir_exists"),
                     br(),
                     
                     # Cluster template
                     h4("Template images directory:"),
                     verbatimTextOutput("q_template_images_dir"),
                     helpText("Directory exists:"),
                     verbatimTextOutput("q_template_images_dir_exists"),
                     h5("Processed template images:"),
                     verbatimTextOutput("q_template_proc_list_docnames"),
                     h5("New template(s):"),
                     textOutput("q_num_templates"),
           ),
         ),
)
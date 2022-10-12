tabPanel("Analyze Questioned Documents", 
         sidebarLayout(
           sidebarPanel(width = 4,
                        textInput("q_main_dir", "Main directory", "/Users/stephanie/Documents/shiny_example"),
                        textInput("q_template_images_dir", "Template images directory", "/Users/stephanie/Documents/shiny_example/data/template_images"),
                        actionButton("q_process_template_images", "Process template images")

           ),
           mainPanel(width = 8, 
                     h4("Main directory:"),
                     verbatimTextOutput("q_main_dir"),
                     helpText("Directory exists:"),
                     verbatimTextOutput("q_main_dir_exists"),
                     h4("Template images directory:"),
                     verbatimTextOutput("q_template_images_dir"),
                     helpText("Directory exists:"),
                     verbatimTextOutput("q_template_images_dir_exists"),
                     h4("Processed template images docnames:"),
                     verbatimTextOutput("q_template_proc_list_docnames"),
           ),
         ),
)
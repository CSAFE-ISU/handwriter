tabPanel("Analyze Questioned Documents", 
         sidebarLayout(
           sidebarPanel(width = 4,
                        textInput("q_main_dir", "Main directory", "/Users/stephanie/Documents/shiny_example"),

           ),
           mainPanel(width = 8, 
                     h4("Main directory:"),
                     verbatimTextOutput("q_main_dir"),
                     helpText("Directory exists:"),
                     verbatimTextOutput("q_main_dir_exists")
           ),
         ),
)
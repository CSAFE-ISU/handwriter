tabPanel("Diagnostics",
         sidebarLayout(
           sidebarPanel(
             # main directory
             shinyDirButton("q_main_dir_diagnostics", "Main Directory", "Choose main directory"),
             verbatimTextOutput("dir_diagnostics", placeholder = TRUE),
             helpText("The cluster template, the fitted model, and other related files will be stored in
                                                     this directory."),
             actionButton("q_load_model", "Load model"),
             hr(),
             
             h5("Trace Plots"),
             selectInput("q_trace_variable", "Select variable to view trace plot", choices = c(NA))
           ),
           mainPanel(
             plotOutput("q_trace_plot")
             )
           )
         )
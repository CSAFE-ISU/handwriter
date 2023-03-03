tabPanel("Diagnostics",
         sidebarLayout(
           sidebarPanel(
             fileInput("q_load_model", label="Load model", accept = c('rds')),
             hr(),
             
             h5("Trace Plots"),
             selectInput("q_trace_variable", "Select variable to view trace plot", choices = c(NA)),
             hr(),
             
             h5("Burn-in"),
             numericInput("q_select_burnin", "Select iterations for burn-in", value=0, min=0, step=1),
             actionButton("q_drop_burnin", "Drob burn-in"),
             downloadButton("q_save_model", "Save model"),
           ),
           mainPanel(
             plotOutput("q_trace_plot")
             )
           )
         )
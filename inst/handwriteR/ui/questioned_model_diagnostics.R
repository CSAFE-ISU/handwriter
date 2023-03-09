tabPanel("Model Diagnostics",
         h2("Model Diagnostics"),
         sidebarLayout(
           sidebarPanel(
             # load model
             fileInput("md_load_model", label="Load model", accept = c('rds')),
             
             # select model parameter
             selectInput("md_param", "Select parameter", choices=c(NA))
             
           ),
           mainPanel(
             h4("Trace Plot"),
             plotOutput("md_trace")
           )
         )
)
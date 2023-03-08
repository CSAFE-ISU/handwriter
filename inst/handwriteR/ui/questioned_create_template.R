tabPanel("Create new template",
         h2("Create a New Tempalte"),
         sidebarLayout(
           sidebarPanel(
             # template dir
             directoryUI("t_docs", label = "Template Training Documents"),
             hr(),
             
             # output dir
             directoryUI("t_output", label = "Output Directory"),
             helpText("The template and other related files with be saved in Output Directory > data."),
             hr(),
             
             # template settings
             h4("Template Settings"),
             substringIndicesUI("t_writer_indices", "writer start character", "writer stop character"),
             fluidRow(column(width=6, numericInput("t_K", "# clusters", value=10, min=1, max=100)),
                      column(width=6, numericInput("t_max_iters", "# iterations", value=25, min=1, max=500))),
             fluidRow(column(width=6, numericInput("t_cores", "# cores", value=2, min=1, max=100)),
                      column(width=6, radioButtons("t_num_graphs", "# graphs", choices=c(1000, 5000, "All")))),
             fluidRow(column(width=6, numericInput("t_centers_seed", "centers seed", value=100, min=1, step=1)),
                      column(width=6, numericInput("t_graphs_seed", "graphs seed", value=200, min=1, step=1))),
                      
             # make template
             actionButton("t_create", "Create template")

             ),
           mainPanel(
             tabsetPanel(
               tabPanel("Documents",
                        fluidRow(column(width=3, directoryContentsUI("t_docs_list")),
                                 column(width=3, substringsUI("t_writers")))
                        ),
             )
           )
         )
)
tabPanel("Create new template",
         h2("STEP 2: Create a New Tempalte"),
         sidebarLayout(
           sidebarPanel(
             # main dir
             h4("Main Directory"),
             directoryUI("t_main_dir", label = "Choose Directory"),
             helpText("The template and other related files with be saved in Directory > data."),
             hr(),
             
             # template dir
             h4("Template Training Documents"),
             directoryUI("t_docs", label = "Choose Directory"),
             hr(),
             
             # template settings
             h4("Template Settings"),
             substringIndicesUI("t_writer_indices", "writer start character", "writer stop character"),
             fluidRow(column(width=3, numericInput("t_K", "# clusters", value=8, min=1, max=100)),
                      column(width=3, numericInput("t_max_iters", "# iterations", value=3, min=1, max=500))),
             fluidRow(column(width=3, numericInput("t_centers_seed", "centers seed", value=100, min=1, step=1)),
                      column(width=3, numericInput("t_cores", "# cores", value=2, min=1, max=100))),
             hr(),

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
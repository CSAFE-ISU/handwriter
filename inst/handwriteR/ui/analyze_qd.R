tabPanel("Analyze Questioned Documents",
         sidebarLayout(
           sidebarPanel(
             # main directory
             shinyDirButton("q_main_dir_qd", "Main Directory", "Choose main directory"),
             verbatimTextOutput("dir_qd", placeholder = TRUE),
             helpText("The cluster template, the fitted model, and other related files will be stored in
                                                     this directory."),
             actionButton("q_load_model_qd", "Load model"),
             hr(),
             
             # questioned documents
             shinyDirButton("q_questioned_docs", "Questioned Document(s)", "Choose the questioned document(s) directory"),
             verbatimTextOutput("q_questioned_docs", placeholder = TRUE),  
             helpText("Select the folder that contains the questioned document(s)."),
             
           ),
           mainPanel(
             helpText("Questioned Document(s):"),
             verbatimTextOutput("q_questioned_docs_list"),
           )
         )
  
)
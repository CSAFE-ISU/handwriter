tabPanel("Analyze Questioned Documents",
         navbarPage("Analyze",
                    # Template ----
                    tabPanel("Cluster Template",
                             sidebarLayout(
                               sidebarPanel(width = 4,
                                            # main directory
                                            shinyDirButton("q_main_dir", "Main Directory", "Choose main template directory"),
                                            verbatimTextOutput("dir", placeholder = TRUE),
                                            ),
                               mainPanel(
                                 
                               )
                             ))))
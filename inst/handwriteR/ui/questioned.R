directoryUI <- function(id, label) {
  tagList(
    shinyDirButton(NS(id, "choose_dir"), label, "Choose directory"),
    verbatimTextOutput(NS(id, "dir_path"), placeholder = TRUE),
  )
}


tabPanel("Questioned Documents",
         navbarPage("",
                    navbarMenu("Analyze",
                               source(file.path("ui", "questioned_create_template.R"), local = TRUE)$value,
                               "----",
                               "Persons of Interest",
                               "----",
                               "Questioned Documents"
                    ),
         )
)

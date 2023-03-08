# choose a directory and render its file path
directoryUI <- function(id, label) {
  tagList(
    shinyDirButton(NS(id, "choose_dir"), label, "Choose directory"),
    verbatimTextOutput(NS(id, "dir_path"), placeholder = TRUE),
  )
}

# list files in directory in a table
directoryContentsUI <- function(id) {
  tagList(
    tableOutput(NS(id, "dir_contents")),
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

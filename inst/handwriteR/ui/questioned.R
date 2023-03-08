# MODULE: choose a directory and render its file path
directoryUI <- function(id, label) {
  tagList(
    shinyDirButton(NS(id, "choose_dir"), label, "Choose directory"),
    verbatimTextOutput(NS(id, "dir_path"), placeholder = TRUE),
  )
}

# MODULE:  list files in directory in a table
directoryContentsUI <- function(id) {
  tagList(
    tableOutput(NS(id, "dir_contents")),
  )
}

# MODULE: start and stop substring indices
substringIndicesUI <- function(id, start_label, stop_label){
  tagList(
    fluidRow(column(width=3, numericInput(NS(id, "start"), start_label, value=1, min=1, step=1),),
             column(width=3, numericInput(NS(id, "stop"), stop_label, value=1, min=1, step=1)))
  )
}

# MODULE:  display unique substrings in table
substringsUI <- function(id) {
  tagList(
    tableOutput(NS(id, "substrings"))
  )
}

tabPanel("Questioned Documents",
         navbarPage("",
                    navbarMenu("Analyze",
                               "Template",
                               source(file.path("ui", "questioned_create_template.R"), local = TRUE)$value,
                               "----",
                               "Persons of Interest",
                               "----",
                               "Questioned Documents"
                    ),
         )
)

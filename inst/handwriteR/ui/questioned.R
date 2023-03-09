# Modules -----------------------------------------------------------------
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

# MODULE: display image
displayImageUI <- function(id, label) {
  tagList(
    selectInput(NS(id, "select_image"), label, choices = NA),
    imageOutput(NS(id, "image"))
  )
}

# MODULE: start and stop substring indices
substringIndicesUI <- function(id, start_label, stop_label){
  tagList(
    fluidRow(column(width=3, numericInput(NS(id, "start"), start_label, value=1, min=1, step=1),),
             column(width=3, numericInput(NS(id, "stop"), stop_label, value=5, min=1, step=1)))
  )
}

# MODULE:  display unique substrings in table
substringsUI <- function(id) {
  tagList(
    tableOutput(NS(id, "substrings"))
  )
}

# MODULE: load file
loadUI <- function(id, label) {
  tagList(fileInput(NS(id, "file"), 
                     label,
                     multiple = FALSE,
                     accept = c(".rds")))
}


# Tab Contents ------------------------------------------------------------
tabPanel("Questioned Documents",
         navbarPage("",
                    navbarMenu("Analyze",
                               "STEP 1: Set-Up",
                               source(file.path("ui", "questioned_setup.R"), local = TRUE)$value,
                               "----",
                               "STEP 2: Template",
                               source(file.path("ui", "questioned_create_template.R"), local = TRUE)$value,
                               "----",
                               "STEP 3: Statistical Model",
                               source(file.path("ui", "questioned_create_model.R"), local = TRUE)$value,
                               source(file.path("ui", "questioned_model_diagnostics.R"), local = TRUE)$value,
                               "----",
                               "STEP 4: Questioned Documents",
                               source(file.path("ui", "questioned_analyze.R"), local = TRUE)$value,
                    ),
         )
)

# Choose a directory and render its file path
directoryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    values <- reactiveValues()
    shinyDirChoose(
      input,
      'choose_dir',
      roots = c(home = '~'),
      filetypes = c('png')
    )
    choose_dir <- reactive(input$choose_dir)
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$choose_dir
                 },
                 handlerExpr = {
                   if (!"path" %in% names(choose_dir())) return()
                   home <- normalizePath("~")
                   values$dir_path <-
                     file.path(home, paste(unlist(choose_dir()$path[-1]), collapse = .Platform$file.sep))
                 })
    output$dir_path <- renderText({
      values$dir_path
    })
    # return dir_path
    reactive(values$dir_path)
  })
}

# List the files in directory. NOTE: dir_path is reactive and output of directoryServer
directoryContentsServer <- function(id, dir_path) {
  moduleServer(id, function(input, output, session) {
    output$dir_contents <- renderTable({
      data.frame("filenames"=list.files(dir_path()))
    })
  })
}

# Pages ----
source(file.path("server", "questioned_create_template.R"), local = TRUE)$value
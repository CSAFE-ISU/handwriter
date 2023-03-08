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
  })
}

# Pages ----
source(file.path("server", "questioned_create_template.R"), local = TRUE)$value
# Modules -----------------------------------------------------------------
# MODULE:  Choose a directory and render its file path
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

# MODULE:  List the files in directory. NOTE: dir_path is reactive and output of directoryServer
directoryContentsServer <- function(id, dir_path) {
  moduleServer(id, function(input, output, session) {
    output$dir_contents <- renderTable({
      if ( !is.null(dir_path()) ){
        data.frame("filenames"=list.files(dir_path()))
      }
    })
  })
}

displayImageServer <- function(id, dir_path) {
  moduleServer(id, function(input, output, session) {
    observe({
      if ( !is.null(dir_path()) ){
        updateSelectInput(session, "select_image", choices = list.files(dir_path()))
      }
    })
    
    output$image <- renderImage({
      if ( !is.null(dir_path()) ){
        filename <- normalizePath(file.path(dir_path(), input$select_image))
        
        # Return a list containing the filename and alt text
        list(src = filename)
      } else{
        list(src = "")
      }
    }, deleteFile = FALSE)
  })
}

# MODULE: start and stop substring indices
substringIndicesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    start <- reactive(input$start)
    stop <- reactive(input$stop)
    list("start"=start, "stop"=stop)
  })
}

# MODULE: display unique substrings in data frame
substringsServer <- function(id, dir_path, indices, df_label="writers"){
  moduleServer(id, function(input, output, session) {
    output$substrings <- renderTable({
      if ( !is.null(dir_path()) ){
        docs <- list.files(dir_path())
        substrings <- unique(substr(docs, indices$start(), indices$stop()))
        df <- data.frame("temp" = substrings)
        colnames(df) <- df_label
        df
      }
    })
  })
}

# MODULE: load file
loadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(readRDS(input$file$datapath))
  })
}


# Tab Contents ------------------------------------------------------------
source(file.path("server", "questioned_create_template.R"), local = TRUE)$value
source(file.path("server", "questioned_create_model.R"), local = TRUE)$value
source(file.path("server", "questioned_model_diagnostics.R"), local = TRUE)$value
source(file.path("server", "questioned_analyze.R"), local = TRUE)$value
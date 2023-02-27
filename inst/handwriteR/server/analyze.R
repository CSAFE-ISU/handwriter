analysis <- reactiveValues(q_main_datapath = "")

#======================= MAIN DIRECTORY =============================
# UPDATE: main directory ----
shinyDirChoose(
  input,
  'q_main_dir',
  roots = c(home = '~'),
  filetypes = c('', 'png', 'txt', 'bigWig', "tsv", "csv", "bw")
)
q_main_dir <- reactive(input$q_main_dir)
observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$q_main_dir
             },
             handlerExpr = {
               if (!"path" %in% names(q_main_dir())) return()
               home <- normalizePath("~")
               analysis$q_main_datapath <-
                 file.path(home, paste(unlist(q_main_dir()$path[-1]), collapse = .Platform$file.sep))
             })

# RENDER: main directory ----
output$dir <- renderText({
  analysis$q_main_datapath
})
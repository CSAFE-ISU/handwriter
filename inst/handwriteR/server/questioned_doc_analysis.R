#==================================================================
#======================= QUESTIONED DOC ANALYSIS ==================
#==================================================================

analysis <- reactiveValues(q_main_dir = "/Users/stephanie/Documents/shiny_example")

# UPDATE: main directory
observeEvent("q_main_dir", {
  analysis$q_main_dir <- input$q_main_dir
})

# RENDER: main directory
output$q_main_dir <- renderText({ analysis$q_main_dir <- input$q_main_dir })
output$q_main_dir_exists <- renderPrint({ dir.exists(analysis$q_main_dir <- input$q_main_dir)})
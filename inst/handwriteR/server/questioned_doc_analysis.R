#==================================================================
#======================= QUESTIONED DOC ANALYSIS ==================
#==================================================================

analysis <- reactiveValues(q_main_dir = "/Users/stephanie/Documents/shiny_example",
                           q_template_images_dir = "/Users/stephanie/Documents/shiny_example/data/template_images")

# UPDATE:
observe({
  analysis$q_main_dir <- input$q_main_dir
  analysis$q_template_images_dir <- input$q_template_images_dir
})

# RENDER:
output$q_main_dir <- renderText({ analysis$q_main_dir })
output$q_main_dir_exists <- renderPrint({ dir.exists(analysis$q_main_dir) })

output$q_template_images_dir <- renderText({ analysis$q_template_images_dir })
output$q_template_images_dir_exists <- renderPrint({ dir.exists(analysis$q_template_images_dir) })
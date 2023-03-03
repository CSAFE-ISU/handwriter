tabPanel("Analyze Questioned Document",
         navbarPage("Analyze",
                    source(file.path("ui", "analyze_qd.R"), local = TRUE)$value,
                    navbarMenu("Tools",
                               "Template",
                               source(file.path("ui", "analyze_create_template.R"), local = TRUE)$value,
                               "----",
                               "Model",
                               source(file.path("ui", "analyze_create_model.R"), local = TRUE)$value,
                               source(file.path("ui", "analyze_model_diagnostics.R"), local = TRUE)$value
                    ),
         )
)

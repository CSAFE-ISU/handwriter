analysis <- reactiveValues(msg_duration = 20,
                           q_main_datapath = "",
                           q_template_docs = NULL,
                           q_template = NULL,
                           q_model_docs = NULL,
                           q_model = NULL)

# Pages ----
source(file.path("server", "analyze_create_template.R"), local = TRUE)$value
source(file.path("server", "analyze_create_model.R"), local = TRUE)$value
source(file.path("server", "analyze_model_diagnostics.R"), local = TRUE)$value
source(file.path("server", "analyze_qd.R"), local = TRUE)$value

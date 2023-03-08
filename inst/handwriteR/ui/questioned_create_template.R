tabPanel("Template",
         h2("Create a New Tempalte"),
         directoryUI("t_docs", label = "Template Training Documents"),
         directoryContentsUI("t_docs_list")
         )
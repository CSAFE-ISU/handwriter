tabPanel("Template",
         h2("Create a New Tempalte"),
         directoryUI("t_docs", label = "Template Training Documents"),
         directoryContentsUI("t_docs_list"),
         hr(),
         directoryUI("t_output", label = "Output Directory"),
         helpText("The template and other related files with be saved in Output Directory > data."),
         hr(),
         )
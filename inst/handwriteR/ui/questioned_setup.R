tabPanel("Set-up",
         h2("STEP 1: Set-up"),
         h3("Handwriting Samples"),
         p("handwriter uses three sets of handwriting samples:"),
         tags$ol(
           tags$li("Examples of handwriting in the same language as the questioned document. These examples may be 
                   from a publicly available database such as the", tags$a(href="https://forensicstats.org/handwritingdatabase/", "CSAFE Handwriting Database"), 
                   "or from your agency. These examples must not include writing samples from any of the persons of interest."), 
           tags$li("Handwriting samples collected from the persons of interest."), 
           tags$li("Questioned documents.")
         ),
         
         h3("Organize the Handwriting Samples"),
         tags$ol(
           tags$li("Create a", tags$b("main directory"), "to store the handwriting samples and analysis related files."),
           tags$li("Create a folder inside the main directory called", tags$b("data.")),
           tags$li("Create a folder inside the 'data' folder called", tags$b("template_docs."), "Copy and paste the first set of handwriting samples into this folder."),
           tags$li("Create a second folder inside the 'data' folder called", tags$b("model_docs."), "Copy and paste the persons of interest handwriting samples into this folder folder."),
           tags$li("Create a third folder inside the 'data' folder called", tags$b("questioned_docs."), "Copy and paste the questioned documents into this folder folder."),
         )
)
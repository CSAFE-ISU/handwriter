#INTRO TAB

tabPanel("Intro",
         h1("Welcome to handwriter!"),
         h3("handwriter is a handwritten document analysis tool created by CSAFE"),
         br(), br(),
         h3("Whats avaialble:"),
         tags$div(tags$ul(
           tags$li("Explore how handwriter works | Plot a document as well as its lines, words, or graphs"),
           tags$li("Pre-process documents | Use rotation, resizing, cropping, and masking to clean up your data"),
           tags$li("Analyze questioned documents | Use a Bayesian hierarchical model to estimate posterior probabilities of writership for questioned documents.")
         ), style = "font-size: 15px"), 
         hr(),
         br(),br(),
         HTML('<p> Learn more about handwriter on our <a href = "https://csafe-isu.github.io/handwriter/index.html"> website </a> or at our <a href = "https://github.com/CSAFE-ISU/handwriter"> Github </a></p>')
)
#INTRO TAB

tabPanel("Intro",
         h1("Welcome to handwriter"),
         p("handwriter is an open-source R package that compares a questioned document with writing samples from persons of interest."),
         br(), 
         br(),
         h3("What's available:"),
         tags$div(tags$ul(
           tags$li("Explore how handwriter works | See how handwriter decomposes handwriting into component shapes called graphs"),
           tags$li("Pre-process documents | Rotate, crop, and mask to clean up your data for use with handwriter"),
           tags$li("Analyze handwriting | Perform writership analysis on questioned documnets")
         ), style = "font-size: 15px"), 
         hr(),
         br(),br(),
         HTML('<p> Learn more about handwriter on our <a href = "https://csafe-isu.github.io/handwriter/index.html"> website </a> or at our <a href = "https://github.com/CSAFE-ISU/handwriter"> Github </a></p>')
)
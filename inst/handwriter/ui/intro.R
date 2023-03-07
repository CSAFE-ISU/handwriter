#INTRO TAB

tabPanel("Intro",
         h1("Welcome to handwriter!"),
         p("handwriter is an open-source computational tool for analyzing handwriting. It imports scanned handwritten documents and 
           breaks the writing into shapes, such as straight lines or loops. Different writers produce certain shapes more frequently than others.
           handwriter uses the frequency of shapes found in a handwriting sample as a writer profile. The program uses a statistical model to compare the 
           writer profile from a questioned document to the writer profiles from known handwriting samples, resulting in a posterior probability that a known 
           writer wrote the questioned document."),
         br(), br(),
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
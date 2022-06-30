#INTRO TAB

tabPanel("Intro",
         h1("Welcome to handwriter!"),
         h3("handwriter is a handwritten document analysis tool created by CSAFE"),
         br(), br(),
         h3("Whats avaialble:"),
         tags$div(tags$ul(
           tags$li("Preproces | Use rotation, resizing, cropping, and masking to clean up your data"),
           tags$li("Plot | Plot your document as well as its lines, words, or graphs"),
           tags$li("Explore Features | Extract and explore valuable features from your documents")
         ), style = "font-size: 15px"), hr(),
         h4("COMING SOON: Two statistical analysis tools to determine probability of writership"),
         tags$div(tags$ul(
           tags$li("k-means Clustering | Perform k-means clustering on a set of documents"),
           tags$li("Triangle Decomposition | Do Kniser Triangle Decomposition & compare on your documents"),
         ),  style = "font-size: 15px"),
         br(),br(),
         HTML('<p> Learn more about handwriter on our <a href = "https://csafe-isu.github.io/handwriter/index.html"> website </a> or at our <a href = "https://github.com/CSAFE-ISU/handwriter"> Github </a></p>')
)
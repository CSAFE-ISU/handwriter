library(handwriter)
library(shiny)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  sidebarLayout(
    sidebarPanel(
      disabled(actionButton("MakePlot", "Make Plot")),
      br(),
      actionButton("EnableButton", "Enable Button"
      ),
      actionButton("DisableButton", "Disable Button"
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$MakePlot, {
    output$distPlot <- renderPlot({
      x    <- faithful[, 2] 
      hist(x, breaks = 30, col = 'darkgray', border = 'white')
    })
  })
  
  observeEvent(input$EnableButton, {
    enable("MakePlot")
  })
  
  observeEvent(input$DisableButton, {
    disable("MakePlot")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
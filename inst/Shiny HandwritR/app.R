#Shiny HandwritR
# devtools::install_github("CSAFE-ISU/handwriter")

library(handwriter)
library(magick)
library(shiny)
library(shinyjs)
library(shinybusy)
library(shinyBS)
library(shinyFiles)
print(paste0('working in: ', getwd()))

source('shinyUI.R', local = TRUE)
source('shinyServer.R')

#runGadget(ui, server, viewer = dialogViewer("Shiny HandwritR", width = 1800, height = 1100))
shinyApp(ui, server)


#Shiny HandwritR
# devtools::install_github("CSAFE-ISU/handwriter")
# Rcpp::sourceCpp(file = "~/src/ThinImageCpp.cpp")
#install.packages("shinybusy")
#library(handwriter)
library(magick)
library(shiny)
library(shinyjs)
#library(shinybusy)
print(paste0('working in: ', getwd()))

source('shinyUI.R', local = TRUE)
source('shinyServer.R')

runGadget(ui, server, viewer = dialogViewer("Shiny HandwritR", width = 1800, height = 900))
#shinyApp(ui, server)


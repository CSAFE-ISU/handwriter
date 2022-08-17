#Shiny handwriter
# devtools::install_github("CSAFE-ISU/handwriter")

#devtools::load_all(".")
library(magick)
library(shiny)
library(shinyjs)
library(shinybusy)
library(shinyBS)
library(shinyFiles)
library(DT)
library(stringr)
library(dplyr)

print(paste0('working in: ', getwd()))

source('shinyUI.R', local = TRUE)
source('shinyServer.R')

shinyApp(ui, server)

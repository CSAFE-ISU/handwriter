#Install the devtools package
#install.packages("devtools")

#Install handwriter package directly from Github
#devtools::install_github("CSAFE-ISU/handwriter")

#Load libraries
library(handwriter)
library(reshape2)
library(igraph)
library(ggplot2)

#Create empty list, and load an image with READPNGBinary
csafe = list()
#CSAFE IMAGE
csafe$image = readPNGBinary("examples/Writing_csafe_single.png") #one word 'csafe'
#csafe$image = readPNGBinary("examples/0003_4.png") #full paragraph

#Use ggplot to plot a binary image
plotImage(csafe$image)

csafe$image = crop(csafe$image) #Single pixel padding around outermost black pixels.
plotImage(csafe$image)

csafe$thin = thinImage(csafe$image)
plotImageThinned(csafe$image, csafe$thin)

# Get paths and graphemes, as well as breakpoints and extra nodes
csafe_processList = processHandwriting(csafe$thin, dim(csafe$image))
csafe$nodes = csafe_processList$nodes
csafe$breaks = csafe_processList$breakPoints
csafe$paths = csafe_processList$pathList
csafe$graphemes = csafe_processList$graphemeList

#plotNodes(csafe$image, csafe$thin, csafe$nodes)
#plotNodes(csafe$image, csafe$thin, csafe$breaks)

###Some stuff for plotting letter:###
dims = dim(csafe$image)
plotLetter(csafe_processList$letterList, 3, dims)


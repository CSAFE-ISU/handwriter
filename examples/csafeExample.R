#Install the devtools package anad the then the handwriter package directly from Github
#install.packages("devtools")
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
#csafe$image = readPNGBinary("examples/0004_4.png") #full paragraph

#Use ggplot to plot a binary image
plotImage(csafe$image)

#Crop single pixel padding around outermost black pixels. -- alreay cropped in readPNGBinary
#csafe$image = crop(csafe$image)
#plotImage(csafe$image)

#Use the Zhang - Suen algorithim to thin the image (1 pixel wide) - then plot it.
csafe$thin = thinImage(csafe$image)
plotImageThinned(csafe$image, csafe$thin)

#Huge step in handwriting processing. Takes in thin image form and the breakpoints suggested by getNodes
#and parses the writing into letters. Returns final letter separation points, a list of the paths in the image,
#and a list of the letter paths in the image.
csafe_processList = processHandwriting(csafe$thin, dim(csafe$image))

#Save off nodes, breaks, paths, and graphemes
csafe$nodes = csafe_processList$nodes
csafe$breaks = csafe_processList$breakPoints
#csafe$paths = csafe_processList$pathList
#csafe$graphemes = csafe_processList$graphemeList

plotNodes(csafe$image, csafe$thin, csafe$nodes)
#plotNodes(csafe$image, csafe$thin, csafe$breaks)

###Some stuff for plotting letter:###
dims = dim(csafe$image)
plotLetter(csafe_processList$letterList, 37, dims)

plotLine(csafe_processList$letterList, 1, dims)
    

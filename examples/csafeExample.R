# #Install the devtools package anad the then the handwriter package directly from Github
#install.packages("devtools")
#devtools::install_github("CSAFE-ISU/handwriter")
#install.packages("handwriter")
# 
#Load libraries
library(handwriter)
library(reshape2)
library(igraph)
library(ggplot2)
library(stringr)
library(randomForest)
# 
#Create empty list, and load an image with READPNGBinary
csafe = list()
#CSAFE IMAGE
csafe$image = readPNGBinary("examples/0006_4.png") #full paragraph

#Use ggplot to plot a binary image
plotImage(csafe$image)

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

plotNodes(csafe$image, csafe$thin, csafe$nodes)
#plotNodes(csafe$image, csafe$thin, csafe$breaks)

###Some stuff for plotting letters, words, and lines:###
dims = dim(csafe$image)
plotLetter(csafe_processList$letterList, 1, dims)
plotLine(csafe_processList$letterList, 1, dims)
# 
# ##EVERYTHING UNDER HERE IS NEW WORD STUFF##
# 
# #plotLine(csafe_processList$letterList, 1, dims)
# 
# wordIndexList = list()
# for(i in csafe_processList$letterList){
#   wordIndexList <- append(wordIndexList, i$characterFeatures$wordIndex)
# }
# print(unlist(wordIndexList))
# 
# #Create list of word objects, process the words for more information, plot the word with colored Nodes
# words = create_words(csafe_processList) 
# words_after_processing = process_words(words, dim(csafe$image), TRUE)
# plotColorNodes(csafe_processList$letterList, 6, dims, words_after_processing)
# 
# plotWord(csafe_processList$letterList, 7, dims)

#IMPORTANT/FASTEST FLOW (only plotting colornodes on words)
# csafe = list()
# csafe$image = readPNGBinary("examples/0006_4.png") #full paragraph
# csafe$thin = thinImage(csafe$image)
# csafe_processList = processHandwriting(csafe$thin, dim(csafe$image))
# dims = dim(csafe$image)
# 
# words = create_words(csafe_processList)
# words_after_processing = process_words(words, dim(csafe$image), TRUE)
# plotColorNodes(csafe_processList$letterList, 6, dims, words_after_processing)
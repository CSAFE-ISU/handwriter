#Install the devtools package and the then the handwriter package directly from Github
#install.packages("devtools")
#devtools::install_github("CSAFE-ISU/handwriter")
#library(handwriter)

#install.packages("handwriter")
library(handwriter)

#Create empty list, and load an image with READPNGBinary
csafe_document = list()
csafe_document$image = readPNGBinary("examples/external_data/handwriter.png")
plotImage(csafe_document$image)

#Use the Zhang - Suen algorithim to thin the image (1 pixel wide) - then plot it.
csafe_document$thin = thinImage(csafe_document$image)
plotImageThinned(csafe_document$image, csafe_document$thin)

#Huge step in handwriting processing. Takes in thin image form and the breakpoints suggested by getNodes
#and parses the writing into letters. Returns final letter separation points, a list of the paths in the image,
#and a list of the letter paths in the image.
csafe_processList = processHandwriting(csafe_document$thin, dim(csafe_document$image))


#Save off nodes, breaks, paths, and graphemes
csafe_document$nodes = csafe_processList$nodes
csafe_document$breaks = csafe_processList$breakPoints

plotNodes(csafe_document$image, csafe_document$thin, csafe_document$nodes)
#plotNodes(csafe_document$image, csafe_document$thin, csafe_document$breaks)

###Some stuff for plotting letters, words, and lines:###
plotLetter(csafe_processList$letterList, 1, dim(csafe_document$image), showNodes = FALSE)
plotLine(csafe_processList$letterList, 1, dim(csafe_document$image))
#ggsave("imagetosave.png")

######### WORD STUFF #######
 
#Create list of word objects, process the words for more information, plot the word with colored Nodes
csafe_processList$letterList = make_single_word(csafe_processList$letterList)
words = create_words(csafe_processList)
words_after_processing = process_words(words, dim(csafe_document$image), TRUE)
plotWord(csafe_processList$letterList, 1, dim(csafe_document$image))

#Plot a word with colored nodes (must have processed words)
plotColorNodes(csafe_processList$letterList, 1, dims, words_after_processing)

#=======================================================================================================================
#=======================================================================================================================

####### QUICK EXAMPLE FOR WORD EXAMPLES #######
csafe_document = list()
csafe_document$image = readPNGBinary("inst/extdata/word_splitting_problems/w0003_s01_pPHR_r03.png")
csafe_document$thin = thinImage(csafe_document$image)
csafe_processList = processHandwriting(csafe_document$thin, dim(csafe_document$image))

csafe_document$nodes = csafe_processList$nodes
csafe_document$breaks = csafe_processList$breakPoints

dims = dim(csafe_document$image)
words = create_words(csafe_processList) 
words_after_processing = process_words(words, dim(csafe_document$image), TRUE)
plotWord(csafe_processList$letterList, 6, dims)


#=======================================================================================================================
#=======================================================================================================================

######   BATCH PROCESSING AND TEMPLATE CREATION EXAMPLE #######
process_batch('examples/50CSAFE_50CVL_2022-06-17/data/template_images_5_test', 'examples/50CSAFE_50CVL_2022-06-17/data/template_graphs_5_test')
make_clustering_templates('examples/50CSAFE_50CVL_2022-06-17', num_graphs = 100)


#=======================================================================================================================
#=======================================================================================================================


### Test space for loading data files (used in examples in documentation) ###
# london_document = list()
# #load("data/nature1.rda")
# london_document$image = nature1
# plotImage(london_document$image)
# london_document$thin = thinImage(london_document$image)
# plotImageThinned(london_document$image, london_document$thin)
# london_processList = processHandwriting(london_document$thin, dim(london_document$image))
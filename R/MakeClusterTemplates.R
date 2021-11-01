#### Create handwriting template
### Combine a diverse set of documents into 1 set of templates that will be used to give clusters to documents run through handwriter.

makeClusterTemplates = function(documentDirectory,
                                dataDirectory,
                                resultsFile,
                                K = 40, 
                                numberToRun = 1, 
                                numCores = 1, 
                                numDistCores = 1, 
                                numPathCuts = 8, 
                                iter.max = 500, 
                                gamma = 3,
                                meanGraph = 'slow_change',
                                meanGraphOrder = 'sequential',
                                fillClusters = 'farthest'){

  # Set seed for pseudo-random number generator
  seed = 2
  set.seed(seed)
  
  if (dataDirectory == "") dataDirectory = documentDirectory
  # ---- Load Data
  # Load the RDS file.
  # proclist is an unnamed list of 100 elements, with 1 element for each handwriting sample. Access element i with procList[[i]].
  # Each element is a named list with 4 fields: process; docname; thin; and image.
  # procList[[i]]$process is a named list with 3 fields: nodes; breakpoints; and letterList.
  # procList[[i]]$process$letterList is an unnamed list with an element for each graph in the document (need to verify)
  # procList[[i]]$process$letterList[[j]] is a named list with 6 fields: path; nodes; allPaths; adjMatrix; letterCode; and characterFeatures.
  procList = readRDS(file.path(dataDirectory, "rdata", paste0(basename(dataDirectory), ".rds")))
  
  # ---- Get graph Specific Locations
  # Get the binarized matrix (image plot) of each graph from each handwriting sample. 
  # The matrix plot can be accessed with procList[[i]]$process$letterList[[j]]$image.
  procList = lapply(procList, function(x){x$process$letterList = handwriter::AddLetterImages(x$process$letterList, dim(x$image)); return(x)})
  
  procList = lapply(procList, function(x){x$process$letterList = handwriter::AddSamplingStrata(x$process$letterList, dim(x$image)); return(x)})
  
  # For each graph in each handwriting sample, get the locations of the nodes,
  # paths, centroids, and other items with respect to the individual graph
  # instead of the handwriting sample. WARNING: This messes up plotting.
  procList = lapply(procList, function(x){x$process$letterList = MakeLetterListLetterSpecific(x$process$letterList, dim(x$image)); return(x)})
  
  # For each graph, find the locations (column and row numbers) of the centroid and proptroid relative to the 
  # bottom left corner of the graph image. 
  imagesList = NULL
  for (i in 1:length(procList)) {
    imagesList = c(imagesList, lapply(procList[[i]]$process$letterList, function(x){centeredImage(x)}))
  }
  
  # For each graph, find the locations (column and row numbers) of the path and path ends
  # with the centroid placed at the origin. 
  imagesList = lapply(imagesList, function(x) centeredImageOnCentroid(x))
  
  # ---- Make starting cluster centers
  centerstarts = MakeCenterStarts(procList, K, numPathCuts = 8, imagesList)
  
  # ---- Kmeans clustering
  handwriterTemplates = list()
  
  handwriterTemplates = runLetterKmeans(imList = imagesList, 
                                        centers = centerstarts, 
                                        numberToRun = 3, 
                                        numCores = 1, 
                                        numDistCores = 1,  
                                        K=40, 
                                        numPathCuts = 8, 
                                        iter.max = 100,
                                        numOutliers = 0,
                                        gamma = 3,
                                        results_file = paste0(basename(documentDirectory), seed),
                                        meanGraph = 'slow_change',
                                        meanGraphOrder = 'sequential',
                                        fillClusters = 'farthest')
}

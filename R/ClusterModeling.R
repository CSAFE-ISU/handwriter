#HANDLING FUNCTIONS

#Load proclist, These docs have already gone through the processing in get_clusterassignment(), so need to do more processing

#' get_cluster_assignments
#' 
#' Description Here
#'
#' @param proclist_path 
#' @param cluster_template 
#' @return nothing
#'
#' @export
# get_cluster_assignments = function(proclist_path, cluster_template){
#   # Make proclist from scratch
#   proclist = readRDS(file = proclist_path)
#   # Remove one layer of nesting
#   proclist = lapply(proclist, function(x) x[[1]])
#   
#   k40_proclist = get_clusterassignment(cluster_template, proclist)  #Started 11pm on Aug 20th
#   saveRDS(object = k40_proclist, file = "examples/k40_proclist.rds")
# }



library(dplyr)
library(rjags)
library(mc2d)
library(foreach)
library(doParallel) 
library(circular)

##FROM MODEL 2 CODE


## Datasets

# Cluster template: `50CSAFE_25CVL_25IAM_2022-06-17` seed 302 run 3
# Model training: 90 CSAFE batch 1 writers with three session 1 London Letter prompts  from each writer
# Questioned documents: One Wizard of Oz prompt repetition 2 from session 1 from each of the 90 writers. If a writer didn't have repetition 2, Wizard of Oz session 1 repetition 1 was used instead.

## Proclist
## SR load Amy's proclist. These docs have already gone through the processing in get_clusterassignment(), so I need to find the docs before this processing.

# Make proclist from scratch
proclist = readRDS(file = "data/session1_proclist_withcluster_jan8.rds")
# Remove one layer of nesting
proclist = lapply(proclist, function(x) x[[1]])
# get list of docs
docs = sapply(proclist, function(x) x$docname)
df = data.frame(docs=docs)
df = df %>% separate(docs, into=c("writer"), extra="drop", remove=FALSE)
# format file paths for graphs that haven't gone through the processing yet
df$graph_paths = file.path("/lss/research/csafe-handwriting/Data_Processing/Stage5_Graphs", df$writer, paste0(df$docs, "_proclist.rds"))
# remove * from file name
df$graph_paths[123] = "/lss/research/csafe-handwriting/Data_Processing/Stage5_Graphs/w0017/w0017_s01_pWOZ_r01_proclist.rds"
# load files
proclist = lapply(df$graph_paths, function(x) readRDS(x))
# save
saveRDS(proclist, "data/proclist.rds")


## Cluster Assignments

# # load cluster template
# clustertemplate = readRDS("data/template_seed302_run3.rds")  
# # get cluster assignments
# k40_proclist = get_clusterassignment(clustertemplate, proclist)  # takes a very long time # save
# saveRDS(object = k40_proclist, file = "data/k40_proclist.rds")  


## Fit the Full Model and Cacluate Posterior Probabilities

# # Load cluster fill counts
# k40_proclist = readRDS("data/k40_proclist.rds")
# 
# # Load old training data to get cluster order
# oldtrain = readRDS("data/session1_modeldata_jan29.rds")$train
# 
# # Select the desired prompts to train and test the model
# testdoc = "s01_pWOZ_r02"
# traindocs = c("s01_pLND_r01", "s01_pLND_r02", "s01_pLND_r03")
# backuptest = "s01_pWOZ_r01"
# k40_data = proclistToData_csafe(proclist = k40_proclist, testdoc = testdoc, traindocs = traindocs, backuptest = backuptest, oldtrain = oldtrain)
# 
# # Save the model documents
# saveRDS(object = k40_data, file = "data/k40_data.rds")



# # Load model data
# k40_data = readRDS("data/k40_data.rds")
# 
# # Summarize data
# k40_data = readRDS("data/k40_data.rds")  # SR
# k40_train_writers = length(unique(k40_data$train$bucketData$writer))  # SR
# k40_train_prompts = unique(k40_data$train$bucketData$doc)  # SR
# k40_train_docs = nrow(k40_data$train$bucketData)  # SR
# k40_test_writers = length(unique(k40_data$test$bucketData$writer))  # SR
# k40_test_prompts = unique(k40_data$test$bucketData$doc)  # SR
# k40_test_docs = nrow(k40_data$test$bucketData)  # SR

# # Load data
# k40_data = readRDS("data/k40_data.rds")
# 
# # Format data for RJAGS
# modeldat = modelquantities_wrappedcauchy_share(bucketData = k40_data$train$bucketData, measData = k40_data$train$measData, a = 2, b = 0.25, c = 2, d = 2, e = 0.5)
# saveRDS(modeldat, "data/modeldat.rds")



# # Load RJAGS data
# modeldat = readRDS("data/modeldat.rds")
# 
# # Fit the full model
# m = jags.model(textConnection(model_wrappedcauchy_share), data = modeldat, n.chains = 1)
# save(object=m, file="data/m.rds")



# m = readRDS("data/m.rds")
# 
# # Draw MCMC samples from model
# fit = coda.samples(m, c("theta", "gamma", "mu", "rho", "eta", "nll_datamodel", "nld_locationparam"), n.iter = 4000)
# 
# # Format as list of dataframes and remove burn-in
# samps = as.data.frame(fit[[1]])
# samps = list(thetas = samps[1001:4000,grep(x = colnames(samps), pattern =  "theta")],
#              mus = samps[1001:4000,grep(x = colnames(samps), pattern =  "mu")],
#              gammas = samps[1001:4000,grep(x = colnames(samps), pattern =  "gamma")],
#              rhos = samps[1001:4000,grep(x = colnames(samps), pattern =  "rho")],
#              etas = samps[1001:4000,grep(x = colnames(samps), pattern =  "^eta")],
#              nll_datamodel = samps[1001:4000,grep(x = colnames(samps), pattern =  "nll_datamodel")],
#              nld_locationparam = samps[1001:4000,grep(x = colnames(samps), pattern =  "nld_locationparam")])
# 
# # Save
# saveRDS(object=samps, file="data/modeltest_samps_big.rds")


# Load
k40_data = readRDS("data/k40_data.rds")
modeldat = readRDS("data/modeldat.rds")
samps = readRDS("data/modeltest_samps_big.rds")

# Format 
newdocs = qdocs_wrappedcauchy(bucketData = k40_data$test$bucketData, measData = k40_data$test$measData, writers = unique(k40_data$test$bucketData$writer), writerIndices = FALSE, clustersinmodel_names = unique(k40_data$train$measData$cluster))
# if(!dir.exists("logs")){dir.create("logs")}
postprobs = posteriopredictive_wrappedcauchy(modeldat = modeldat, samps = samps, newdocs = newdocs, numCores = 5) # takes a while
# 
smry = list(samps = samps, postprobs = postprobs)
saveRDS(object = smry, file="data/modeldata/modeltest_samps_big.rds")



smry = readRDS("data/modeldata/modeltest_samps_big.rds")
# assign votes and calculate accuracy
k40_voting = lapply(smry$postprobs, function(y) {as.data.frame(t(apply(y, 1, function(x) floor(x/max(x)))))})
sapply(k40_voting, sum)
accuracy = k40_voting %>% postprobAccuracy()


########
#####
######
##########
###

##MODEL 1 CODE
# Full model run (did this on the server)
modeldat = modelquantities_wrappedcauchy_share(bucketData = data$train$bucketData, measData = data$train$measData, a = 2, b = 0.25, c = 2, d = 2, e = 0.5)
m = jags.model(textConnection(model_wrappedcauchy_share), data = modeldat, n.chains = 1)
fit = coda.samples(m, c("theta", "gamma", "mu", "rho", "eta", "nll_datamodel", "nld_locationparam"), n.iter = 4000)
samps = as.data.frame(fit[[1]])

samps = list(thetas = samps[1001:4000,grep(x = colnames(samps), pattern =  "theta")],
             mus = samps[1001:4000,grep(x = colnames(samps), pattern =  "mu")],
             gammas = samps[1001:4000,grep(x = colnames(samps), pattern =  "gamma")],
             rhos = samps[1001:4000,grep(x = colnames(samps), pattern =  "rho")],
             etas = samps[1001:4000,grep(x = colnames(samps), pattern =  "^eta")],
             nll_datamodel = samps[1001:4000,grep(x = colnames(samps), pattern =  "nll_datamodel")],
             nld_locationparam = samps[1001:4000,grep(x = colnames(samps), pattern =  "nld_locationparam")])

# saveRDS(object = samps, file = "data/modeldata/results/finalmodel_priorsensitivity/modeltest_samps_big.rds")  # SR commented out
saveRDS(object=samps, file="data/modeldata/modeltest_samps_big.rds")

#
samps1 = readRDS("data/modeldata/modeltest_samps_big.rds")
newdocs1 = qdocs_wrappedcauchy(bucketData = data$test$bucketData, measData = data$test$measData, writers = unique(data$test$bucketData$writer), writerIndices = FALSE, clustersinmodel_names = unique(data$train$measData$cluster))
# if(!dir.exists("logs")){dir.create("logs")}
postprobs1 = posteriopredictive_wrappedcauchy(modeldat = modeldat, samps = samps, newdocs = newdocs, numCores = 5) # takes a while

# smry = list(samps = samps, postprobs = postprobs)
# saveRDS(object = smry, file="data/modeldata/modeltest_samps_big.rds")

#ACCURACY
smry = readRDS("data/modeldata/modeltest_samps_big.rds")
# assign votes and calculate accuracy
k40_voting = lapply(smry$postprobs, function(y) {as.data.frame(t(apply(y, 1, function(x) floor(x/max(x)))))})
sapply(k40_voting, sum)
accuracy = k40_voting %>% postprobAccuracy()
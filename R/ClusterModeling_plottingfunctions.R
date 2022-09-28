#' plot_posterior_probabilities
#'
#' Creates a tile plot of posterior probabilities of writership for each
#' questioned document and each known writer analyzed with
#' [`analyze_questioned_documents()`]. 
#'
#' @param analysis A named list of analysis results from [`analyze_questioned_documents()`].
#' @return A tile plot of posterior probabilities of writership.
#' 
#' @examples 
#' \dontrun{
#' draws <- fit_model(example_model_training_data, num_iters = 4000)
#' draws <- drop_burnin(draws, 1000)
#' analysis <- analyze_questioned_documents(example_model_training_data, draws, example_questioned_data, num_cores = 4)
#' }
#' 
#' @md
plot_posterior_probabilities <- function(analysis) {
  # data frame of posterior probabilities
  pp <- as.data.frame(analysis$posterior_probabilities)
  pp <- cbind("known_writer" = rownames(pp), data.frame(pp, row.names=NULL))  # change rownames to column
  pp = pp %>% 
    tidyr::pivot_longer(cols = -known_writer, 
                        names_to = "questioned_document", 
                        values_to = "posterior_probability")
  
  # plot
  p = pp %>%
    ggplot2::ggplot(aes(x = known_writer, y = questioned_document, fill=posterior_probability)) + 
    geom_tile() +
    scale_fill_gradient2("Probability ", low="grey90", midpoint = 0, high="steelblue") +
    ylab("Questioned Document") + 
    xlab("Known writer") + 
    theme_bw()+
    theme(legend.position="right", axis.text.x = element_text(angle=90, hjust=0, vjust=.5))
  return(p)
}



# UNUSED? -----------------------------------------------------------------

#' Plot relative bucket frequencies.
#'
#' Takes a Ybucket (either train or test) from the \code{proclistToData} function and plots relative bucket frequencies broken down by document and writer.
#'
#' @param bucketData a data matrix with first two columns \code{writer, doc}, then bucket columns to follow. Entries in the data matrix are observed bucket counts.
#' @param plotbucket which bucket to plot relative frequencies for.
plotBuckets = function(bucketData, plotbucket = "4_BMA")
{
  Ydat_rel_m = cbind(bucketData[,c(1,2)], t(apply(X = bucketData[,-c(1,2)], MARGIN = 1,  function(x){x/sum(x)}))) %>% gather(key = "letterbucket", value = "frequency", -c(writer, doc))
  
  Ydat_plot <- Ydat_rel_m[Ydat_rel_m$letterbucket %in% plotbucket,]
  p = ggplot(Ydat_plot, aes(x=writer, y = frequency, fill = factor(doc))) +
    geom_bar(position = "dodge",stat='identity') + scale_fill_brewer(palette="Spectral", name = "Document") +
    coord_flip() + theme_bw() + xlab("Relative Frequency") + ylab("Writer") + ggtitle("Relative Bucket Frequencies", subtitle = paste0(plotbucket))
  plot(p)
}


## plot distribution of rotation angles for a writer and cluster, 
plotWrappedDensity = function(measData, writer_plot, cluster_plot, numbins, density_select, plot_density=FALSE, add_unwrapped_density = FALSE, plot_title = TRUE){
  angleset = measData %>% filter(writer == writer_plot, cluster == cluster_plot) %>% pull(pc_wrapped) %>% circular(type = "angles", units = "radians", modulo = "2pi")
  linedat = data.frame(xf = seq(0, 2*pi, length.out = 10000))
  linedat$wrappedCauchy = dwrappedcauchy(circular(linedat$xf), mu = mle.wrappedcauchy(angleset)$mu, rho = mle.wrappedcauchy(angleset)$rho)
  linedat$vonMises = dvonmises(circular(linedat$xf, units = "radians", modulo = "2pi"), mu = mle.vonmises(angleset)$mu, kappa = mle.vonmises(angleset)$kappa)
  linedat = linedat %>% pivot_longer(cols = c(wrappedCauchy, vonMises), names_to = "Density", values_to = "values") %>% filter(Density %in% density_select)
  
  p = ggplot(as.data.frame(angleset)) + theme_bw() + coord_polar(start = -pi/2, direction = -1, clip = "off") + 
    scale_x_continuous(expand = c(0,0),
                       breaks = pi*c(-4:4)/4+pi,
                       limits = c(0, 2*pi),
                       labels = c(expression(0),
                                  expression(frac(pi, 4)),
                                  expression(frac(pi, 2)),
                                  expression(frac(3*pi, 4)),
                                  expression(pi),
                                  expression(frac(5*pi, 4)),
                                  expression(frac(3*pi, 2)),
                                  expression(frac(7*pi, 4)),
                                  expression(2*pi))) +
    geom_hline(yintercept = seq(0, 1, by = 0.25), colour = "grey80", size = 0.2) +
    geom_vline(xintercept = pi*c(-4:4)/4+pi, colour = "grey80", size = 0.2) +
    scale_y_continuous(breaks = seq(0,1,by = 0.25)) + ylab("")+ xlab("")+
    geom_histogram(aes(x = x, y = 0.9*sqrt(..density..)/max(sqrt(..density..))), fill = "lightblue", color = "grey40", breaks = seq(0,2*pi, length.out = numbins+1)) +
    geom_point(aes(x = x, y = 1)) + 
    theme(panel.border = element_blank(),
          legend.key = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          panel.grid  = element_blank()) 
  
  if(plot_title) 
    p = p + ggtitle(label = "", subtitle = paste0("Writer ", writer_plot, ", Cluster ", cluster_plot))
  
  if(plot_density) 
    p = p + geom_line(data = linedat, aes(x = xf, y = 1+values, color = Density))
  
  if(!add_unwrapped_density){
    return(p)
  } else if(add_unwrapped_density){
    q = ggplot(linedat) + 
      geom_line(aes(x = xf, y = values, color = Density)) + 
      theme_bw() + 
      scale_x_continuous(breaks = pi*c(-4:4)/4+pi,
                         limits = c(0, 2*pi),
                         labels = c(expression(0),
                                    "",
                                    expression(frac(pi, 2)),
                                    "",
                                    expression(pi),
                                    "",
                                    expression(frac(3*pi, 2)),
                                    "",
                                    expression(2*pi))) + 
      ylab("") + xlab("Rotation Angle") + ylim(0,max(linedat$values))
    
    return(grid.arrange(p,q,nrow=2, layout_matrix = matrix(c(rep(1, 4), rep(1, 4), rep(1, 4), rep(2, 4)), byrow = TRUE, nrow = 4)))
  }
}





## plot distribution of rotation angles for a writer and cluster, 
plotUnwrappedHalf = function(measData, writer_plot, cluster_plot, numbins, include_yaxis = FALSE, include_title=FALSE, scaled_xaxis = FALSE){
  angleset = measData %>% filter(writer == writer_plot, cluster == cluster_plot) %>% mutate(pc_wrapped = pc_wrapped/2)
  
  p = ggplot(angleset) + theme_bw() + 
    scale_x_continuous(expand = c(0,0),
                       breaks = pi*c(0:4)/4,
                       limits = c(0, pi),
                       labels = c(expression(0),
                                  expression(frac(pi, 4)),
                                  expression(frac(pi, 2)),
                                  expression(frac(3*pi, 4)),
                                  expression(pi))) +
    xlab("Rotation Angle") + 
    geom_histogram(aes(x = pc_wrapped, y = ..density..), fill = "lightblue", color = "grey40", breaks = seq(0,pi, length.out = numbins+1)) +
    geom_point(aes(x = pc_wrapped, y = 0))
  
  if(scaled_xaxis){
    p = ggplot(angleset) + theme_bw() + 
      xlab("Scaled Rotation Angle") + 
      geom_histogram(aes(x = pc_wrapped/pi, y = ..density..), fill = "lightblue", color = "grey40", breaks = seq(0,1, length.out = numbins+1)) +
      geom_point(aes(x = pc_wrapped/pi, y = 0))
  }
  
  if(include_yaxis){
    p = p + ylab("Density")
  }
  
  if(!include_yaxis){
    p = p + ylab("") + 
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
  }
  
  if(include_title){
    p = p + ggtitle(label = "", subtitle = paste0("Writer ", writer_plot, ", Cluster ", cluster_plot))
  }
  return(p)
}



# plot the blue grid
plotpostprobs_old = function(train, postprobs_plot){
  for(i in 1:length(postprobs_plot)){
    colnames(postprobs_plot[[i]]) = as.factor(as.numeric(unique(train$bucketData$writer)))
    rownames(postprobs_plot[[i]]) = 1:nrow(postprobs_plot[[i]])
    postprobs_plot[[i]] = cbind(postprobs_plot[[i]], questioned = as.factor(as.numeric(unique(train$bucketData$writer)[i])))
  }
  
  postprobs_plot = do.call("rbind", postprobs_plot)
  
  mpp = postprobs_plot %>% 
    pivot_longer(cols = -questioned, names_to = "known", values_to = "prob") %>% 
    mutate(known = factor(known, levels = unique(train$bucketData$writer))) %>%
    group_by(questioned, known) %>% 
    summarise(meanpostprob = mean(prob))
  
  # plot
  p = mpp %>%
    ggplot(aes(x = known, y = questioned, fill=meanpostprob)) + 
    geom_tile() +
    #scale_fill_continuous(low = "white", high = adjustcolor("steelblue"))+
    scale_fill_gradient2("Probability ", low="grey90", midpoint = 0, high="steelblue") +
    #ggtitle("", subtitle = paste0(round(ondiag,4), " on diagonal")) +
    ylab("Questioned") + 
    xlab("Known") + theme_bw()+
    theme(legend.position="right", axis.text.x = element_text(angle=90, hjust=0, vjust=.5))
  
  return(p)
}


postprobAccuracy = function(postprobs_calc){
  nwriters = length(postprobs_calc)
  meanmat = matrix(nrow = nwriters, ncol = nwriters)
  for(i in 1:length(postprobs_calc)){
    meanmat[i,] = postprobs_calc[[i]] %>% colMeans()
  }
  denom = nwriters-sum(is.na(diag(meanmat)))
  return(list(ondiag = sum(diag(meanmat), na.rm = TRUE)/denom, num_na = sum(is.na(diag(meanmat)))))
}



formatResults = function(resultlist, data, long=TRUE){
  for(i in 1:length(resultlist)){
    colnames(resultlist[[i]]) = as.factor(as.numeric(unique(data$train$bucketData$writer)))
    rownames(resultlist[[i]]) = 1:nrow(resultlist[[i]])
    resultlist[[i]] = cbind(resultlist[[i]], questioned = as.factor(as.numeric(unique(data$train$bucketData$writer)[i])))
  }
  
  resultgrid = do.call("rbind", resultlist)
  
  if(!long){
    resultgrid = resultgrid %>% 
      pivot_longer(cols = -questioned, names_to = "known", values_to = "prob") %>% 
      mutate(known = factor(known, levels = unique(data$train$bucketData$writer))) %>%
      group_by(questioned, known) %>% 
      summarise(meanpostprob = mean(prob)) %>%
      pivot_wider(names_from = known, values_from = meanpostprob)
  }
  
  if(long){
    resultgrid = resultgrid %>% 
      pivot_longer(cols = -questioned, names_to = "known", values_to = "prob") %>% 
      mutate(known = factor(known, levels = unique(data$train$bucketData$writer))) %>%
      group_by(questioned, known) %>% 
      summarise(meanpostprob = mean(prob))
  }
  
  return(resultgrid)
}



get_diagonalprops_byit = function(postprobs, numCores = 2, logdocname = "diagonalprop_byit"){
  it_diagonalprop = c()
  
  registerDoParallel(cores = numCores)
  #writeLines(c(""), paste0("logs/", logdocname,".txt"))
  
  ls = foreach(i = 1:nrow(postprobs[[1]]), .combine = c) %dopar% {
    it_grid = matrix(NA, nrow = ncol(postprobs[[1]]), ncol= ncol(postprobs[[1]]))
    it_grid = t(sapply(postprobs, function(x){as.numeric(x[i,])}))
    if(sum(is.na(it_grid)) > 0){cat(i, "has an NA.")}
    it_diagonalprop = mean(diag(it_grid))
    #cat(i, " ", file = paste0("logs/", logdocname, ".txt"), append = TRUE)
    return(it_diagonalprop)
  }
  return(ls)
}

# find highest posterior probability writer
get_hppw_dist = function(postprobs){
  nwriters = length(postprobs)
  meanmat = matrix(nrow = nwriters, ncol = nwriters)
  for(i in 1:length(postprobs)){
    meanmat[i,] = postprobs[[i]] %>% colMeans()
  }
  
  hppw = c()
  hppw_samps = matrix(nrow = nrow(postprobs[[1]]), ncol = ncol(postprobs[[1]]))
  for(j in 1:length(postprobs)){
    hppw[j] = which.max(meanmat[j,])
    hppw_samps[,j] = postprobs[[j]][,hppw[j]]
  }
  return(list(hppw = hppw, hppw_samps = hppw_samps))
}

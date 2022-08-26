library(foreach)
library(doParallel) 


############################################################################################################################################
## MODEL 
############################################################################################################################################
model_wrappedcauchy_share = "
model {
for(letter in 1:numletters){                          /* numletters = num unique letters with measurements */
  nll_datamodel[letter] = -log( (1-pow(rho[letterwriter[letter], lettercluster[letter]],2)) / (2*pi*(1+pow(rho[letterwriter[letter], lettercluster[letter]],2)-2*rho[letterwriter[letter], lettercluster[letter]]*cos(pc_wrapped[letter]-mu[letterwriter[letter], lettercluster[letter]]))) ) + C
  zero_vec[letter] ~ dpois( nll_datamodel[letter] )
}


# Priors for wrapped cauchy
for(g in 1:Gsmall){    
  gamma[g] ~ dgamma(a, b) 
  eta[g] ~ dunif(0,2*pi)
  for(w in 1:W){                                      /* W = num unique writers */
    mu[w,g]  ~ dunif(0,2*pi)
    nld_locationparam[w,g] = -log( (1-pow(e,2)) / (2*pi*(1+pow(e,2)-2*e*cos(mu[w,g]-eta[g]))) ) + C
    zero_mat[w,g] ~ dpois(nld_locationparam[w,g])
    rho[w,g] ~ dbeta(c,d) 
  }
}

for (w in 1:W) {                                      /* W = num unique writers */
  theta[w,1:G] ~ ddirch(gamma[1:G] + 0.001)
}

for(d in 1:D) {                                       /* D = num unique documents */
  Y[d,1:G] ~ dmulti(theta[docwriter[d],1:G], docN[d])
}

# other values
C    = 30   # for the ones's trick
pi   = 3.14159
pi_1 = -pi
}"


posteriopredictive_wrappedcauchy = function(modeldat, samps, newdocs, numCores = 2, logdocname = "postpred_log"){ #mcmc_samples = coda samples c('theta')
  niter = nrow(samps$thetas)
  thetas = array(dim = c(niter, modeldat$G, modeldat$W))    # 3 dim array, a row for each mcmc iter, a column for each cluster, and a layer for each writer
  mus = rhos = array(dim = c(niter, modeldat$Gsmall, modeldat$W))    # 3 dim array, a row for each mcmc iter, a column for each cluster, and a layer for each writer
  dmult = dwc_sums = data.frame(matrix(nrow = niter, ncol = modeldat$W))
  layered_wc_params = array(dim = c(niter, modeldat$Gsmall, 2))
  ls = list()
  
  flat_theta = as.data.frame(cbind(iters = 1:niter, samps$thetas))
  flat_mus = as.data.frame(cbind(iters = 1:niter, samps$mus))
  flat_rhos = as.data.frame(cbind(iters = 1:niter, samps$rhos))
  
  for(i in 1:modeldat$W){ #i is writer, j is graph
    for(j in 1:modeldat$G){
      thetas[,j,i] = flat_theta[1:niter, as.character(paste0("theta[", i, ",", j, "]"))]
    }
  }  
  
  for(i in 1:modeldat$W){ #i is writer, j is graph
    for(j in 1:modeldat$Gsmall){
      mus[,j,i] = flat_mus[1:niter, as.character(paste0("mu[", i, ",", j, "]"))]
      rhos[,j,i] = flat_rhos[1:niter, as.character(paste0("rho[", i, ",", j, "]"))]
    }
  }  
  
  registerDoParallel(cores = numCores)
  writeLines(c(""), paste0("logs/", logdocname,".txt"))
  
  ls = foreach(m = 1:nrow(newdocs$bucketData)) %dopar% {
    m_qdoc = newdocs$measData %>% filter(as.numeric(writer) == m)
    m_cluster = as.numeric(m_qdoc$cluster)
    m_pcrot = circular(m_qdoc$pc_wrapped, units = "radians", modulo = "2pi")
    
    if(length(m_cluster) > 0){
      for(i in 1:modeldat$W){ #i is writer, j is graph
        dmult[,i] = dmultinomial(x = newdocs$bucketData[m,-c(1,2)], prob = thetas[,,i], log = TRUE)
        layered_wc_params[,,1] = mus[,,i]; layered_wc_params[,,2] = rhos[,,i]
        dwc_sums[,i] = rowSums(t(sapply(1:niter, function(it) log(dwrappedcauchy(x = circular(m_pcrot), mu = circular(layered_wc_params[it, m_cluster, 1]), rho = layered_wc_params[it,m_cluster,2])))))
        if(i%%10==0) cat(paste0(i, "mwc "), file = paste0("logs/", logdocname,".txt"), append=TRUE)
      }
      nn = dmult+dwc_sums+abs(max(colMeans(dmult+dwc_sums)))
    } else if(length(m_cluster) == 0){
      for(i in 1:modeldat$W){ #i is writer, j is graph
        dmult[,i] = dmultinomial(x = newdocs$bucketData[m,-c(1,2)], prob = thetas[,,i], log = TRUE)
        if(i%%10==0) cat(paste0(i, "mult "), file = paste0("logs/", logdocname,".txt"), append=TRUE)
      }
      nn = dmult+abs(max(colMeans(dmult)))
    }
    
    cat("\n qdoc for writer ", rownames(newdocs$bucketData)[m], " done. \t(", m, " of ", nrow(newdocs$bucketData), ") \n", sep = "", file = paste0("logs/", logdocname,".txt"), append=TRUE)
    
    postprobs = as.data.frame(exp(nn)/rowSums(exp(nn)))
    #saveRDS(object = postprobs, file = paste0("data/2_mixedsources/postprobs_ind/postprobs_", m))
    return(postprobs)
  }
  return(ls)
}

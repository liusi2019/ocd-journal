#################################################################################
## this code contains four functions for four MPE method in PU learning.
## require:
##      * funcs_patrasen.R
##      * funcs_roc.R
#################################################################################

## this reduced version only cares about getting estimates
## method_roc returns predicted scores from kernel logistic regression as well

## c-patra/sen's estimation
##' p0: probability prediction of unlabeled samples
##' p1: probability prediction of positive samples
##' cl: true class label of unlabeled samples, default is NULL, only used to get accuracy
##' alpha: true alpha, default is 0.5, only used to plot
##' plotit: to plot or not
method_c_patrasen <- function(p0, p1, cl=NULL, alpha=0.5, plotit=F,...){
  pi_hat = length(p1)/(length(p0)+length(p1))
  out <- EstMixMdl(p0, ecdf(p1),alpha_grid = (1:1000)/1000)
 
  dder = ComputeSecondDeriv(out$distance)
  ix <- which.max(dder)
  alpha0 <- out$alpha_grid[ix]
  
  if(alpha0 < 0){
    alpha0 = 0
  }
  if(alpha0 > 1){
    alpha0 = 1
  }
  return(alpha0)
}

## c-roc estimation
##' p0: probability prediction of unlabeled samples
##' p1: probability prediction of positive samples
##' cl: true class label of unlabeled samples, default is NULL, only used to get accuracy
method_c_roc <- function(p0, p1, cl=NULL){
  
  n0 = length(p0)
  n1 = length(p1)
  pi_hat = n1/(n1+n0)
  
  post = c(p1,p0)
  y = c(rep(1,n1), rep(-1,n0))
  
  ##mpi1t_roc = kappahat(post, y, n0, n1)
  mpi0t_roc = kappahat(1-post, -y, n1, n0)
  ##alpha0 = 1 - mpi0t_roc*(1-mpi1t_roc)/(1-mpi0t_roc*mpi1t_roc)
  alpha0 <- 1 -mpi0t_roc

  if(alpha0 < 0){
    alpha0 = 0
  }
  if(alpha0 > 1){
    alpha0 = 1
  }

  return(alpha0)
}


## roc estimation
## modify it on July 9th to additionaly return yhat as predicted probability for being clean
##' x0: unlabeled set
##' x1: positive set
##' cl: true class label of unlabeled samples, default is NULL, only used to get accuracy
##' nrep = 5: number of repetitions to get robust result

method_roc <- function(x0, x1, cl=NULL, nrep = 5){
  pi_hat = nrow(x1)/(nrow(x0)+nrow(x1))
  pi1t_roc = rep(0,nrep)
  pi0t_roc = rep(0,nrep)
  yhat = rep(0,nrow(x0))
  for(ii in 1:nrep){
    cpe.fit = cpe(x1, x0)
    #yhat = yhat + cpe.fit$Y_fit$probabilities[1:nrow(x0),1]
    yhat = yhat + cpe.fit$Y_fit$probabilities[,1] # get the predicted probability on both clean and mixture
    eta = cpe.fit$eta # this part is on testing data
    y = cpe.fit$labels # this part is on testing data
    nvec = cpe.fit$nvec
    ##pi1t_roc[ii] = kappahat(eta, y, nvec[4], nvec[3])
    pi0t_roc[ii] = kappahat(1-eta, -y, nvec[3], nvec[4])
  }
  yhat = yhat/nrep
  
  ##mpi1t_roc = median(pi1t_roc)
  mpi0t_roc = median(pi0t_roc)
  ##alpha0 = 1 - mpi0t_roc*(1-mpi1t_roc)/(1-mpi0t_roc*mpi1t_roc)
  alpha0 <- 1 - mpi0t_roc
  if(alpha0 < 0){
    alpha0 = 0
  }
  if(alpha0 > 1){
    alpha0 = 1
  }
  
  return(list(alpha0 = alpha0, yhat = yhat))
}

## spy tech
##' x0: unlabeled set
##' x1: positive set
##' cl: true class label of unlabeled samples, default is NULL, only used to get accuracy
##' s = proportion of positive as spy set
##' l = noise level to determine threshold for reliable negative set
method_spy <- function(x0,x1, cl=NULL, s=0.15,l=0.15){
  indspy <- ceiling(nrow(x1)*(1-s)):nrow(x1)
  datspy = rbind(x1,x0)
  datspy$cl <- as.factor(c(rep("labeled",nrow(x1)), rep("unlabeled",nrow(x0))))
  datspy$cl[indspy] = "unlabeled"
  
  ## first classifier to extract "reliable negative" set
  rf.fit1 <- randomForest(cl~.,data=datspy)
  rf.preds1 <- predict(rf.fit1,type='prob')
  preds1 <- rf.preds1[,1]
  
  pspy = preds1[indspy]
  
  thres = min(quantile(pspy, l))
  xrn = x0[preds1[(nrow(x0)+1):length(preds1)]<thres,] # reliable negative
  dat_PRN = rbind(x1, xrn) # P + RN
  dat_PRN$cl <- as.factor(c(rep("labeled",nrow(x1)), rep("unlabeled",nrow(xrn))))
  
  ## second classifier to predict unlabeled set
  rf.fit2 <- randomForest(cl~.,data=dat_PRN)
  rf.preds2 <- predict(rf.fit2, x0)
  
  alpha0 = mean(rf.preds2=="unlabeled")
  if(alpha0 < 0){
    alpha0 = 0
  }
  if(alpha0 > 1){
    alpha0 = 1
  }
  
  return(alpha0)
}





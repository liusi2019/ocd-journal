## this file contains the functions for estimating threshold and getting the recall & FPR

raw_cdf<- function(datab, datan, alpha, epsilon){
  trialv <- sort(c(datan, datab))
  F.n <- ecdf(datan)
  Fn  <- F.n(trialv)
  F.b <- ecdf(datab)
  Fb <- F.b(trialv)
  Fa <- (Fn - (1-alpha)*Fb)/alpha 
  if (length(which(Fa <= epsilon))==0){
    index <- 1
  }else{
    index <-max(which(Fa <= epsilon))
  }
  return(trialv[index])
}


cv_result <- function(datab, datan, na, alpha_est, epsilon){
  ## 10 fold cross validation
  nn <- length(datan)
  ## need to pay attention to whether nn * alpha is an integer or not
  ## the exact value of na is very important for computing recall and FPR
  #na <- round(nn*alpha) ## number of anomaly points. If na/nn = alpha, just use this one. If na/nn doesn't equal alpha exactly, write the exacty number of anomaly points. 
  # let's directly take na as one input
  if (nn/10 - floor(nn/10) > 0){
    group <- c(rep((1:10), floor(nn/10)), (1:(nn - 10*floor(nn/10))))
  }else{
    group <- rep((1:10), floor(nn/10))
  }
  group.label <- sample(group)
  
  ## build a data frame to help with grouping and identifying anomaly points
  ## the first column is for group id
  ## the second column is the row number of each point. All points with row number smaller or equal than na are actual anomaly points
  df <- data.frame(group.label = group.label, index = (1:nn))
  
  ## construct vectors for recording some intermediate quantities in order to compute the final output
  ## for grid search
  correct.vec <- rep(0, 10) ## number of correctly detected anomaly points
  wrong.vec <- rep(0, 10) ## number of nominal points that are wrongly claimed to be anomaly points
  ano.vec <- rep(0, 10) ## number of anomaly points in this fold. This is supposed to be the same across all 3 or 4 methods.
  nom.vec <- rep(0, 10) ## number of nominal points in this fold. Set one copy for each method, just for convenience in coding. 
  
  
  result <- rep(0, 2)
  
  
  for (i in 1:10){
    use_datan <- datan[which(df$group.label!= i)] ## anomaly scores from the 9 folds to use
    test_datan <- datan[which(df$group.label == i)] ## anomaly scores from the left out 1 fold to use as testing data
    use_df <- df[which(df$group.label!= i),] ## the part of group label dataframe that correspond to the 9 folds
    test_df <- df[which(df$group.label == i),] ## the part of group label dataframe that correspond to the 1 fold
    index_use <- use_df$index ## index of the 9 folds
    index_test <- test_df$index ## index of the 1 fold
    
    est <- raw_cdf(datab, use_datan, alpha_est, epsilon) ## estimate from raw ECDF method
    
    
    if (length(which(test_datan >= est))==0){
      correct.vec[i] <- 0 ## number of correctly claimed anomaly points
      wrong.vec[i] <- 0 ## number of nominal points which are wrongly claimed to be anomaly points
    }else{
      pos <- index_test[which(test_datan >= est)] ## the row numbers of the points in the 1 fold test data which are claimed to be anomaly
      correct.vec[i] <- sum(pos <= na) 
      wrong.vec[i] <- sum(pos > na) 
    }
    
    ano.vec[i] <- sum(index_test <= na) ## number of anomaly points in this testing data set 
    nom.vec[i] <- sum(index_test > na) ## number of nominal points in this testing data set
    
  }
  
  
  result[1] <- sum(correct.vec)/sum(ano.vec) ## recall. Actuallty here sum(ano.vec) should equal n*alpha
  #result[5] <- sum(wrong.vec2)/(sum(correct.vec2) + sum(wrong.vec2)) ## false positive rate. Will be NA if no point is claimed to be anomaly
  result[2] <- sum(wrong.vec)/sum(nom.vec) ## what proportion of nominal points are claimed to be anomaly points.
  
  #result[10] <- mean(score_nominal > quantile(score_anomaly, 0.05))
  
  return(result)
}

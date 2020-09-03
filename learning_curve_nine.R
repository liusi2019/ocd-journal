





## 
## 9 different estimators:
## KM1, KM2, KM1_score, KM2_score, bt_patrasen, c_patrasen, c_roc, roc, spy


## in this file we use updated source R file which corrected the random seed issue with function cpe in file funcs_roc.R

## this version has specific part to convert the matrix format of data1, data2 to data frame

## steps:
## (1) combine the 9 dimensional normal generation part into this file
##     pay attention to how to connect the loading data sets with both R and python files
## (2) adjust the 4 KM estimator part 
## (3) Ajust to output data frame


## for this file, alpha = 0.05, 0.10, 0.20, 0.40
## alpha = 0.05,  is too small and causes troubles when n = 100, let's start from alpha = 0.10
## for this file, n = 100, 500, 1000
evaluation <- function(k){ 

  library(randomForest)
  library(MASS)
  #library(reticulate)
  source("R_file_for_sourcing/bt_patrasen.R")
  source("R_file_for_sourcing/funcs_reduced.R")
  source("R_file_for_sourcing/funcs_patrasen.R")
  source("R_file_for_sourcing/funcs_roc.R")
  #source("generate_data_set.R")
  #source("out_of_bag_recall_FPR.R")
  
  ## *** the cv_result function requires the anomaly points to be put before the nominal points in the mixture, 
  ## for the convenience of computing recall and FPR
  
  
  random_seeds = as.numeric(unlist(read.csv("random_seeds.csv", header = TRUE)))
  
  epsilon <- 0.05  # target on 5% quantile
  
  
  
  
  
  ## %%% part 0: basic settings
  
  size_vec = c(100, 500, 1000)
  #name_vec = c("landsat", "LR")
  #alpha_vec = c(0.10, 0.20, 0.40)
  alpha_vec = c(0.10, 0.20, 0.40)
  
  
  n_size = length(size_vec)
  n_alpha = length(alpha_vec) # number of different alpha values
  n_est = 1 + 9 # 1 count for true value of alpha, 9 counts for the nine different estimators
  
  ## create vectors to hold results
  ## the final data frame should contain: data_name, alpha, est_alpha, est_name, recall, FPR
  total_row = n_size * n_alpha * n_est
  col_size = rep(0, total_row)
  col_alpha = rep(0, total_row)
  col_est_name = rep("", total_row)
  col_est_alpha = rep(0, total_row) 
  #col_recall = rep(0, total_row)
  #col_FPR = rep(0, total_row)
  
  r_index = 0 # prepare for starting from filling row 1
  
  n_size = 1
  n_alpha = 1
  for (i in 1:n_size){
    
  ## %%% part 1: generate data sets
    
  n = size_vec[i]
  
  for (j in 1:n_alpha){
  
  alpha = alpha_vec[j]
  
  set.seed(random_seeds[k])
  
  # insert the 9 dimensional normal generation here
  
  
  
  ##  generate nominal and mixture datasets, each of size n
  dat =  matrix(ncol = 9, nrow = 2*n)
  vmat = matrix(0, ncol = 9, nrow = 9)
  diag(vmat) = 1
  for(i in (1:round(n*alpha))){
    center = rep(0, 9)
    if(rbinom(1, 1, 0.4)==1){
      center[sample(9, 3,replace = F)] = 3
    }else{
      center[sample(9, 4,replace = F)] = 3
    }
    dat[i,] = mvrnorm(1, center, vmat)
  } 
  
  nnrow <- round(n * (2 - alpha))
  rvec <- rnorm(9 * nnrow, 0, 1)
  ## the top alpha * n points are alien points 
  dat[(round(n*alpha)+1):(2*n),] <- base::matrix(rvec, nrow = round(n*(2-alpha)), ncol = 9)
  
  data1 <- dat[(n+1):(2*n),] #nominal data set
  data2 <- dat[1:n,] #mixture data set
  
  ## write them into .csv file for running iforest using linux    
  #write.csv(data1, file = paste("data1.csv", sep = ""), row.names = FALSE)
  #write.csv(data2, file = paste("data2.csv", sep = ""), row.names = FALSE)
  
  
  #data_input = generate_data_set(data_name, alpha)
  #data1 = data_input$clean
  #data2 = data_input$mixture
  na = round(n * alpha)
  #n = nrow(data1)
  ## %%% part 2: get anomaly scores
  
  ## write data sets out 
  vpro = round(alpha * 100)
  
  write.csv(data1, file = paste("data1_",vpro,"_",n,"_", k, ".csv", sep = ""), row.names = FALSE) # write clean data out
  write.csv(data2, file = paste("data2_",vpro,"_",n,"_", k, ".csv", sep = ""), row.names = FALSE) # write mixture data out
  
  ################ !!!!!!!!!!!!!!!!!!! here we are using 20% of the points for growing each tree
  ################ OOB method
  ################ use randomly selected 20% of the points to grow each tree, 
  ################ for each point, use the average depth from the trees which don't use this point to calculate anomaly score
  ################ the following part is for getting anomaly scores for all clean data set, mixture data set and test data set. Also ground truth data sets. 
  ### use the first clean data set to grow the forest, and store the trees as "forest1_10_10000_1.bin" for example. 
  system(paste('./iforest','-i', paste('./data1_',vpro,'_',n,'_',k,'.csv', sep = ""),'-o', paste('./depth1_',vpro,'_',n,'_',k,'.csv', sep = ""),'-s', round(0.2*n), '-t 1000 -g -k -b', paste('./forest1_',vpro,'_',n,'_', k,'.bin',sep = "")), wait = TRUE)
  score1 <- read.csv(paste('./depth1_',vpro,'_',n,'_',k,'.csv', sep = ""), header = TRUE)
  datab <- as.numeric(unlist(score1))
  
  ### run the mixture data set through the forest  
  system(paste('./iforest','-i', paste('./data2_',vpro,'_',n,'_',k,'.csv', sep = "") ,'-o',paste('./depth2_',vpro,'_',n,'_',k,'.csv', sep = ""),'-g -f', paste('./forest1_',vpro,'_',n,'_', k,'.bin',sep = "")), wait = TRUE)
  score2 <- read.csv(paste('./depth2_',vpro,'_',n,'_',k,'.csv', sep = ""), header = TRUE)
  datan <- as.numeric(unlist(score2))
  
  ### remove the forest stored in the directory
  system(paste('rm','-f',file = paste('./forest1_',vpro,'_',n,'_', k,'.bin',sep = ""), row.names = FALSE))
  

  

  ## %%% part 3: get alpha estimates
  
  ## use true alpha
  
  r_index = r_index + 1
  ### datab: anomlay scores for the second clean data set
  ### datan: anomaly scores for the first mixture data set
  ### alpha: anomaly proportion
  ### epsilon: which quantile of the anomaly score of anomlay points are we focusing on
  #output <- cv_result(datab, datan, na, alpha, epsilon)
  col_size[r_index] = n
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "truth"
  col_est_alpha[r_index] = alpha

  
  
  ## get KM1, KM2, KM1_score, KM2_score

  clean_name = paste("data1_",vpro,"_",n,"_", k, ".csv", sep = "")
  mixture_name = paste("data2_",vpro,"_",n,"_", k, ".csv", sep = "")
  py_output_name = paste("KM_ori_",vpro,"_",n,"_", k, ".csv", sep = "")
  
  system(paste("python KM.py", clean_name, mixture_name, 0, py_output_name), wait = TRUE)
  KM_ori_df = read.csv(py_output_name, header = TRUE)
  
  est_alpha_1 = KM_ori_df$alpha_1[1]
  est_alpha_2 = KM_ori_df$alpha_2[1]
  
  r_index = r_index + 1
  
  #output <- cv_result(datab, datan, na, est_alpha_1, epsilon)
  col_size[r_index] = n
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "alpha_1"
  col_est_alpha[r_index] = est_alpha_1
  #col_recall[r_index] = output[1]
  #col_FPR[r_index] = output[2]  
  
  r_index = r_index + 1
  
  #output <- cv_result(datab, datan, na, est_alpha_2, epsilon)
  col_size[r_index] = n
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "alpha_2"
  col_est_alpha[r_index] = est_alpha_2
  #col_recall[r_index] = output[1]
  #col_FPR[r_index] = output[2]  
  
  
  system(paste('rm','-f',file = paste("data1_",vpro,"_",n,"_", k, ".csv", sep = ""), row.names = FALSE))
  system(paste('rm','-f',file = paste("data2_",vpro,"_",n,"_", k, ".csv", sep = ""), row.names = FALSE))  
  system(paste('rm','-f', py_output_name)) 
  
  
  clean_name = paste('./depth1_',vpro,'_',n,'_',k,'.csv', sep = "")
  mixture_name = paste('./depth2_',vpro,'_',n,'_',k,'.csv', sep = "")
  py_output_name = paste("KM_score_",vpro,"_",n,"_", k, ".csv", sep = "")
  
  system(paste("python KM.py", clean_name, mixture_name, 1, py_output_name), wait = TRUE)
  KM_score_df = read.csv(py_output_name, header = TRUE)
  
  est_alpha_1_score = KM_score_df$alpha_1[1]
  est_alpha_2_score = KM_score_df$alpha_2[1]
  
  r_index = r_index + 1
  
  #output <- cv_result(datab, datan, na, est_alpha_1_score, epsilon)
  col_size[r_index] = n
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "alpha_1_score"
  col_est_alpha[r_index] = est_alpha_1_score
  #col_recall[r_index] = output[1]
  #col_FPR[r_index] = output[2]  
  
  
  r_index = r_index + 1
  
  #output <- cv_result(datab, datan, na, est_alpha_2_score, epsilon)
  col_size[r_index] = n
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "alpha_2_score"
  col_est_alpha[r_index] = est_alpha_2_score
  #col_recall[r_index] = output[1]
  #col_FPR[r_index] = output[2]  
  
  
  system(paste('rm','-f',file = paste('./depth1_',vpro,'_',n,'_',k,'.csv', sep = ""), row.names = FALSE))
  system(paste('rm','-f',file = paste('./depth2_',vpro,'_',n,'_',k,'.csv', sep = ""), row.names = FALSE))  
  system(paste('rm','-f', py_output_name)) 
  
  
  ## source_python('python_file_for_sourcing/KM.py')
  ##use_condaenv("base")
  ##use_python("/usr/bin/python")
  
  
  ## bt_patrasen
  
  r_index = r_index + 1
  
  grid_num = 200
  boot_num = 100
  est_bt_patrasen = bt_patrasen(datab, datan, grid_num = grid_num, boot_num = boot_num)
    
  #output <- cv_result(datab, datan, na, est_bt_patrasen, epsilon)
  col_size[r_index] = n
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "bt_patrasen"
  col_est_alpha[r_index] = est_bt_patrasen
  #col_recall[r_index] = output[1]
  #col_FPR[r_index] = output[2]  
  
  
  ## c_patrasen

  dat = rbind(data1, data2)
  dat = as.data.frame(dat)
  dat$cl <- as.factor(c(rep("labeled",nrow(data1)), rep("unlabeled",nrow(data2))))
  rf.fit <- randomForest(cl~.,data=dat, ntree=1000)
  rf.preds <- predict(rf.fit,type='prob')
  preds <- rf.preds[,1]
  lu <- rep(0,length(preds))
  lu[dat$cl=="labeled"] <- 1
  p0 = preds[lu==0]
  p1 = preds[lu==1]
  p0 = as.numeric(unlist(p0)) # mixture
  p1 = as.numeric(unlist(p1)) # clean
  
  r_index = r_index + 1
  
  est_c_patrasen = method_c_patrasen(p0, p1) ## c-patra/sen
  
  #output <- cv_result(datab, datan, na, est_c_patrasen, epsilon)
  col_size[r_index] = n
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "c_patrasen"
  col_est_alpha[r_index] = est_c_patrasen
  #col_recall[r_index] = output[1]
  #col_FPR[r_index] = output[2] 

  
  ## c_roc
  
  r_index = r_index + 1
  
  est_c_roc = method_c_roc(p0, p1) ## c-roc
  
  #output <- cv_result(datab, datan, na, est_c_roc, epsilon)
  col_size[r_index] = n
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "c_roc"
  col_est_alpha[r_index] = est_c_roc
  #col_recall[r_index] = output[1]
  #col_FPR[r_index] = output[2] 
  
  ## roc
  
  r_index = r_index + 1
  
  est_roc = method_roc(data2, data1) ## ked
  
  #output <- cv_result(datab, datan, na, est_roc, epsilon)
  col_size[r_index] = n
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "roc"
  col_est_alpha[r_index] = est_roc$alpha0
  #col_recall[r_index] = output[1]
  #col_FPR[r_index] = output[2]   
  
  
  ## spy method
  
  r_index = r_index + 1
  
  est_spy = method_spy(as.data.frame(data2), as.data.frame(data1))
  
  #output <- cv_result(datab, datan, na, est_spy, epsilon)
  col_size[r_index] = n
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "spy"
  col_est_alpha[r_index] = est_spy
  #col_recall[r_index] = output[1]
  #col_FPR[r_index] = output[2]  

  
  }
  }
  
  
  df_output = data.frame("size" = col_size, 
                         "alpha" = col_alpha, 
                         "est_name" = col_est_name,
                         "est_alpha" = col_est_alpha)
  
  write.csv(df_output, file = paste("learning_curve_nine_small/learning_curve_nine_small_alpha_", n, "_", k,".csv", sep = ""),row.names = FALSE)  ### write the result into .csv file
}


args = commandArgs(trailingOnly = TRUE)
k = as.numeric(args[1])
evaluation(k)
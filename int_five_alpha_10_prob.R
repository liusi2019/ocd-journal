


## this version uses predicted probabilities from classifier instead of anomaly scores from isolation forest

## five methods:
## bt_patrasen uses predicted probability (from c_patrasen) for doing proportion estimation and cv_result
## c_patrasen builds random forest, get predicted probability and uses that for doing proportion estimation and cv_result
## c_roc uses predicted probability (from c_patrasen) for doing proportion estimation and cv_result
## roc uses takes original data to get proportion estimation, and uses its own kernel logistic regression predicted probabilty to get cv_result
## spy method takes original data and uses predicted probability (from c_patrasen) to get cv_result

## 


evaluation <- function(k){ 
  ### out of bag edition of isolation forest, only need one clean data set
  ### cross validation for getting final results, only need one mixture data set
  
  library(randomForest)
  #library(reticulate)
  source("R_file_for_sourcing/bt_patrasen.R")
  source("R_file_for_sourcing/funcs_reduced.R")
  source("R_file_for_sourcing/funcs_patrasen.R")
  source("R_file_for_sourcing/funcs_roc.R")
  source("R_file_for_sourcing/generate_data_set.R")
  source("R_file_for_sourcing/out_of_bag_recall_FPR.R")
  
  ## the cv_result function requires the anomaly points to be put before the nominal points in the mixture, 
  ## for the convenience of computing recall and FPR
  
  
  random_seeds = as.numeric(unlist(read.csv("random_seeds.csv", header = TRUE)))
  
  q <- 0.05  # target on 5% quantile
  
  ## %%% part 0: basic settings
  name_vec = c("landsat", "LR", "OCR", "pageb", "shuttle", "covertype")
  #name_vec = c("landsat", "LR")
  #alpha_vec = c(0.10, 0.20, 0.40)
  alpha_vec = c(0.10)
  
  
  n_name = length(name_vec)
  n_alpha = length(alpha_vec) # number of different alpha values
  n_est = 1 + 5 # 1 count for true value of alpha, 9 counts for the nine different estimators
  
  ## create vectors to hold results
  ## the final data frame should contain: data_name, alpha, est_alpha, est_name, recall, FPR
  total_row = n_name * n_alpha * n_est
  col_data_name = rep("", total_row)
  col_alpha = rep(0, total_row)
  col_est_name = rep("", total_row)
  col_est_alpha = rep(0, total_row) 
  col_recall = rep(0, total_row)
  col_FPR = rep(0, total_row)
  
  r_index = 0 # prepare for starting from filling row 1
  
  for (i in 1:n_name){
  ## %%% part 1: generate data sets
  data_name = name_vec[i]
  
  for (j in 1:n_alpha){
  
  alpha = alpha_vec[j]
  
  set.seed(random_seeds[k])
  data_input = generate_data_set(data_name, alpha)
  data1 = data_input$clean
  data2 = data_input$mixture
  na = data_input$na
  n = nrow(data1)
  ## %%% part 2: get anomaly scores
  
  ## write data sets out 
  vpro = round(alpha * 100)
  #write.csv(data1, file = paste("data1_",vpro,"_",n,"_", k, ".csv", sep = ""), row.names = FALSE) # write clean data out
  #write.csv(data2, file = paste("data2_",vpro,"_",n,"_", k, ".csv", sep = ""), row.names = FALSE) # write mixture data out
  ################ finish generating and writing the data
  
  ################ !!!!!!!!!!!!!!!!!!! here we are using 20% of the points for growing each tree
  ################ OOB method
  ################ use randomly selected 20% of the points to grow each tree, 
  ################ for each point, use the average depth from the trees which don't use this point to calculate anomaly score
  ################ the following part is for getting anomaly scores for all clean data set, mixture data set and test data set. Also ground truth data sets. 
  ### use the first clean data set to grow the forest, and store the trees as "forest1_10_10000_1.bin" for example. 
  #system(paste('./iforest','-i', paste('./data1_',vpro,'_',n,'_',k,'.csv', sep = ""),'-o', paste('./depth1_',vpro,'_',n,'_',k,'.csv', sep = ""),'-s', round(0.2*n), '-t 1000 -g -k -b', paste('./forest1_',vpro,'_',n,'_', k,'.bin',sep = "")), wait = TRUE)
  #score1 <- read.csv(paste('./depth1_',vpro,'_',n,'_',k,'.csv', sep = ""), header = TRUE)
  #datab <- as.numeric(unlist(score1))
  #system(paste('rm','-f',paste('./depth1_',vpro,'_',n,'_',k,'.csv', sep = "")))
  ################ try doing cross validation
  
  ### run the mixture data set through the forest  
  #system(paste('./iforest','-i', paste('./data2_',vpro,'_',n,'_',k,'.csv', sep = "") ,'-o',paste('./depth2_',vpro,'_',n,'_',k,'.csv', sep = ""),'-g -f', paste('./forest1_',vpro,'_',n,'_', k,'.bin',sep = "")), wait = TRUE)
  #score2 <- read.csv(paste('./depth2_',vpro,'_',n,'_',k,'.csv', sep = ""), header = TRUE)
  #datan <- as.numeric(unlist(score2))
  #system(paste('rm','-f',paste('./depth2_',vpro,'_',n,'_',k,'.csv', sep = "")))
  
  ################# the next part we need to change
  
  ### remove the forest stored in the directory
  #system(paste('rm','-f',file = paste('./forest1_',vpro,'_',n,'_', k,'.bin',sep = ""), row.names = FALSE))
  
  #system(paste('rm','-f',file = paste("data1_",vpro,"_",n,"_", k, ".csv", sep = ""), row.names = FALSE))
  #system(paste('rm','-f',file = paste("data2_",vpro,"_",n,"_", k, ".csv", sep = ""), row.names = FALSE))
  

  ## %%% part 3: get alpha estimates
  
  ## run c_patrasen first before run true alpha to get probs
  
  ## c_patrasen
  
  
  dat = rbind(data1, data2)
  dat$cl <- as.factor(c(rep("labeled",nrow(data1)), rep("unlabeled",nrow(data2))))
  rf.fit <- randomForest(cl~.,data=dat, ntree=1000)
  rf.preds <- predict(rf.fit,type='prob')
  preds <- rf.preds[,1] # probs for being clean
  lu <- rep(0,length(preds))
  lu[dat$cl=="labeled"] <- 1
  p0 = preds[lu==0] # mixture
  p1 = preds[lu==1] # clean
  p0 = as.numeric(unlist(p0)) # mixture
  p1 = as.numeric(unlist(p1)) # clean
  
  r_index = r_index + 1
  
  est_c_patrasen = method_c_patrasen(p0, p1) ## c-patra/sen
  
  output <- cv_result(1 - p1, 1 - p0, na, est_c_patrasen, q)
  col_data_name[r_index] = data_name
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "c_patrasen"
  col_est_alpha[r_index] = est_c_patrasen
  col_recall[r_index] = output[1]
  col_FPR[r_index] = output[2] 
  
  
  
  
  
  ## use true alpha
  r_index = r_index + 1
  output <- cv_result(1 - p1, 1 - p0, na, alpha, q)
  col_data_name[r_index] = data_name
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "truth"
  col_est_alpha[r_index] = alpha
  col_recall[r_index] = output[1]
  col_FPR[r_index] = output[2]
  
  
  ## source_python('python_file_for_sourcing/KM.py')
  ##use_condaenv("base")
  ##use_python("/usr/bin/python")
  
  ## insert the alpha estimation part
  ## bt_patrasen
  ## c_patrasen
  ## c_roc
  ## roc
  ## spy
  
  ## bt_patrasen
  r_index = r_index + 1
  
  grid_num = 200
  boot_num = 100
  est_bt_patrasen = bt_patrasen(1 - p1, 1 - p0, grid_num = grid_num, boot_num = boot_num)
    
  output <- cv_result(1 - p1, 1 - p0, na, est_bt_patrasen, q)
  col_data_name[r_index] = data_name
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "bt_patrasen"
  col_est_alpha[r_index] = est_bt_patrasen
  col_recall[r_index] = output[1]
  col_FPR[r_index] = output[2]  
  
  


  
  ## c_roc
  r_index = r_index + 1
  
  est_c_roc = method_c_roc(p0, p1) ## c-roc
  
  output <- cv_result(1 - p1,  1 - p0, na, est_c_roc, q)
  col_data_name[r_index] = data_name
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "c_roc"
  col_est_alpha[r_index] = est_c_roc
  col_recall[r_index] = output[1]
  col_FPR[r_index] = output[2] 
  
  # need to modify this part to adapt to 9 dimensional normal data
  # need to double check why want to remove one dimension ?????????????????
  #est_spy = method_spy(x0, x1, cl) ## spy
  
  
  
  
  ## roc
  r_index = r_index + 1
  
  result_est_roc = method_roc(data2, data1) ## ked
  est_roc = result_est_roc$alpha0
  yhat_roc = result_est_roc$yhat
    
  #output <- cv_result(datab, datan, na, est_roc, epsilon)
  # the following line is under the assumption that clean size = mixture size, and the method_roc has mixture before clean in yhat
  output <- cv_result(1 - yhat_roc[(n+1):(2*n)], 1 - yhat_roc[1:n], na, est_roc, q)
  col_data_name[r_index] = data_name
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "roc"
  col_est_alpha[r_index] = est_roc
  col_recall[r_index] = output[1]
  col_FPR[r_index] = output[2]   
  
  
  
  
  ## spy method
  r_index = r_index + 1
  
  est_spy = method_spy(data2, data1)
  
  #output <- cv_result(datab, datan, na, est_spy, epsilon)
  # directly use the previous random forest result, instead of modifying spy method and return predicted probability
  output <- cv_result(1 - p1, 1 - p0, na, est_spy, q)
  col_data_name[r_index] = data_name
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "spy"
  col_est_alpha[r_index] = est_spy
  col_recall[r_index] = output[1]
  col_FPR[r_index] = output[2]  
  
  

  ### datab: anomlay scores for the second clean data set
  ### datan: anomaly scores for the first mixture data set
  ### alpha: anomaly proportion
  ### epsilon: which quantile of the anomaly score of anomlay points are we focusing on
  ### nboots: number of bootstraps
  
  ## insert the calculation for alpha here
  ##output <- cv_result(datab, datan, alpha, epsilon)
  ## recall, FPR
  
  
  
  }
  }
  
  
  df_output = data.frame("data_name" = col_data_name, 
                         "alpha" = col_alpha, 
                         "est_name" = col_est_name,
                         "est_alpha" = col_est_alpha, 
                         "recall" = col_recall,
                         "FPR" = col_FPR)
  
  write.csv(df_output, file = paste("probs_five_methods_alpha_10_results/int_probs_five_methods_alpha_",vpro,"_", k,".csv", sep = ""),row.names = FALSE)  ### write the result into .csv file
}


args = commandArgs(trailingOnly = TRUE)
k = as.numeric(args[1])
evaluation(k)







## 


evaluation <- function(h){ 
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
  
  epsilon <- 0.05  # target on 5% quantile
  
  ## %%% part 0: basic settings
  name_vec = c("landsat", "LR", "OCR", "pageb", "shuttle", "covertype")
  #name_vec = c("landsat", "LR")
  alpha_vec = c(0.10, 0.20, 0.40)
  #alpha_vec = c(0.10, 0.20)
  
  
  n_name = length(name_vec)
  n_alpha = length(alpha_vec) # number of different alpha values
  n_est = 1 + 5 # 1 count for true value of alpha, 9 counts for the nine different estimators
  
  ## create vectors to hold results
  ## the final data frame should contain: data_name, alpha, est_alpha, est_name, recall, FPR
  ## write one file out each time for one alpha, so set the middle item to be 1
  total_row = n_name * 1 * n_est

  


  
  
  for (s in 1:n_alpha){
  r_index = 0 # prepare for starting from filling row 1  
  alpha = alpha_vec[s] 
  vpro = round(alpha * 100)
  
  col_data_name = rep("", total_row)
  col_alpha = rep(0, total_row)
  col_est_name = rep("", total_row)
  col_est_alpha = rep(0, total_row) 
  col_recall = rep(0, total_row)
  col_FPR = rep(0, total_row)
  
  for (i in 1:n_name){
  ## %%% part 1: generate data sets
  data_name = name_vec[i]
  

  
  set.seed(random_seeds[h])
  data_input = generate_data_set(data_name, alpha)
  data1 = data_input$clean
  data2 = data_input$mixture
  na = data_input$na
  o_col = data_input$o_col
  known_classes = data_input$known_classes
  
  #n = nrow(data1)
  ## %%% part 2: get anomaly scores
  
  ## write data sets out 

  
  
  ## use the classwise anomaly score
  ## first try the out-of-bag (on the forest from own class) + regular (on the forests from other classses) way of getting anomaly score for points in clean data set
  
  ## want to construct a data frame
  ## such that each row correspponds to one point
  ## each column corresponds to one class
  
  
  ##for clean data set
  ##plan to do
  ##for every class, build a matrix inside datab_t_t
  ##each matrix will have (nrow, ncol) = (nrow(curr_class_j), n_classes)
  ##there will be n_classses matrixes inside datab_t_t
  datab_t_t =c()
  
  n_test=nrow(data2)
  n_classes=length(known_classes)
  
  datan_t=data.frame(matrix(ncol = n_classes, nrow = n_test))

  n_each_class = rep(0, n_classes)
  
  for  (j in 1:n_classes){
    #n=nrow(data_known_train[[j]])#for iforest file naming.
    
    curr_data = data1[which(data1[, o_col] == known_classes[j]), ]
    #curr_data = data_known_train[[j]]
    curr_data = curr_data[-o_col]
    n = nrow(curr_data)
    n_each_class[j] = n
    write.csv(curr_data, file = paste("data1_",vpro,"_",n,"_", j, "_", h, ".csv", sep = ""), row.names = FALSE)
    
    
    system(paste('./iforest','-i', paste('./data1_',vpro,'_',n,'_', j, "_", h, '.csv', sep = ""),'-o', paste('./depth1_',vpro,'_',n,'_',j,"_", h, '.csv', sep = ""),'-s', round(0.2*n), '-t 1000 -g -k -b',paste('./forest1_',vpro,'_', n, '_', j,"_", h, '.bin',sep = "")), wait = TRUE)
    score1 <- read.csv(paste('./depth1_',vpro,'_',n,'_',j,"_", h, '.csv', sep = ""), header = TRUE)
    datab_t <- as.numeric(unlist(score1))
    system(paste('rm','-f',paste('./depth1_',vpro,'_',n,'_',j,"_", h, '.csv', sep = "")))
    #datab=c(datab,datab_t)
    datab_t_t[[j]] = matrix(nrow = n, ncol = n_classes)
    datab_t_t[[j]][, j] = datab_t
    
    #system(paste('rm','-f',file = paste("data1_unlabel_",vpro,"_",n,"_", k, "_", h, ".csv", sep = ""), row.names = FALSE))
  }
  
  datab = c()
  
  for (j in 1:n_classes){
    n_j = n_each_class[j]
    for (k in 1:n_classes){
      if (j != k){
        n_k = n_each_class[k]
        
        system(paste('./iforest','-i', paste('./data1_',vpro,'_',n_j,'_',j,"_", h, '.csv', sep = "") ,'-o',paste('./depth1_',vpro,'_',n_j,'_',j, "_", k,"_", h, '.csv', sep = ""),'-g -f', paste('./forest1_',vpro,'_', n_k, "_", k,"_", h, '.bin',sep = "")), wait = TRUE)
        score1 <- read.csv(paste('./depth1_',vpro,'_',n_j,'_',j, "_", k,"_", h, '.csv', sep = ""), header = TRUE)
        datab_t <- as.numeric(unlist(score1))
        system(paste('rm','-f',paste('./depth1_',vpro,'_',n_j,'_', j, "_", k,"_", h, '.csv', sep = "")))
        datab_t_t[[j]][, k] = datab_t
      }
    }
    system(paste('rm','-f',paste("data1_",vpro,"_",n_j,"_", j, "_", h, ".csv", sep = "")))
    datab = c(datab, apply(datab_t_t[[j]], 1, min))
  }
  
  data1 = data1[-o_col]  # this step is for later use in c_patrasen, c_roc, roc, spy
  
  data2 = data2[-o_col]
  write.csv(data2, file = paste("data2_",vpro,"_",n_test, "_", h, ".csv", sep = ""), row.names = FALSE)
  
  for  (j in 1:n_classes){
    n = n_each_class[j]
    system(paste('./iforest','-i', paste("data2_",vpro,"_",n_test, "_", h, ".csv", sep = "") ,'-o',paste('./depth2_',vpro,'_',n_test,'_',j,"_", h, '.csv', sep = ""),'-g -f', paste('./forest1_',vpro,'_', n, "_", j,"_", h, '.bin',sep = "")), wait = TRUE)
    score1 <- read.csv(paste('./depth2_',vpro,'_',n_test,'_',j,"_", h, '.csv', sep = ""), header = TRUE)
    datan_t1 <- as.numeric(unlist(score1))
    system(paste('rm','-f',paste('./depth2_',vpro,'_',n_test,'_',j,"_", h, '.csv', sep = "")))
    datan_t[[j]]=datan_t1
    
    system(paste('rm', '-f', paste('./forest1_',vpro,'_', n,'_', j,"_", h, '.bin',sep = "")))
  }
  datan=apply(datan_t,1,FUN=min)
  
  system(paste('rm','-f', paste("data2_",vpro,"_",n_test, "_", h, ".csv", sep = "")))
  
  
  ## then try the total blackbox way of getting anomaly score for points in clean data set
  

  ## %%% part 3: get alpha estimates
  
  
  ## use true alpha
  r_index = r_index + 1
  output <- cv_result(datab, datan, na, alpha, epsilon)
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
  est_bt_patrasen = bt_patrasen(datab, datan, grid_num = grid_num, boot_num = boot_num)
    
  output <- cv_result(datab, datan, na, est_bt_patrasen, epsilon)
  col_data_name[r_index] = data_name
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "bt_patrasen"
  col_est_alpha[r_index] = est_bt_patrasen
  col_recall[r_index] = output[1]
  col_FPR[r_index] = output[2]  
  
  
  ## c_patrasen

  
  dat = rbind(data1, data2)
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
  
  output <- cv_result(datab, datan, na, est_c_patrasen, epsilon)
  col_data_name[r_index] = data_name
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "c_patrasen"
  col_est_alpha[r_index] = est_c_patrasen
  col_recall[r_index] = output[1]
  col_FPR[r_index] = output[2] 

  
  ## c_roc
  r_index = r_index + 1
  
  est_c_roc = method_c_roc(p0, p1) ## c-roc
  
  output <- cv_result(datab, datan, na, est_c_roc, epsilon)
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
  
  est_roc = method_roc(data2, data1) ## ked
  
  output <- cv_result(datab, datan, na, est_roc$alpha0, epsilon)
  col_data_name[r_index] = data_name
  col_alpha[r_index] = alpha
  col_est_name[r_index] = "roc"
  col_est_alpha[r_index] = est_roc$alpha0
  col_recall[r_index] = output[1]
  col_FPR[r_index] = output[2]   
  
  
  
  
  ## spy method
  r_index = r_index + 1
  
  est_spy = method_spy(data2, data1)
  
  output <- cv_result(datab, datan, na, est_spy, epsilon)
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
  df_output = data.frame("data_name" = col_data_name, 
                         "alpha" = col_alpha, 
                         "est_name" = col_est_name,
                         "est_alpha" = col_est_alpha, 
                         "recall" = col_recall,
                         "FPR" = col_FPR)
  
  write.csv(df_output, file = paste("five_methods_alpha_", vpro, "_results/int_five_methods_alpha_",vpro,"_", h,".csv", sep = ""),row.names = FALSE)  ### write the result into .csv file  
  }
}


args = commandArgs(trailingOnly = TRUE)
h = as.numeric(args[1])
evaluation(h)



## this file is for rerunning the experiments of using alpha' instead of alpha


## follow the same upper bound levels as before
## over_alpha = c(0, 0.002, 0.004, 0.006, 0.008, 0.01)

## keep only true alpha setting



evaluation <- function(h){ 
  ### out of bag edition of isolation forest, only need one clean data set
  ### cross validation for getting final results, only need one mixture data set

  source("R_file_for_sourcing/generate_data_set.R")
  source("R_file_for_sourcing/out_of_bag_recall_FPR.R")
  
  ## the cv_result function requires the anomaly points to be put before the nominal points in the mixture, 
  ## for the convenience of computing recall and FPR
  
  
  random_seeds = as.numeric(unlist(read.csv("random_seeds.csv", header = TRUE)))
  
  q_vec <- c(0.02, 0.05, 0.10)  # aim at different quantiles
  ## leave it here for now
  ## check how the loops are done over different alphas and q
  ## how about writing everything out into one file, for each h
  
  ## %%% part 0: basic settings
  name_vec = c("landsat", "LR", "OCR", "pageb", "shuttle", "covertype", "mnist")
  alpha_vec = c(0.10, 0.20, 0.40)
  over_alpha = c(0, 0.002, 0.004, 0.006, 0.008, 0.01)
  
  n_q = length(q_vec)
  n_name = length(name_vec)
  n_alpha = length(alpha_vec) # number of different true alpha values
  n_est = length(over_alpha) # how many different values of alpha primes to use, including the true alpha
  
  ## create vectors to hold results
  ## the final data frame should contain: data_name, alpha, est_alpha, recall, FPR
  ## write one file out each time for one alpha, so set the middle item to be 1
  total_row = n_name * 1 * n_est * n_q


  
  for (s in 1:n_alpha){
  r_index = 0 # prepare for starting from filling row 1  
  alpha = alpha_vec[s] 
  vpro = round(alpha * 100)
  
  col_data_name = rep("", total_row)
  col_q = rep(0, total_row)
  col_alpha = rep(0, total_row)
  #col_est_name = rep("", total_row)
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


  ## %%% part 3: get recall & FPR from multiple values of 
  
  for (t in 1:length(over_alpha)){
    for (q_ind in 1:n_q){
      q = q_vec[q_ind]
      r_index = r_index + 1
      curr_alpha = alpha + over_alpha[t]
      output <- cv_result(datab, datan, na, curr_alpha, q)
      col_data_name[r_index] = data_name
      col_q[r_index] = q
      col_alpha[r_index] = alpha
      col_est_alpha[r_index] = curr_alpha
      col_recall[r_index] = output[1]
      col_FPR[r_index] = output[2]
    }
  }
  

  
  }
  df_output = data.frame("data_name" = col_data_name, 
                         "q" = col_q,
                         "alpha" = col_alpha, 
                         "est_alpha" = col_est_alpha, 
                         "recall" = col_recall,
                         "FPR" = col_FPR)
  
  write.csv(df_output, file = paste("over_alpha_full_", vpro, "_results/over_alpha_full_",vpro,"_", h,".csv", sep = ""),row.names = FALSE)  ### write the result into .csv file  
  }
}


args = commandArgs(trailingOnly = TRUE)
h = as.numeric(args[1])
evaluation(h)

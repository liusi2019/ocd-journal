

## this file is for rerunning the experiments of using alpha' instead of alpha


## follow the same upper bound levels as before
## over_alpha = c(0, 0.002, 0.004, 0.006, 0.008, 0.01)

## keep only true alpha setting



evaluation <- function(h){ 

  source("R_file_for_sourcing/out_of_bag_recall_FPR.R")


  random_seeds = as.numeric(unlist(read.csv("random_seeds.csv", header = TRUE)))
  
  q_vec = c(0.02, 0.05, 0.10)  # aim at different quantiles
  name_vec = c( "tinyimagenet")
  alpha_vec = c(0.10, 0.20, 0.40)
  over_alpha = c(0, 0.002, 0.004, 0.006, 0.008, 0.01)
  
  n_q = length(q_vec)
  n_name = length(name_vec)
  n_alpha = length(alpha_vec) # number of different true alpha values
  n_est = length(over_alpha) # how many different values of alpha primes to use, including the true alpha
  

  total_row = n_name * 1 * n_est * n_q

  score_clean = as.numeric(unlist(read.csv("benchmark_datasets/data_clean.csv", header = FALSE)))
  score_alien = as.numeric(unlist(read.csv("benchmark_datasets/data_alien.csv", header = FALSE)))
  
  for (s in 1:n_alpha){
  r_index = 0 # prepare for starting from filling row 1  
  alpha = alpha_vec[s] 
  vpro = round(alpha * 100)
  
  col_data_name = rep("", total_row)
  col_q = rep(0, total_row)
  col_alpha = rep(0, total_row)
  col_est_alpha = rep(0, total_row) 
  col_recall = rep(0, total_row)
  col_FPR = rep(0, total_row)
  
  for (i in 1:n_name){

  data_name = name_vec[i]
  
  set.seed(random_seeds[h])
  
  n0_big = length(score_clean) # 10000
  na_big = length(score_alien) # 10000
  n = floor(n0_big/(2 - 0.1)) # hard coded 
  if (na_big < floor(0.40 * n)){
    n = floor(na_big/0.40)
  }
  # n = 5263 for TinyImageNet
  n = min(n, 10000)
  n0_total = floor(n * (2 - alpha))
  na = ceiling(n * alpha)
  nominal_total = score_clean[sample(n0_big, n0_total)]
  anomaly = score_alien[sample(na_big, na)]
  datab = nominal_total[1:n]
  datan = c(anomaly, nominal_total[(n+1):n0_total])

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
  
  write.csv(df_output, file = paste("over_alpha_tinyimagenet_", vpro, "_results/over_alpha_tinyimagenet_",vpro,"_", h,".csv", sep = ""),row.names = FALSE)  ### write the result into .csv file  
  }
}


args = commandArgs(trailingOnly = TRUE)
h = as.numeric(args[1])
evaluation(h)

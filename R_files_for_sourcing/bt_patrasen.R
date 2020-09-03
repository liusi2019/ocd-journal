## CVPR workshop method
## need to borrow some from Sen's paper
## note here we are have an additional adjustment inside get_d_gamma_quartile on line 111 to accomodate to the case when sample size is very small
## this code only try positive grid values, cannot deal with the 0 proportion case

  # write a function to resample from F_alpha_check
  binary_find_interval <- function(F_alpha_check, m, r_value){ # write a helper function to get the correct interval to sample from
    int_bounds = c(0, 0)
    complete_F_alpha_check = c(0, F_alpha_check, 1)
    left = 1
    right = m + 3
    while(left < right){
      mid = floor((left + right)/2)
      if(complete_F_alpha_check[mid] < r_value){
        left = mid + 1
      }else{
        right = mid
      }
    }
    if(complete_F_alpha_check[left] == r_value){# this is the actually index in the complete [0,1] range
      int_bounds[1] = left
    }else{
      int_bounds[1] = left - 1
    }
    left = 1
    right = m + 3
    while(left < right){
      mid = floor((left + right)/2)
      if(complete_F_alpha_check[mid] <= r_value){
        left = mid + 1
      }else{
        right = mid
      }
    }
    if(left > m + 2){
      left = left - 1
    }
    int_bounds[2] = left
    return(int_bounds)
  }
  
  
  sample_F_alpha_check <- function(F_alpha_check, m_a, S_m_ordered){
    r_uniform = runif(m_a)
    r_gen = rep(0, m_a)
    start_index = 1
    m = length(S_m_ordered)
    for(i in 1:m_a){
      r_value = r_uniform[i]
      complete_S_m_ordered = c(0, S_m_ordered, 1)
      int_bounds = binary_find_interval(F_alpha_check, m, r_value)
      r_gen[i] = runif(1, min = complete_S_m_ordered[int_bounds[1]], max = complete_S_m_ordered[int_bounds[2]])
    }
    return(r_gen)
  }
  
  
  # write a function to calculate d(lambda) based on the two resamples, one from S_0 and one from F_alpha_check
  
  
  calc_d_gamma_boot <- function(S_0_resample, S_a_resample, F_0, n_m, grid_value){
    mixture = c(S_0_resample, S_a_resample)
    mixture = sort(mixture)
    F_m = ecdf(mixture)
    F_m_values = F_m(mixture)
    F_0_values = F_0(mixture)
    
    #d_gamma_vec = rep(0, grid_num)
    #grid_vec = (1:grid_num)/grid_num
    #for (i in 1:grid_num){
    #grid_value = grid_vec[i]
    F_hat_gamma = (F_m_values - (1 - grid_value)*F_0_values)/grid_value
    #F_is_gamma = pava(F_hat_gamma, rep(1/n_m, n_m), decreasing=FALSE) ## Computes the Isotonic Estimator of F_s
    F_is_gamma = isoreg(F_hat_gamma)$yf
    F_is_gamma[which(F_is_gamma < 0)] = 0
    F_is_gamma[which(F_is_gamma > 1)] = 1
    d_gamma_value <- grid_value*sqrt(t((F_hat_gamma - F_is_gamma)^2) %*% rep(1/n_m, n_m))
    #}
    return(d_gamma_value)
  }
  
  
  get_d_gamma_quartile <- function(S_0, S_m, grid_num = 10, boot_num = 100){
    # for every value of grid, need to calculate two things
    # first the value of d(gamma)
    # then the quartile of d(gamma) under bootstrap
    d_gamma_vec = rep(0, grid_num)
    d_gamma_upper = rep(0, grid_num)
    d_gamma_lower = rep(0, grid_num)
    # F_0 is used everywhere, can be calculated in advance to avoid repeating
    S_0 = sort(S_0)
    F_0 = ecdf(S_0)
    n_m = length(S_m)
    S_m = sort(S_m)
    F_m = ecdf(S_m)
    F_m_values = F_m(S_m)
    F_0_values = F_0(S_m)
    
    for(i in 1:grid_num){
      grid_value = i/grid_num
      F_hat_gamma = (F_m_values - (1 - grid_value)*F_0_values)/grid_value
      #F_is_gamma = pava(F_hat_gamma, rep(1/n_m, n_m), decreasing=FALSE) ## Computes the Isotonic Estimator of F_s
      F_is_gamma = isoreg(F_hat_gamma)$yf
      F_is_gamma[which(F_is_gamma < 0)] = 0
      F_is_gamma[which(F_is_gamma > 1)] = 1
      d_gamma_vec[i] <- grid_value*sqrt(t((F_hat_gamma - F_is_gamma)^2) %*% rep(1/n_m, n_m)) # calculate d_gamma_vec[i]
      # move on to calculate 1st and 3rd quartiles on the boostrap results
      
      # first get both resamples
      m_a = round(n_m * grid_value) #rounding in R is weird on .5
      m_a = max(m_a, 1) # ** additional adjustment to the case when sample size is very small, and original m_a runs to 0
      m_0 = n_m - m_a
      
      # calculate the 1st and 3rd quartiles
      d_gamma_boots = rep(0, boot_num)
      for (j in 1:boot_num){
        resample_0 = sample(S_0, m_0, replace = TRUE)
        resample_a = sample_F_alpha_check(F_is_gamma, m_a, S_m)      
        d_gamma_boots[j] = calc_d_gamma_boot(resample_0, resample_a, F_0, n_m, grid_value)
      }
      quan = quantile(d_gamma_boots, c(0.25, 0.75))
      
      d_gamma_upper[i] = quan[2]
      d_gamma_lower[i] = quan[1]
    }
    
    return(list(d_gamma_vec = d_gamma_vec, d_gamma_upper = d_gamma_upper, d_gamma_lower = d_gamma_lower))
  }
  
  
  bt_patrasen <- function(S_0, S_m, grid_num = 200, boot_num = 100){
  
    df = get_d_gamma_quartile(S_0, S_m, grid_num = grid_num, boot_num = boot_num)
    df$grid = (1:grid_num)/grid_num
    
    df_result = data.frame("grid" = df$grid, "d_gamma" = df$d_gamma_vec, "d_gamma_1st" = df$d_gamma_lower, "d_gamma_3rd" = df$d_gamma_upper)
    
    estimate = 1
    for (i in 1:grid_num){
      if ((df_result$d_gamma_1st[i] <= df_result$d_gamma[i]) & (df_result$d_gamma_3rd[i] >= df_result$d_gamma[i])){
        estimate = i/grid_num
        break  
      }
    }
    
    return(estimate)
  }
  








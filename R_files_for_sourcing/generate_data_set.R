## function generate_data_set provide the data sets according to the data_name and alpha inputs
## *** notice that the current version fits for alpha = 0.10, 0.20, 0.40, not necessarily works for other values
## it contains hard-coded part




## need to add the part to take into consideration the possible way of getting the largest 
## clean and mixture datasets

# consider alpha = 0.10, 0.20, 0.40

# the constructing data set part contains hard-coded min (0.10) and max (0.40) of alpha_vec
# better change it if want to run experiments with other alpha value later


#data_name = "landsat"


#alpha_vec = c(0.10, 0.20, 0.40)


generate_data_set <- function(data_name, alpha){

if (data_name == "landsat"){
  data_trn=read.csv('benchmark_datasets/sat.trn',sep="", header = FALSE)
  data_tst=read.csv('benchmark_datasets/sat.tst',sep="", header = FALSE)
  names(data_tst)=names(data_trn)
  data=rbind(data_trn,data_tst)
  n_col=ncol(data)
  o_col=n_col
  known_classes=c(1,7)
  big_nominal = subset(data,data[[o_col]] %in% known_classes)
  #nominal = big_nominal[sample(nrow(big_nominal), round(n * (1 - vpro[j]))), ]
  big_anomaly = subset(data, !data[[o_col]] %in% known_classes)
  n0_big = nrow(big_nominal) # 3041
  na_big = nrow(big_anomaly) # 3394
  n = floor(n0_big/(2 - 0.1)) # hard coded 
  if (na_big < floor(0.40 * n)){
    n = floor(na_big/0.40)
  }
  #  n = 1600 for landsat
  n = min(n, 10000)
  n0_total = floor(n * (2 - alpha))
  na = ceiling(n * alpha)
  nominal_total = big_nominal[sample(nrow(big_nominal), n0_total), ]
  anomaly = big_anomaly[sample(nrow(big_anomaly), na), ]
  data_clean = nominal_total[1:n, ]
  data_mixture = rbind(anomaly, nominal_total[(n+1):n0_total,])
  #data_clean = data_clean[-o_col]
  #data_mixture = data_mixture[-o_col]
  
  
}else if(data_name == "LR"){
  data=read.csv('benchmark_datasets/letter-recognition.data', header = FALSE)
  n_col=ncol(data)
  o_col=1
  data[[1]]=as.numeric(data[[1]])
  #known_classes=c(1,3,5,7,9,11,13,15,17,19,21,23,26,25)
  known_classes=c(1,3) # as stated in icml2018 paper
  big_nominal = subset(data,data[[o_col]] %in% known_classes)
  #nominal = big_nominal[sample(nrow(big_nominal), round(n * (1 - vpro[j]))), ]
  big_anomaly = subset(data, !data[[o_col]] %in% known_classes)
  #anomaly = big_anomaly[sample(nrow(big_anomaly), round(n * vpro[j])), ]
  n0_big = nrow(big_nominal) # 1525
  na_big = nrow(big_anomaly) # 18475
  n = floor(n0_big/(2 - 0.1)) # hard coded 
  if (na_big < floor(0.40 * n)){
    n = floor(na_big/0.40)
  }
  #  n = 802 for LR
  n = min(n, 10000)
  n0_total = floor(n * (2 - alpha))
  na = ceiling(n * alpha)
  nominal_total = big_nominal[sample(nrow(big_nominal), n0_total), ]
  anomaly = big_anomaly[sample(nrow(big_anomaly), na), ]
  data_clean = nominal_total[1:n, ]
  data_mixture = rbind(anomaly, nominal_total[(n+1):n0_total,])
  #data_clean = data_clean[-o_col]
  #data_mixture = data_mixture[-o_col]

}else if(data_name == "OCR"){
  data_trn=read.csv('benchmark_datasets/optdigits.tra', header = FALSE)
  data_tes=read.csv('benchmark_datasets/optdigits.tes', header = FALSE)
  names(data_tes)=names(data_trn)
  data=rbind(data_trn,data_tes)
  n_col=ncol(data)
  o_col=n_col
  known_classes=c(1,3,4,5,7)
  big_nominal = subset(data,data[[o_col]] %in% known_classes)
  #nominal = big_nominal[sample(nrow(big_nominal), round(n * (1 - vpro[j]))), ]
  big_anomaly = subset(data, !data[[o_col]] %in% known_classes)
  #anomaly = big_anomaly[sample(nrow(big_anomaly), round(n * vpro[j])), ]
  n0_big = nrow(big_nominal) # 2835
  na_big = nrow(big_anomaly) # 2785
  n = floor(n0_big/(2 - 0.1)) # hard coded 
  if (na_big < floor(0.40 * n)){
    n = floor(na_big/0.40)
  }
  #  n = 1492 for OCR
  n = min(n, 10000)
  n0_total = floor(n * (2 - alpha))
  na = ceiling(n * alpha)
  nominal_total = big_nominal[sample(nrow(big_nominal), n0_total), ]
  anomaly = big_anomaly[sample(nrow(big_anomaly), na), ]
  data_clean = nominal_total[1:n, ]
  data_mixture = rbind(anomaly, nominal_total[(n+1):n0_total,])
  #data_clean = data_clean[-o_col]
  #data_mixture = data_mixture[-o_col]

}else if(data_name == "pageb"){
  data=read.csv("benchmark_datasets/page-blocks.data",sep="", header = FALSE)
  n_col=ncol(data)
  o_col=n_col
  known_classes=c(1,5)
  big_nominal = subset(data,data[[o_col]] %in% known_classes)
  #nominal = big_nominal[sample(nrow(big_nominal), round(n * (1 - vpro[j]))), ]
  big_anomaly = subset(data, !data[[o_col]] %in% known_classes)
  #anomaly = big_anomaly[sample(nrow(big_anomaly), round(n * vpro[j])), ]
  n0_big = nrow(big_nominal) # 5028
  na_big = nrow(big_anomaly) # 445
  n = floor(n0_big/(2 - 0.1)) # hard coded 
  if (na_big < floor(0.40 * n)){
    n = floor(na_big/0.40)
  }
  #  n = 1112 for pageb
  n = min(n, 10000)
  n0_total = floor(n * (2 - alpha))
  na = ceiling(n * alpha)
  nominal_total = big_nominal[sample(nrow(big_nominal), n0_total), ]
  anomaly = big_anomaly[sample(nrow(big_anomaly), na), ]
  data_clean = nominal_total[1:n, ]
  data_mixture = rbind(anomaly, nominal_total[(n+1):n0_total,])
  #data_clean = data_clean[-o_col]
  #data_mixture = data_mixture[-o_col]
  
}else if(data_name == "shuttle"){
  data_trn=read.csv('benchmark_datasets/shuttle.trn',sep="", header = FALSE)
  data_tst=read.csv('benchmark_datasets/shuttle.tst',sep="", header = FALSE)
  names(data_tst)=names(data_trn)
  data=rbind(data_trn,data_tst)
  o_col=ncol(data)#output class column
  ##known_class=c(1)
  known_classes=c(1,4)
  big_nominal = subset(data,data[[o_col]] %in% known_classes)
  #nominal = big_nominal[sample(nrow(big_nominal), round(n * (1 - vpro[j]))), ]
  big_anomaly = subset(data, !data[[o_col]] %in% known_classes)
  #anomaly = big_anomaly[sample(nrow(big_anomaly), round(n * vpro[j])), ]
  n0_big = nrow(big_nominal) # 54489
  na_big = nrow(big_anomaly) # 3511
  n = floor(n0_big/(2 - 0.1)) # hard coded 
  if (na_big < floor(0.40 * n)){
    n = floor(na_big/0.40)
  }
  #  n = 8777 for shuttle
  n = min(n, 10000)
  n0_total = floor(n * (2 - alpha))
  na = ceiling(n * alpha)
  nominal_total = big_nominal[sample(nrow(big_nominal), n0_total), ]
  anomaly = big_anomaly[sample(nrow(big_anomaly), na), ]
  data_clean = nominal_total[1:n, ]
  data_mixture = rbind(anomaly, nominal_total[(n+1):n0_total,])
  #data_clean = data_clean[-o_col]
  #data_mixture = data_mixture[-o_col]

}else if(data_name == "covertype"){
  ## only keep the numerical features
  data=read.csv("benchmark_datasets/covtype.data",sep=',', header= FALSE)
  o_col=ncol(data)#output class column
  known_classes=c(1,2, 3, 7)
  big_nominal = subset(data,data[[o_col]] %in% known_classes)
  #nominal = big_nominal[sample(nrow(big_nominal), round(n * (1 - vpro[j]))), ]
  big_anomaly = subset(data, !data[[o_col]] %in% known_classes)
  #anomaly = big_anomaly[sample(nrow(big_anomaly), round(n * vpro[j])), ]
  n0_big = nrow(big_nominal) # 551405
  na_big = nrow(big_anomaly) # 29607
  n = floor(n0_big/(2 - 0.1)) # hard coded 
  if (na_big < floor(0.40 * n)){
    n = floor(na_big/0.40)
  }
  #  n = 74017 for covertype
  n = min(n, 10000) # force the sample size to be no greater than 20000, for computing consideration
  n0_total = floor(n * (2 - alpha))
  na = ceiling(n * alpha)
  nominal_total = big_nominal[sample(nrow(big_nominal), n0_total), ]
  anomaly = big_anomaly[sample(nrow(big_anomaly), na), ]
  data_clean = nominal_total[1:n, ]
  data_mixture = rbind(anomaly, nominal_total[(n+1):n0_total,])
  #data_clean = data_clean[-o_col]
  #data_mixture = data_mixture[-o_col]
  data_clean = data_clean[, c(1:10, 55)]
  data_mixture = data_mixture[, c(1:10, 55)]
  o_col = 11
  
}
  output = list()
  output$clean = data_clean
  output$mixture = data_mixture
  output$na = na
  output$o_col = o_col
  output$known_classes = known_classes
  return(output)
}


#vector_name = c("landsat", "LR", "OCR", "pageb", "shuttle", "covertype")

#for (r in 1:length(vector_name)){
#  data_name = vector_name[r]
#  rseed <- as.numeric(unlist(read.csv("general_random_seed.csv", header = FALSE)))
#  set.seed(rseed[a])
#}
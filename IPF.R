################## IPF

# requiero estados observables en 0 pero no en 1

IPF_f = function(N0, N1){

  library('mipfp')
  seed <- matrix(0, 120, 120)
  for (i in seq(1,120,3)){
    # i=1
    seed[i:(i+2),i:(i+2)] = matrix(c(1,1,1,
                                    0,1,1,
                                    0,0,1), ncol= 3, byrow = T)}
  target.row <- N0 ; sum(target.row)
  target.col <- N1 ; sum(target.col)
  target.data <- list(target.row, target.col)
  target.list <- list(1, 2)
  r.ipfp <- Estimate(seed, target.list, target.data,  method = "ipfp",  iter = 5000)
  #r.ml <- Estimate(seed, target.list, target.data, method = "ml")
  #r.chi2 <- Estimate(seed, target.list, target.data, method = "chi2")
  #r.lsq <- Estimate(seed, target.list, target.data, method = "lsq")
  # true.table = Q
  # true.table[seq(3,150,3),] = 0
  # true.table = true.table  * target.row
  #CompareMaxDev(list(r.ipfp), echo = TRUE, true.table=true.table)
  
  # in probabilities
  #(r.ipfp$x.hat/target.row)/Q

  library(pracma)
  
  # class(r.ipfp$x.hat)
  # class(target.row)
  # dim(r.ipfp$x.hat)
  # dim(target.row)
  # r.ipfp$x.hat / as.numeric(target.row)
  # 0.9722406/0.97625550
  
  p_hnh.hat.ipf = Diag(r.ipfp$x.hat / as.numeric(target.row), 1)[seq(1,120,3)]
  p_hd.hat.ipf = Diag(r.ipfp$x.hat / as.numeric(target.row),2)[seq(1,120,3)] 
  p_nhd.hat.ipf = Diag(r.ipfp$x.hat / as.numeric(target.row),1)[seq(2,120,3)] 
  N1_nh.hat.ipf = N$N0_nh * (1-p_nhd.hat.ipf) + N$N0_h * p_hnh.hat.ipf
  
  out = data.frame(p_hnh.hat.ipf, p_hd.hat.ipf, p_nhd.hat.ipf, N1_nh.hat.ipf)
  # https://cran.r-project.org/web/packages/gllm/gllm.pdf
  # https://stat.ethz.ch/R-manual/R-devel/library/stats/html/loglin.html
  # Bayesian IPF
  
  return(out)
}

# IPF_f(N0, N1)



# ###### edad x
# library('mipfp')
# x = 30
# seed <- matrix(c(1,1,1,0,1,1), 2, 3, T)
# target.row <- N0[N0[,2] == x, 1][1:2] ; sum(target.row)
# target.col <- N1[N1[,2] == x, 1] ; sum(target.col)
# target.row %*% Q[1:2,1:3]; target.col
# target.data <- list(target.row, target.col)
# target.list <- list(1, 2)
# r.ipfp <- Estimate(seed, target.list, target.data,  method = "ipfp")
# sum(r.ipfp$x.hat) # cumple objetivo
# r.ml <- Estimate(seed, target.list, target.data, method = "ml")
# r.chi2 <- Estimate(seed, target.list, target.data, method = "chi2")
# r.lsq <- Estimate(seed, target.list, target.data, method = "lsq")
# # target.row %*% rbind(Q[1:2,1:3],c(0,0,0)) - target.col # chek
# true.table = Q[1:2,1:3] * target.row # eventos
# CompareMaxDev(list(r.ipfp,r.ml,r.chi2,r.lsq), echo = TRUE, true.table=true.table)
# 
# # in probabilities
# (r.ipfp$x.hat/target.row)/Q[1:2,1:3]
# (r.lsq$x.hat/target.row)/Q[1:2,1:3]



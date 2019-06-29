############################
256

Transp_f = function(N){
  
  library(lpSolve)
  matr_prob = data.frame()
  
  for (x in c(30:69)){
    # x = 30
    
    f.obj <- c(   1,     2,     0,
                  10000,     2,   0)
    
    prev = N[N$Age == x,]
    
    f.con <- matrix (c(1, 1, 1, 0, 0, 0,
                       0, 0, 0, 1, 1, 1,
                       1, 0, 0, 1, 0, 0,
                       0, 1, 0, 0, 1, 0,
                       0, 0, 1, 0, 0, 1,
                       0, 0, 0, 0, 0, 1,
                       0, 0, 0, 0, 0, 1), #min value
                       nrow=7, byrow=TRUE)
    
    f.dir <- c("=", "=", "=", "=", "=", 
               ">", "<")

    f.rhs <- c(prev$N0_h, prev$N0_nh, 
               prev$N1_h, prev$N1_nh, prev$N1_d, 
               0.001, 0.02) 
    
    solve = lp (direction = "min", objective.in = f.obj,
                const.mat =  f.con, const.dir =  f.dir,
                const.rhs = f.rhs, scale = 256)
    solve$solution
    
    N$p_nhd
    N$p_hd
    
    
    
    
    
    
    matr_prob = rbind(matr_prob, c(x, solve$objval, solve$solution/
                                     c(prev$N0_h, prev$N0_h, prev$N0_h,
                                       prev$N0_nh, prev$N0_nh, prev$N0_nh)))
    
    
  }
  colnames(matr_prob) = c('x', 'error', 'p11', 'p12', 'p13', 'p21', 'p22', 'p23')
  return(matr_prob)
}

Trans_r = Transp_f(N)
plot(30:69, N$p_hnh[1:40],  t = "o", main = 'H->NH', ylim=c(0,0.1), xlab = 'Ages', ylab='Probability', cex = 0.8)
points(30:69, Trans_r$p12, col=2)


plot(30:69, N$p_nhd[1:40], cex = 0.8, main = 'rr', xlab = '', ylab='rr')
points(30:69, Trans_r$p23, col=2)

plot(30:69, N$p_hd[1:40], cex = 0.8, main = 'rr', xlab = '', ylab='rr')
points(30:69, Trans_r$p13, col=2)

N$p_nhd
N$p_hd
N$p_nhd-N$p_hd



Trans_r$p23-Trans_r$p13

Trans_r$p23/Trans_r$p13


# # min
# 0.0004179000*N0[1]
# 0.001159119*N0[2]


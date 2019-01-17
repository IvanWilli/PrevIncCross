################## IPF

# requiero estados observables y mortalidad general

length(N0)
length(N1)

library('mipfp')

###### edad x

x = 75
seed <- matrix(c(1,1,1,0,1,1), 2, 3, T)
target.row <- N0[N0[,2] == x, 1][1:2] ; sum(target.row)
target.col <- N1[N1[,2] == x, 1] ; sum(target.col)
  target.row %*% Q[1:2,1:3]; target.col
target.data <- list(target.row, target.col)
target.list <- list(1, 2)
r.ipfp <- Estimate(seed, target.list, target.data,  method = "ipfp")
  sum(r.ipfp$x.hat) # cumple objetivo
r.ml <- Estimate(seed, target.list, target.data, method = "ml")
r.chi2 <- Estimate(seed, target.list, target.data, method = "chi2")
r.lsq <- Estimate(seed, target.list, target.data, method = "lsq")
# target.row %*% rbind(Q[1:2,1:3],c(0,0,0)) - target.col # chek
true.table = rbind(Q[1:2,1:3]) * target.row # eventos
CompareMaxDev(list(r.ipfp,r.ml,r.chi2,r.lsq), echo = TRUE, true.table=true.table)


library('cat')
m=c(1,2)
thetahat <- ipf(HairEyeColor,margins=m,
                showits=TRUE) # fit model
thetahat <- ipf(HairEyeColor+.5,m) # find an interior starting value
rngseed(1234567) # set random generator seed
theta <- bipf(HairEyeColor,m,
              start=thetahat,prior=0.5,
              steps=50) # take 50 step


# in probabilities
(r.ipfp$x.hat/target.row)/Q[1:2,1:3]
(r.lsq$x.hat/target.row)/Q[1:2,1:3]



###### todas las edades

seed <- matrix(0, 150, 150)
for (i in seq(1,150,3)){
  # i=1
  seed[i:(i+2),i:(i+2)] = matrix(c(1,1,1,
                                  0,1,1,
                                  0,0,1), ncol= 3, byrow = T)}

target.row <- N0[N0[,2] != 80,1] ; sum(target.row)
target.col <- N1[,1] ; sum(target.col)
target.data <- list(target.row, target.col)
target.list <- list(1, 2)
r.ipfp <- Estimate(seed, target.list, target.data,  method = "ipfp",  iter = 5000)
#r.ml <- Estimate(seed, target.list, target.data, method = "ml")
#r.chi2 <- Estimate(seed, target.list, target.data, method = "chi2")
#r.lsq <- Estimate(seed, target.list, target.data, method = "lsq")
true.table = Q
true.table[seq(3,150,3),] = 0
true.table = true.table  * target.row
CompareMaxDev(list(r.ipfp), echo = TRUE, true.table=true.table)

# in probabilities
(r.ipfp$x.hat/target.row)/Q


# graf comparacion

library(pracma)



p_hnh.hat = Diag(r.ipfp$x.hat / target.row, 1)[seq(1,150,3)]
p_hd.hat = Diag(r.ipfp$x.hat / target.row,2)[seq(1,150,3)] 
p_nhd.hat = Diag(r.ipfp$x.hat / target.row,1)[seq(2,150,3)] 


par(mfrow=c(3,1)) 

plot(30:79, p_hnh, cex = 0.8, main = 'H->NH', ylim=c(0,0.12), xlab = '', ylab='Probability')
points(30:79, p_hnh.hat, cex = 0.8, col=2)
# plot(p_hnh.hat/p_hnh, ylim=c(1,1.5)) # siempre sobreestima

plot(30:79, p_hd, cex = 0.8, main = 'H->D', xlab = '', ylab='Probability')
points(30:79, p_hd.hat, cex = 0.8, col=2)
# plot(p_hd.hat/p_hd) # siempre subestima +++

plot(30:79, p_nhd, cex = 0.8, main = 'NH->D', ylim=c(0,0.05), xlab = 'Age', ylab='Probability')
points(30:79, p_nhd.hat, cex = 0.8, col=2) # +++
# plot(p_nhd.hat/p_nhd) # siempre sobreestima +++


# https://cran.r-project.org/web/packages/gllm/gllm.pdf
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/loglin.html
# Bayesian IPF



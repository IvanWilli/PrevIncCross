# simulated data
edades = c("x","x+1","X+2","x+3")

# prev inicial "observable"
Ni_0 = matrix(c(.5,.4,.3,
                .5,.6,.7,
                0, 0, 0), nrow = 3, byrow = T) * 100

# in each row m12, m1d, m2d
M_rates = matrix(c(0.01, 0.02, 0.04,
                   0.02, 0.03, 0.05,
                   0.03, 0.04, 0.06),nrow = 3,byrow = T)

M_01 = array(0,dim=c(3,3,3))
M_01[1,2,] = M_rates[,1]
M_01[1,3,] = M_rates[,2]
M_01[2,3,] = M_rates[,3]
M_01[1,1,] = -M_rates[,1]-M_rates[,2]
M_01[2,2,] = -M_rates[,3]

library(expm)
P_01 = array(0,dim=c(3,3,3))
P_01[,,1] = expm(M_01[,,1])
P_01[,,2] = expm(M_01[,,2])
P_01[,,3] = expm(M_01[,,3])

Ni_1 = cbind(t(Ni_0[,1] %*% P_01[,,1]),
             t(Ni_0[,2] %*% P_01[,,2]),
             t(Ni_0[,3] %*% P_01[,,3]))

Ni_0[,1] %*% P_01[,,1] - Ni_1[,1] # chek
Ni_0[,2] %*% P_01[,,2] - Ni_1[,2] # chek
Ni_0[,3] %*% P_01[,,3] - Ni_1[,3] # chek


################## SCHOEN

library('mipfp')

# 2 D
for (i in 1:3){
  # i = 1
  seed <- matrix(c(1, 1, 1,
                   0, 1, 1), nrow = 2,byrow = T)
  target.row <- Ni_0[,i][-3] ; sum(target.row)
  target.col <- Ni_1[,i] ; sum(target.col)
  target.data <- list(target.row, target.col)
  target.list <- list(1, 2)
  r.ipfp <- Estimate(seed, target.list, target.data,  method = "ipfp")
  r.ml <- Estimate(seed, target.list, target.data, method = "ml")
  r.chi2 <- Estimate(seed, target.list, target.data, method = "chi2")
  r.lsq <- Estimate(seed, target.list, target.data, method = "lsq")
  true.table = Ni_0[-3,1] * P_01[-3,,1]
  CompareMaxDev(list(r.ipfp,r.ml,r.chi2,r.lsq), echo = TRUE, true.table=true.table)
}

# 3 D
# para 3 dim

target.No.N1 = matrix(runif(6),nrow = 3,byrow = T)
target.No.N1 = target.No.N1/sum(target.No.N1)*300
target.No.Age <- Ni_0
target.N1.Age <- Ni_1
target.No.N1 <- # condición sobre transición gral
  target.data <- list(target.row, target.col)
target.list <- list(1, 2)
r.ipfp <- Ipfp(seed, target.list, target.data)

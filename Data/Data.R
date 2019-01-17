#####################################Canada

## raw data
Can_inc = c(0.2,0.3,0.5,0.8,1.3,2,2.8,3.7,4.9,6.3,7.4,8.4,8.8,7.3) / 100
Can_prev = c(0.5,1.6,3.4,5.7,9.3,14.8,22.8,32.9,43.3,53.6,62.6,69.5,74.3,74.6) / 100
Can_m = c(0.5,0.5,0.5,0.7,1.1,1.8,2.7,4.2,6.4,10.3,16.5,28.5,49.2,105.1) / 1000
Can_rr = c(4.2,4,2.7,2.4,2.4,1.9,1.8,1.6,1.5,1.5,1.4,1.2,1.2,1.1)
age_li = c(seq(20,85,5))
age_ls = c(seq(24,84,5),99)
age_avg = (age_li+(age_ls+1))/2
age_group = paste0(age_li,'-',age_ls)
Can = data.frame(age_group, age_li, age_ls, age_avg, Can_prev, Can_inc, Can_m, Can_rr)

## take from 30 to 80
Can = Can[Can$age_li >= 30 & Can$age_li <= 80,]

# graph
g1 = cbind(Can[,-c(6,7,8)], PrInc = 'Prev'); g1$val = g1$Can_prev; g1$Can_prev = NULL
g2 = cbind(Can[,-c(5,7,8)], PrInc = 'Inc'); g2$val = g2$Can_inc; g2$Can_inc = NULL
g3 = cbind(Can[,-c(5,6,8)], PrInc = 'm'); g3$val = g3$Can_m; g3$Can_m = NULL
g4 = cbind(Can[,-c(5,6,7)], PrInc = 'rr'); g4$val = g4$Can_rr; g4$Can_rr = NULL
Can_graph = rbind(g1, g2, g3, g4)

library(ggplot2)
Fig1 = ggplot(data = Can_graph[1:22,]) +
              geom_bar(aes(x = age_group, y = val * 100, fill = PrInc),
                       stat = "identity", position = "dodge") +
                  labs(title = "Figure 1. Hypertension in Canada",
                       subtitle = "2012",
                       caption = "Source: Robitaille et.al. (2012)",
                       x = "Age",
                       y = "%",
                       colour = "Gears") +
                  theme_classic() 


################# EDADES SIMPLES

######smooth splines
ages_pred = 30:80 

par(mfrow=c(2,2)) 
##
plot(Can$age_avg, Can$Can_inc * 100, xlab = 'Age', ylab = 'Incidence (% x year)', xlim=c(30,80))
Can_inc_s = smooth.spline(Can$age_avg, Can$Can_inc)
Can_inc_s = predict(Can_inc_s, x = ages_pred)$y
axis(side=1, at=seq(20, 90, 5))
lines(ages_pred, Can_inc_s * 100, col=2)
legend(70, 4, c("Observed", "Splines"), col = c(1, 2), 
       lty = c(NA, 1), pch = c(1, NA), merge = TRUE, cex = 0.8, bty = "n")
##
Can_prev_s = smooth.spline(Can$age_avg, Can$Can_prev)
Can_prev_s = predict(Can_prev_s, x = ages_pred)$y
plot(ages_pred, Can_prev_s * 100, col=2, t='l',  xlab = 'Age', ylab = 'Prevalence (%)', xlim=c(30,80))
axis(side=2, at=seq(0, 80, 10))
axis(side=1, at=seq(20, 90, 5))
points(Can$age_avg, Can$Can_prev * 100)
legend(70, 30, c("Observed", "Splines"), col = c(1, 2), 
       lty = c(NA, 1), pch = c(1, NA), merge = TRUE, cex = 0.8, bty = "n")
##
plot(Can$age_avg, Can$Can_rr, xlab = 'Age', ylab = 'RR', xlim=c(30,80))
Can_rr_s = smooth.spline(Can$age_avg, Can$Can_rr)
Can_rr_s = predict(Can_rr_s, x = ages_pred)$y
axis(side=1, at=seq(20, 90, 5))
lines(ages_pred, Can_rr_s, col=2)
legend(70, 2.5, c("Observed", "Splines"), col = c(1, 2), 
       lty = c(NA, 1), pch = c(1, NA), merge = TRUE, cex = 0.8, bty = "n")
##
plot(Can$age_avg, log(Can$Can_m), xlab = 'Age', ylab = 'log(m)', xlim=c(30,80), ylim=c(-8, -3))
Can_m_s = smooth.spline(Can$age_avg, Can$Can_m)
Can_m_s = predict(Can_m_s, x = ages_pred)$y
lines(ages_pred, log(Can_m_s), col=2)
Can_mr_s = Can_rr_s * Can_m_s
lines(ages_pred, log(Can_mr_s), col=4)
legend(70, -5, c("Observed", "Healthy", "Not Healthy"), col = c(1, 2, 4), 
       lty = c(NA, 1, 1), pch = c(1, NA, NA), merge = TRUE, cex = 0.8, bty = "n")

mtext("Hypertension in Canada in 2012. Observed and smoothed by spline", side = 3, line = -18, outer = TRUE)
par(mfrow=c(1,1)) 

## simulation population

# transition matrix rate
M = matrix(0, 3*(length(ages_pred)-1), 3*(length(ages_pred)-1))
dim(M)
j = 1
for (i in seq(1,148,3)) {
  M[i,i] = -Can_m_s[j]-Can_inc_s[j]
  M[i,i+1] = Can_inc_s[j]
  M[i,i+2] = Can_m_s[j]
  M[i+1,i+1] = -Can_mr_s[j]
  M[i+1,i+2] = Can_mr_s[j]
  j = j+1
}
sum(rowSums(M))

# transition matrix probability - constant rate for the year
library(expm)
Q = expm(M)
rowSums(Q)

# Poblacion Simulada
# arregl vectorial 30:80

w = length(30:80) 
N0 = matrix(rep(0,w*3))
N0[seq(1,w*3,3)] = 1-Can_prev_s[51]
N0[seq(2,w*3,3)] = Can_prev_s[51]

N1 = t(t(N0[-c((w*3)-2,(w*3)-1,w*3),1]) %*% Q)

dim(Q); length(N0); sum(N0) ; dim(N1); sum(N1) # ok

Age = c() ; for (i in 30:80) {Age = append(Age, rep(i,3))}
N0 = cbind(N0, Age)
N1 = cbind(N1, Age+1)

# arreglo dataframe
library(pracma)
N = data.frame(Age = 30:80,
               N0_h = N0[seq(1,w*3,3),1],
               N0_nh = N0[seq(2,w*3,3),1],
               
               m_hnh = Diag(M,1)[seq(1,w*3,3)],
               m_hd = Diag(M,2)[seq(1,w*3,3)],
               m_nhd = Diag(M,1)[seq(2,w*3,3)],
               
               p_hnh = Diag(Q,1)[seq(1,w*3,3)],
               p_hd = Diag(Q,2)[seq(1,w*3,3)],
               p_nhd = Diag(Q,1)[seq(2,w*3,3)],
               
               N1_h = N1[seq(1,w*3*3,3),1],
               N1_nh = N1[seq(2,w*3*3,3),1],
               N1_d = N1[seq(3,w*3*3,3),1])


# graf
par(mfrow=c(1,1))
plot(N$Age, N$N0_h * 100, ylim=c(0,100), xlab='Age', ylab='%', main = 'Simulated population')
points(N$Age, N$N1_h * 100, col=4)
points(N$Age, N$N0_nh * 100, col=3)
points(N$Age, N$N1_nh * 100, col=2)
legend(35, 60, c("Healthy t", "Healthy t+1", "Not Healthy t", "Not Healthy t+1"), 
       col = c(1, 2, 3, 4), pch = c(1, 1, 1, 1), cex = 0.8, bty = "n")


################# EDADES QUINQUENALES

w = nrow(Can)

# transition matrix rate
Mq = matrix(0, 3*w, 3*w)
dim(Mq)
j = 1
for (i in seq(1,w*3,3)) {
  Mq[i,i] = -Can$Can_inc[j]-Can$Can_m[j]
  Mq[i,i+1] = Can$Can_inc[j]
  Mq[i,i+2] = Can$Can_m[j]
  Mq[i+1,i+1] = -Can$Can_m[j]*Can$Can_rr[j]
  Mq[i+1,i+2] = Can$Can_m[j]*Can$Can_rr[j]
  j = j+1
}
sum(rowSums(Mq))

# transition matrix probability - constant rate for the year
library(expm)
Qq = expm(Mq*5)
rowSums(Qq)

# Poblacion Simulada
# arreglo vectorial

N0q = matrix(rep(0,w*3))
N0q[seq(1,w*3,3)] = 1-Can$Can_prev[1:w]
N0q[seq(2,w*3,3)] = Can$Can_prev[1:w]

N5q = t(t(N0q) %*% Qq)

dim(Qq); dim(N0q); sum(N0q) ; dim(N5q); sum(N5q) # ok

Ageq = c() ; for (i in Can$age_li[1:w]) {Ageq = append(Ageq, rep(i,3))}
N0q = cbind(N0q, Ageq)
N5q = cbind(N5q, Ageq)

# arreglo dataframe
library(pracma)

Nq = data.frame(Age = Can$age_li[1:w],
               N0_h = N0q[seq(1,w*3,3),1],
               N0_nh = N0q[seq(2,w*3,3),1],
               
               m_hnh = Diag(Mq,1)[seq(1,w*3,3)],
               m_hd = Diag(Mq,2)[seq(1,w*3,3)],
               m_nhd = Diag(Mq,1)[seq(2,w*3,3)],
               
               p_hnh = Diag(Qq,1)[seq(1,w*3,3)],
               p_hd = Diag(Qq,2)[seq(1,w*3,3)],
               p_nhd = Diag(Qq,1)[seq(2,w*3,3)],
               
               N1_h = N5q[seq(1,w*3,3),1],
               N1_nh = N5q[seq(2,w*3,3),1],
               N1_d = N5q[seq(3,w*3,3),1])


# graf
par(mfrow=c(1,1))
plot(Nq$Age, Nq$N0_h * 100, ylim = c(0,100), xlab='Age', ylab='%', main = 'Simulated population')
points(Nq$Age, Nq$N1_h * 100, col=4)
points(Nq$Age, Nq$N0_nh * 100, col=3)
points(Nq$Age, Nq$N1_nh * 100, col=2)
legend(35, 60, c("Healthy t", "Healthy t+5", "Not Healthy t", "Not Healthy t+5"), 
       col = c(1, 2, 3, 4), pch = c(1, 1, 1, 1), cex = 0.8, bty = "n")











































#####################################ENFR
################09
enfr09 <- read.table("Data/ENFR-2009 Base Usuario.txt",sep="|",header = T)
# head(enfr09);str(enfr09)
# BHCH05 Edad (Edad en años cumplidos)
# BIHA01 ¿Alguna vez le han tomado la presión
# arterial?
# 1 Sí
# 2 No
# 9 Ns/nc 
# PRV_HA Prevalencia de hipertensión arterial
# 1 Sí
# 2 No
# 9 Ns/Nc
HA_60_09 <- sum(enfr09$PRV_HA[enfr09$BIHA01==1 & enfr09$BHCH05==60 & enfr09$PRV_HA==1])
T_60_09 <- sum(enfr09$PRV_HA[enfr09$BIHA01==1 & enfr09$BHCH05==60])
P_60_09 <- HA_60_09/T_60_09

#################13
enfr13 <- read.table("Data/ENFR2013_baseusuario.txt",sep="|",header = T)
# head(enfr13);str(enfr13)
# BHCH05 Edad (Edad en años cumplidos)
# BIHA01 ¿Alguna vez le han tomado la presión arterial?
# 1 Sí
# 2 No
# 9 Ns/Nc 
# PREVALENCIA_HIPERTENSION Prevalencia de hipertensión arterial
# 1 Sí
# 2 No
# 9 Ns/Nc 
# BIAC01_01
# ¿Alguna vez un médico, enfermera u otro profesional de la salud le dijo que tuvo un ataque cardíaco, también llamado infarto de miocardio?
# 1 Sí
# 2 No
# 9 Ns/Nc
# BIAC01_02
# ¿Alguna vez un médico, enfermera u otro profesional de la salud le dijo que tuvo un accidente cerebrovascular o derrame o ACV?
# 1 Sí
# 2 No
# 9 Ns/Nc 

# no excluyentes
# table(enfr13$PREVALENCIA_HIPERTENSION,enfr13$BIAC01_02)
# table(enfr13$BIHA01)
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

## take from 30
Can = Can[Can$age_li >= 30,]

# graph
g1 = cbind(Can[,-c(6,7,8)], PrInc = 'Prev'); g1$val = g1$Can_prev; g1$Can_prev = NULL
g2 = cbind(Can[,-c(5,7,8)], PrInc = 'Inc'); g2$val = g2$Can_inc; g2$Can_inc = NULL
g3 = cbind(Can[,-c(5,6,8)], PrInc = 'm'); g3$val = g3$Can_m; g3$Can_m = NULL
g4 = cbind(Can[,-c(5,6,7)], PrInc = 'rr'); g4$val = g4$Can_rr; g4$Can_rr = NULL
Can_graph = rbind(g1, g2, g3, g4)
Fig1 = ggplot(data = Can_graph[1:24,]) +
              geom_bar(aes(x = age_group, y = val * 100, fill = PrInc),
                       stat = "identity", position = "dodge") +
                  labs(title = "Figure 1. Hypertension in Canada",
                       subtitle = "2012",
                       caption = "Source: Robitaille et.al. (2012)",
                       x = "Age",
                       y = "%",
                       colour = "Gears") +
                  theme_classic()

## smooth splines
ages_pred = 30:80 

plot(Can$age_avg, Can$Can_inc)
Can_inc_s = smooth.spline(Can$age_avg, Can$Can_inc)
Can_inc_s = predict(Can_inc_s, x = ages_pred)$y
lines(ages_pred, Can_inc_s, col=2)

plot(Can$age_avg, Can$Can_prev)
Can_prev_s = smooth.spline(Can$age_avg, Can$Can_prev)
Can_prev_s = predict(Can_prev_s, x = ages_pred)$y
lines(ages_pred, Can_prev_s, col=2)

plot(Can$age_avg, Can$Can_rr)
Can_rr_s = smooth.spline(Can$age_avg, Can$Can_rr)
Can_rr_s = predict(Can_rr_s, x = ages_pred)$y
lines(ages_pred, Can_rr_s, col=2)

plot(Can$age_avg, Can$Can_m)
Can_m_s = smooth.spline(Can$age_avg, Can$Can_m)
Can_m_s = predict(Can_m_s, x = ages_pred)$y
lines(ages_pred, Can_m_s, col=2)

Can_mr_s = Can_rr_s * Can_m_s
lines(ages_pred, Can_mr_s, col=3)

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

# transition matrix probability
Q = expm(M)
rowSums(Q)

#pop
NH = Can_prev_s
H = 1-NH
N0 = rep(0,50*3)
N0[seq(1,50*3-2,3)] = H 
N0[seq(2,50*3-1,3)] = NH 
dim(Q); length(N0)
N1 = N0 %*% Q
N1_h = N1[seq(1,50*3-2,3)]/(N1[seq(1,50*3-2,3)]+N1[seq(2,50*3-1,3)])
N1_nh = N1[seq(2,50*3-1,3)]/(N1[seq(1,50*3-2,3)]+N1[seq(2,50*3-1,3)])
N1_h+N1_nh
plot(30:79, N1_nh-NH[-51])
plot(30:80, NH, t='l')
lines(30:79, N1_nh, col=2)
















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
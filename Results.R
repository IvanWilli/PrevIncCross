
#Results
GY_r = GY_f(N$N0_nh, N$N1_nh, N$p_d, a1.li, a1.ls, b1.li, b1.ls, a2.li, a2.ls, b2.li, b2.ls)
ipf_r = IPF_f(N0, N1)
Schm_r = R_funct(c(N$N0_nh, Can_prev_s[41]), c(N$N0_nh[1], N$N1_nh), N$p_d, N$p_nhd)
Trans_r = Transp_f(N)

#Prevalence replicated (no observational)
GY_r$N1 = N$N0_h * GY_r$p_hnh.hat + N$N0_nh * (1-GY_r$p_nhd)
ipf_r$N1 = N$N0_h * ipf_r$p_hnh.hat.ipf + N$N0_nh * (1-ipf_r$p_nhd.hat.ipf)
Schm_r$N1 = N$N0_h * Schm_r$p_hnh.hat + N$N0_nh * (1-N$p_nhd)
Trans_r$N1 = N$N0_h * Trans_r$p12 + N$N0_nh * (1-Trans_r$p23)

#rr
ipf_r$rr.hat = ipf_r$p_nhd.hat.ipf/ipf_r$p_hd.hat.ipf
Trans_r$rr.hat = Trans_r$p23 / Trans_r$p13

# Accuracy
esm = function(x,y) {sum((x-y)^2/y)*100}

errors = data.frame(GY = esm(GY_r$N1, N$N1_nh),
                    IPF = esm(ipf_r$N1, N$N1_nh),
                    Rm = esm(Schm_r$N1, N$N1_nh),
                    Tr = esm(Trans_r$N1, N$N1_nh))

errors[2,] = c(esm(GY_r$p_hnh.hat, N$p_hnh),
               esm(ipf_r$p_hnh.hat.ipf, N$p_hnh),
               esm(Schm_r$p_hnh.hat, N$p_hnh),
               esm(Trans_r$p12, N$p_hnh))

errors[3,] = c(esm(GY_r$p_nhd, N$p_nhd), esm(ipf_r$p_nhd, N$rr), NA, esm(Trans_r$p23, N$rr))

rownames(errors) = c('Prevalence', 'H->NH', 'mortHTA')

t(errors)

write.csv(t(errors), "errors.csv")

## prevalencia
ages = 30:69
plot(ages, N$N1_nh,  t = "o", main = 'H->NH', ylim=c(0,.8), xlab = 'Ages',
     ylab='Probability', cex = 0.8)
points(ages, ipf_r$N1, t = "o", cex = 0.8, col=2)
points(ages, Schm_r$N1, t = "o", cex = 0.8, col=3)
points(ages, GY_r$N1, t = "o", cex = 0.8, col=4)
points(ages, Trans_r$N1, t = "o", cex = 0.8, col=6)
legend(40, .6, legend=c('Obs', 'IPF', 'r-method', 'GY', 'Transport'),
       lty = c(1), pch= c(1), col=c(1:4,6), bg =  "n", bty =  "n")



par(mfrow = c(1,2),mai = c(1, 0.8, .3, 0.1))
## incidencia
plot(ages, N$p_hnh,  t = "o", main = 'P(H->HTA)', ylim=c(0,0.1), xlab = 'Age', ylab='Probability', cex = 0.8)
points(ages, ipf_r$p_hnh.hat.ipf, t = "o", cex = 0.8, col=2)
# points(ages, Schm_r$p_hnh.hat, t = "o", cex = 0.8, col=3)
points(ages, GY_r$p_hnh.hat, t = "o", cex = 0.8, col=4)
legend(35, .1, legend=c('Observed (smooth)', 'IPF', 'Intercensal'),
       lty = c(1), pch= c(1), col=c(1,2,4,6), bg =  "n", bty =  "n")

## mort HTA
plot(ages, N$p_nhd,  t = "o", main = 'P(HTA->D)', ylim=c(0,0.03), xlab = 'Age', ylab='', cex = 0.8)
points(ages, ipf_r$p_nhd.hat.ipf, t = "o", cex = 0.8, col=2)
points(ages, GY_r$p_nhd, t = "o", cex = 0.8, col=4)
legend(35, .03, legend=c('Observed (smooth)', 'IPF',  'Intercensal'),
       lty = c(1), pch= c(1), col=c(1,2,4), bg =  "n", bty =  "n")



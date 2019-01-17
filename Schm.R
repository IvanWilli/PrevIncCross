
########################### EDAD SIMPLE

# edad exacta por splines
N_Schm = data.frame(N, m = Can_m_s[-51], rr = Can_rr_s[-51])
plot(N_Schm$N0_nh, t='l')
lines(N_Schm$N1_nh, col=4)
N_Schm$N0_nh-N_Schm$N1_nh

# mortalidad gral
m_gral = N_Schm$m*N_Schm$rr*Can_prev_s[-51]+N_Schm$m*(1-Can_prev_s[-51])
plot(m_gral)
points(N_Schm$m, col=4)
points(N_Schm$m*N_Schm$rr, col=2)

# incidence rate
i = (1/2/1+1/2/1)*(N_Schm$N1_nh[-1]-N_Schm$N0_nh[-50]
                   +N_Schm$N0_nh[-1]-N_Schm$N1_nh[-50])

i = i + m_gral[-50] * (N_Schm$rr[-50]-1) * (N_Schm$N1_nh[-1]+N_Schm$N0_nh[-50]+
                                              N_Schm$N0_nh[-1]+N_Schm$N1_nh[-50])/4

# graph
plot(log(Can_inc_s))
points(log(i), col=3)
plot(i/Can_inc_s)

########################## QUINQUENAL

# edad exacta por splines
plot(Nq$N0_nh, t='p', ylim=c(0,.8))
points(Nq$N1_nh, col=4)

# mortalidad gral
m_gral = Nq$m_nhd * Nq$N0_nh + Nq$m_hd * Nq$N0_h
plot(m_gral, ylim = c(0,0.08))
points(Nq$m_hd, col=4)
points(Nq$m_nhd, col=2)

# incidence rate
n = 5
t = 5

A = Nq$N0_nh[-11]
D = Nq$N1_nh[-11]
B = Nq$N0_nh[-1]
C = Nq$N1_nh[-1]
length(A);length(B);length(C);length(D)

i = (1/(2*n)+1/(2*t))*(C-A)+ (1/(2*n)-1/(2*t))*(B-D)

i = i + m_gral[-11] * (Can$Can_rr[-11] - 1) * (A+B+C+D)/4

# graph
plot(Can$age_li[-11], log(Can$Can_inc)[-11])
points(Can$age_li[-11], log(i), col=3)
plot(Can$age_li[-11], i/Can$Can_inc[-11])
abline(h=1, lty=2, col=4)

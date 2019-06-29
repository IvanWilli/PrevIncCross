
########################### EDAD SIMPLE

R_funct = function(N0_nh, N1_nh, p_d, p_nhd){
  
  # incidence rate
  
  n = 1
  t = 1
  

  A = N0_nh[-41]
  D = N1_nh[-41]
  B = N0_nh[-1]
  C = N0_nh[-1]
  
  #A = N0_nh
  #D = N1_nh
  #B = c(N0_nh[-1], Can_prev_s[51])
  #C = c(N0_nh[1], N1_nh[-1])
  #length(A);length(D);length(B);length(C)
  
  i0 = (1/(2*n)+1/(2*t))*(C-A)+ (1/(2*n)-1/(2*t))*(B-D)
  
  i1 = i0
  # i1 = i0 + (p_nhd - p_d) * (A+B+C+D)/4
  
  #plot(i0); points(i1, col=2)
  
  # convertirlo en probabilidad
  p_hnh.hat.r = i1 / (1-N0_nh[-41])
  
  return(data.frame(p_hnh.hat = p_hnh.hat.r))
}




R_funct(c(N$N0_nh, Can_prev_s[41]), c(N$N0_nh[1], N$N1_nh), N$p_d, N$p_nhd)
















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

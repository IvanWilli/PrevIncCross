

# result = GY_f(N$N0_nh, N$N1_nh, N$p_d, a1.li, a1.ls, b1.li, b1.ls, a2.li, a2.ls, b2.li, b2.ls)

#### estimacion

GY_f = function(N0_nh, N1_nh, q, 
                a1.li, a1.ls, b1.li, b1.ls, a2.li, a2.ls, b2.li, b2.ls){
  
  # N0_nh = N$N0_nh
  # N1_nh = N$N1_nh
  # q = N$p_d
  
  #### Prepara data
  prev1 = N0_nh
  prev2 = N1_nh
  
  #optimization
  solOk_0913<-data.frame(a1=NA,b1=NA,a2=NA,b2=NA,c2=NA, con=NA, val=NA, err=NA)
  
  library("alabama")
  set.seed(2378)
  
  sol0913<-auglag_funcion(prev1,prev2,q)
  
  solOk_0913[i,]<-head(sol0913[order(sol0913$val),],2)[1,]
  
  #### resultados
  
  sol0913=subset(solOk_0913,!is.na(err))
  
  p_hnh.hat<-data.frame()
  rr.hat<-data.frame()
  
  for (i in 1:nrow(sol0913)){
    a1<-sol0913[i,1]; b1<-sol0913[i,2]
    a2<-sol0913[i,3]; b2<-sol0913[i,4]; c2<-sol0913[i,5]
    ord<-1
    for (j in 30:69) {
      p_hnh.hat[i,ord]<-inc(j)
      rr.hat[i,ord]<-rr(j)
      ord<-ord+1}
  }
  
  p_hnh.hat = as.numeric(p_hnh.hat)
  rr.hat = as.numeric(rr.hat)
  no_prev1 = 1 - prev1
  no_prev2 = 1 - prev2
  p_hd <- as.numeric(q/(no_prev1+prev1 * rr.hat))
  p_nhd <- as.numeric(p_hd * rr.hat)
  
  out = data.frame(p_hnh.hat=p_hnh.hat, rr.hat=rr.hat, p_hd=p_hd, p_nhd=p_nhd)
  
  return(out)
}

##############aulag

auglag_funcion<-function(prev1, prev2, q){
  
  ###Formas funcionales por edad (se le llama inc a q(HTA) erróneamente)
  rr<-function(x) {a1*exp(x*b1)}
  der_rr<-function(x) {a1*b1*exp(x*b1)}
  inc<-function(x) {a2*exp(x*b2)}
  der_inc<-function(x) {a2*b2*exp(x*b2)}
  
  #complemento (sanos)
  Noprev1<-1-prev1
  Noprev2<-1-prev2
  
  #agregación en letras según GY(2009)
  
  Y<-c();B<-c();C<-c();D<-c()
  
  for (i in 1:length(q)){
    Y[i]<-Noprev2[i]-Noprev1[i]/(1-q[i])
    B[i]<-Noprev1[i]*q[i]/(1-q[i])
    C[i]<-Noprev1[i]
    D[i]<-Noprev1[i]/(1-q[i])}
  
  #edades de cohortes al 2009
  x<-30:69
  
  #función objetivo
  fn<-function(p){
    sum((Y+B/(C+(1-C)*p[1]*exp(x*p[2]))+D*p[3]*exp(x*p[4]))^2)
  }
  
  #sistema de restricciones de desigualdad (bandas)
  hin<-function(p){
    
    h <- rep(NA, 70)
    cont<-1
    for (k in seq(30,69,1)){
      #rr
      h[cont] <- p[1]*exp(k*p[2])-a1.li*exp(k*b1.li)
      cont<-cont+1
      h[cont] <- a1.ls*exp(k*b1.ls)-p[1]*exp(k*p[2])
      cont<-cont+1
      h[cont] <- -p[1]*p[2]*exp(k*p[2])
      cont<-cont+1
      #inc
      h[cont] <- p[3]*exp(k*p[4])-a2.li*exp(k*b2.li)
      cont<-cont+1
      h[cont] <- a2.ls*exp(k*b2.ls)-p[3]*exp(k*p[4])
      cont<-cont+1
      h[cont] <- p[3]*p[4]*exp(k*p[4])
      cont<-cont+1
    }
    
    #parámetros
    nn = cont
    h[nn]<-p[1]-a1.li
    h[nn+1]<-a1.ls-p[1]
    h[nn+2]<-p[2]-b1.li
    h[nn+3]<-b1.ls-p[2]
    h[nn+4]<-p[3]-a2.li
    h[nn+5]<-a2.ls-p[3]
    h[nn+6]<-p[4]-b2.li
    h[nn+7]<-b2.ls-p[4]
    
    return(h)
  }
  
  #restricciones de igualdad (rr=1.1 en edades finales)
  heq<-function(p){
    h<-rep(NA,1)
    h[1]<-p[1]*exp(100*p[2])-1.1
    h
  }
  
  #loopeo soluciones
  soluciones<-data.frame(a1=NA,b1=NA,a2=NA,b2=NA,c2=NA,con=NA, val=NA, err=NA)
  
  m<-30:69 #edades de chekeo de restr
  a1<-c();b1<-c();a2<-c();b2<-c()
  
  for (i in 1:1){ #si se quiere se puede randomizar la solución inicial. Converge
    # i = 11
    randoma1<-runif(1)
    randomb1<-runif(1)
    randoma2<-runif(1)
    randomb2<-runif(1)
    a1_i<-randoma1*a1.li+(1-randoma1)*a1.ls
    b1_i<-randomb1*b1.li+(1-randomb1)*b1.ls
    a2_i<-randoma2*a2.li+(1-randoma2)*a2.ls
    b2_i<-randomb2*b2.li+(1-randomb2)*b2.ls
    iniciales<-c(a1_i,b1_i,a2_i,b2_i)
    
    #optim
    solucion <- auglag(par=iniciales, fn=fn, hin=hin, heq=heq,
                       control.outer = list(itmax= 300, method="nlminb"))
    
    #control de soluciones
    soluciones[i,1]<-solucion$par[1]
    soluciones[i,2]<-solucion$par[2]
    soluciones[i,3]<-solucion$par[3]
    soluciones[i,4]<-solucion$par[4]
    soluciones[i,5]<-0
    soluciones[i,6]<-solucion$convergence
    soluciones[i,7]<-solucion$value
    soluciones[i,8]<-0
    a1<<-soluciones[i,1];b1<<-soluciones[i,2]
    a2<<-soluciones[i,3];b2<<-soluciones[i,4]
    
    
      if (any(inc(m)<0) | any(inc(m)>1) | any(der_inc(m)<0) | 
          any(rr(m)<1) | any(rr(m)>10) | any(der_rr(m)>0))
      {soluciones[i,8]<-1}
    
  }
  solOK<-subset(soluciones, soluciones$err==0)
  return(solOK)
}

############bandas

###set de bandas para rr y q(HTA)

#rr
rr_li<-c(2,1)
rr_ls<-c(4,2.5)
x_l<-c(Can$age_li[1],Can$age_ls[8])
dat_li=data.frame(rr_li,x_l)
dat_ls=data.frame(rr_ls,x_l)
model_li <- lm(log(rr_li) ~ x_l + 1) 
model_ls <- lm(log(rr_ls) ~ x_l + 1)
x <- data.frame(x_l=seq(30,70,5))
pred_li_rr<-data.frame(x=x,p=exp(predict(model_li,x)))
pred_ls_rr<-data.frame(x=x,p=exp(predict(model_ls,x)))
a1.li<-exp(model_li$coefficients[1])
b1.li<-model_li$coefficients[2]
a1.ls<-exp(model_ls$coefficients[1])
b1.ls<-model_ls$coefficients[2]

#gráfico de bandas
plot(pred_ls_rr$x_l, pred_ls_rr$p, t='o', ylim = c(1,6))
points(pred_li_rr$x_l, pred_li_rr$p, t='o')
points(Can$age_avg, Can$Can_rr, t='o', lty=2)
points(N$Age, N$rr, t='o', lty=2)

#p(HTA)
inc_li<-c(.001, .02)
inc_ls<-c(.01, 0.15)
x_l<-c(Can$age_li[1],Can$age_li[8])
model_li <- lm(log(inc_li) ~ x_l + 1) 
model_ls <- lm(log(inc_ls) ~ x_l + 1)
x <- data.frame(x_l=seq(30,69,5))
pred_li_inc<-data.frame(x=x,p=exp(predict(model_li,x)))
pred_ls_inc<-data.frame(x=x,p=exp(predict(model_ls,x)))
a2.li<-exp(model_li$coefficients[1])
b2.li<-model_li$coefficients[2]
a2.ls<-exp(model_ls$coefficients[1])
b2.ls<-model_ls$coefficients[2]

#gráfico de bandas
plot(pred_ls_inc$x_l, pred_ls_inc$p, t='o', ylim = c(0,.2))
points(pred_li_inc$x_l, pred_li_inc$p, t='o')
points(N$Age, N$p_hnh, t='o', lty=2)



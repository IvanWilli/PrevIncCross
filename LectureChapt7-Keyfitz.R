#Markov Chain
# See too multiple classifications> age+health
t=t(matrix(c(.8, .13, .05,
              0, .8 , .1 ,
              0,  0 , .6), 3, 3, byrow = T))
d=1-colSums(t)
#cardinales
dim(t);length(d)
#there are no immortal stages in the life cycle
#column-stochastic
P=cbind(rbind(t,d),c(0,0,0,1))


#reach the state “dead” from every state t guarantees that the dominant eigenvalue of t is strictly less than 1
#thus lim t→∞ Tt = 0
t^10000
eigen(t)

#proy
x_0=c(.91,.05,.04,0)
x_t=x_0
plot(x = x_0[4], y=1 , ylim = c(0,50) , xlim = c(0,1))
for (i in 1:50){
  #i=2
  x_t=P%*%x_t
  points(y = i, x = x_t[4], col = i)
}

#fundamental matrix
E_v=solve(diag(1,3)-t)
#expected time to absortion
colSums(E_v)
#variance of time to absortion
Var_v=(2*diag(diag(E_v),3,3)-diag(1,3,3))%*%E_v-E_v*E_v
#coef variation
Var_v^.5/E_v

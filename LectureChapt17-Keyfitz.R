#####################Ch17
##################### .2 y .3
#supongo...
md1=.05
md2=.07
md3=.08
m12=.01
m21=.015
m23=.02
m32=.012
m31=.013
m13=.02
n0=c(10,10,10)

#matriz de decrementos instantanos
m=matrix(c(md1+m21+m31  ,-m12        ,-m13,
          -m21          ,md2+m12+m32 ,-m23,
          -m31          ,-m32        ,md3+m13+m23) , 3 , 3)
10*(-m12-m13+(md1+m21+m31)) #decrementos de estado 1
rowSums(t(m)*n0) #decrementos 

#discretizar hacia probabilidades
#con 3 autovalores distintos, hay 3 valores de l(x) tal que (Gantmacher 1959,vol. 2, p. 113)
      # dl(x)/dx=-m(x)*l(x) 
      # dl(x)/dx/l(x)=-m(x)
      # dln(l(x))/dx=-m(x)
      # l(x+t)=l(x)*exp(-m(x)*t)
# M(x) will be the ﬁnite approximation to µ(x). It is obtained as raw data by dividing the transitions from the jth state to the ith state by the exposure
# Partiendo el espacio en partes pequenias donde M(x) sea constante, siguiendo el hecho de que L(y)=L(y/x)L(x)
# L(x)ij --> probability that a new born in state j be at state i at age x 
  #L(x+h)=exp(h * -M(x)) %*% L(x)
# expansion de Taylor hasta grado 2: exp(x)~1+x
  #L(x+h) =(I - h * M(x)) * L(x)
#Tiempo Vivido entre x y x+h
  #C_x_h = (h/2)*(Lx + L_x_h)

###Application
# suponiendo estados iniciales
L_0=rep(1,3)
# if always the same risk in all ages...
M=m
h=5
L=matrix(0,21,3); C=matrix(0,21,3)
length(seq(0,95,5))
for (i in seq(0,95,5)/5+2){
  #i=4
  L[1,]=L_0
  L[i,]=rowSums(t(diag(1,3)-h*M)*L[i-1,])
  C[i-1,]=(L[i-1,]+L[i,])*h/2
} 
C=cbind(seq(0,100,5),C)
L=cbind(seq(0,100,5),L)

#Expectancy times in states
  #eij_x=T(x,i)L(x,j) --> in state j in x, expected time in state i 
e12_20=sum(C[,2][C[,1]>=20])/L[,3][L[,1]==20]

################## .4
# Si no es singular puedo por despeje obtener matriz
# filas deben sumar 1? No! pueden morir tmb
L0=diag(1,3)
L50=matrix(c(.8 , .13, .05,
              0 , .8 , .1 ,
              0 , 0  , .6), 3, 3, byrow = T)
det(L50)==prod(diag(L50)) 
L50%*%L0
L55=matrix(c(.7 , .15, .1,
              0 , .7 , .2 ,
              0 , 0  , .5), 3, 3, byrow = T)
S50_55=L55%*%solve(L50) #pero L singular
rowSums(M50_55);colSums(M50_55)
S50_55%*%L50 #ok!
#Pero no puedo armar la matriz L (no tengo partida, solo destino)       
##############################################
       


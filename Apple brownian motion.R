#Predicciones utilizando el modelo de Black-Scholes.
library(readxl)

Datos <- read_excel("~/Dropbox/Master Análisis de datos/Calculo Estocastico/AAPL.xls")
Datos
library(MASS)
fomega<-function(t){
  n<-length(t)
  d<-rep(0,n)
  d[1]<-rnorm(1,0,sqrt(t[1]))
  if (n>1){
    for (i in 2:n) d[i]<-d[i-1]+
        rnorm(1,0,sqrt(t[i]-t[i-1]))
  }
  ;d}
brownexp<-function(mu,sigma,t) {
  d<-exp(sigma*fomega(t)+mu*t)
  ;d}

#9.1
valor <- as.data.frame(Datos[,2])
log<-rep(0,60) 
for (i in 1:60){
  log[i]<-log(valor[i+1,]/valor[i,])
}
shapiro.test(log)
names(valor)
#9.2 
hist(log, scale="density", breaks="Sturges",
     col="darkgray",
     main=expression(paste("Histograma de log ",
                           frac(R[t[i]],R[t[i-1]]))))


#9.3
delta<-1/253
mu<-mean(log)/delta
sigma<-sd(log)/sqrt(delta)
mu 
sigma

#9.4
x<-seq(1/253,61/253,1/253)
plot(x,valor[,],type="l",
     col="red", main="Evolución de las acciones de Apple",
     xlab="Tiempo desde 01/02/2018 a 30/04/2018
     (1 día = 1/253 unidades)",
     ylab="Valor de la acción")
#9.5
t<-c(1/253) #Solo es un dia, es como empezar de cero debido a la propiedad de reproductividad.
a<-rep(0,10000)
for (i in 1:10000){
  a[i]<-162.32*brownexp(mu,sigma,t)
}
espe<-mean(a)
inferior<-quantile(a,0.025)
superior<-quantile(a,0.975)
espe
inferior
superior

#9.6
t<-seq(1/253,7/253,1/253)
a<-matrix(1:(10000*7),ncol=7,byrow=TRUE)
for (i in 1:10000){
  a[i,]<-162.32*brownexp(mu,sigma,t)}
inferior<-rep(0,7)
espe<-rep(0,7)
superior<-rep(0,7)
for (i in 1:7){
  inferior[i]<-quantile(a[,i],0.05)
  espe[i]<-mean(a[,i])
  superior[i]<-quantile(a[,i],0.95)}
inferior
espe
superior

#9.7
k<-7
alpha<-0.1
t<-seq(1/253,k/253,1/253)
a<-matrix(1:(10000*k),ncol=k,byrow=TRUE)
for (i in 1:10000){
  a[i,]<-162.32*brownexp(mu,sigma,t)}

inferior<-rep(0,k)
espe<-rep(0,k)
superior<-rep(0,k)
for (i in 1:k){
  inferior[i]<-quantile(a[,i],alpha/2)
  espe[i]<-mean(a[,i])
  superior[i]<-quantile(a[,i],1-(alpha/2))}
inferior
espe
superior
#9.7 BIS
prediccion<-function(k,alpha){
  t<-seq(1/253,k/253,1/253)
  a<-matrix(1:(10000*k),ncol=k,byrow=TRUE)
  for (i in 1:10000){
    a[i,]<-162.32*brownexp(mu,sigma,t)
  }
  inferior<-rep(0,k)
  espe<-rep(0,k)
  superior<-rep(0,k)
  for (i in 1:k){
    inferior[i]<-quantile(a[,i],alpha/2)
    espe[i]<-mean(a[,i])
    superior[i]<-quantile(a[,i],1-(alpha/2))}
  
  b<-matrix(1:(k*3),ncol=3,byrow=TRUE)
  rownames(b)<-rownames(b,do.NULL=FALSE,prefix="Día ")
  columnas<-c("Inferior", "Esperanza", "Superior")
  colnames(b)<-columnas
  for (i in 1:k){
    b[i,]<-c(inferior[i],espe[i],superior[i])}
  ;b}
prediccion(7,.10)

#9.8
t<-seq(1/253,61/253,1/253)
plot(t,valor[,],type = "l",
     main="Predicción del valor de las acciones
     de Apple en el mes próximo",
     xlab="Tiempo", ylab="Valor", col = "blue",
     xlim=c(0,(61+6)/253),ylim=c(155,189))

#9.9
pred<-prediccion(6,.10)
pred

#9.10
inferior<-rep(0,6+1)
espe<-rep(0,6+1)
superior<-rep(0,6+1)
inferior[1]<-valor[61,]
espe[1]<-valor[61,]
superior[1]<-valor[61,]
for (i in 2:(6+1)){
  inferior[i]<-pred[i-1,1]
  espe[i]<-pred[i-1,2]
  superior[i]<-pred[i-1,3]
}

tpred<-seq(61/253,(61+6)/253,1/253)
lines(tpred,inferior,col="red",lwd=2)
lines(tpred,espe,col="green",lwd=2)
lines(tpred,superior,col="red",lwd=2)

#9.11
real<-c(162.32,165.26,169.10,176.57,176.89,183.83,185.16)
lines(tpred,real,col="blue",lwd=2)

#9.12
t<-seq(0,6/253,1/253)
a<-162.32*brownexp(mu,sigma,t)
lines(tpred,a,col="orange")

#9.13
for (i in 1:100){
  a<-162.32*brownexp(mu,sigma,t)
  lines(tpred,a,col="orange")
}
lines(tpred,inferior,col="red",lwd=2)
lines(tpred,espe,col="green",lwd=2)
lines(tpred,superior,col="red",lwd=2)
lines(tpred,real,col="blue",lwd=2)

#9.14
probmax<-function(k,M){ 
  a<-0
  t<-seq(1/253,k/253,1/253)
  for (i in 1:1000){
    b<-162.32*brownexp(mu,sigma,t)
    if (max(b)> M) a=a+1 #Maximo porque tienes que comprobar que de todos los valores el primero que esté por encima de dicho valor.
  } ;a/1000}
probmax(19,7.0)
probmax(19,6.5)

#9.15
probmin<-function(k,A){
  a<-0
  t<-seq(1/253,k/253,1/253)
  for (i in 1:1000){
    b<-5.83*brownexp(mu,sigma,t)
    if (min(b)< A) a=a+1 #Min para que siempre estemos por encima de dicho minimo
  } ;a/1000}
probmin(19,5.0)
probmin(19,5.5)

pfad<-"~/00Master/bodenphysik/"
plotpfad<-"~/00Master/bodenphysik/protokoll/"
sarpy<-read.csv2(paste0(pfad,"sarpyloam.csv"))
sarpy$Psi.cm[20]<--1
plot(log10(-sarpy$Psi.cm),sarpy$Theta,type="l",xlab="pF-Wert",ylab=expression(theta))
thetas<-max(sarpy$Theta)
a<-sarpy$Theta[1:3]
b<-log10(-sarpy$Psi.cm)[1:3]
plot(a~b,xlab="pF-Wert",ylab=expression(theta))
fm<-lm(a~poly(b,2))
x<-seq(0,6,length.out = 100)
lines(x, predict(fm, data.frame(b=x)), col="red")
thetar<-predict(fm, data.frame(b=4.2))
#thetar<-min(sarpy$Theta)
sarpy$thetatilde<-(sarpy$Theta-thetar)/(thetas-thetar)
plot(log10(-sarpy$Psi.cm),sarpy$thetatilde,type="l",xlab="pF-Wert",ylab=expression(theta[Tilde]))
a<-sarpy$thetatilde
b<-log10(-sarpy$Psi.cm)
plot(a~b,xlab="pF-Wert",ylab=expression(theta))
fm<-lm(a~poly(b,4))
x<-seq(0,5,length.out = 100)
sarpy$psi<-sarpy$Psi.cm
lines(x, predict(fm, data.frame(b=x)), col="red")
sarpy$psi[20]<-0
mua<-nls(sarpy$thetatilde~(1+(-al*sarpy$psi)^N)^-(1-1/N),data=sarpy,start = list(al=0.02,N=1.2))
alpha<-coef(mua)[1]
n<-coef(mua)[2]
sarpy$fit<-(1+(-alpha*sarpy$psi)^n)^-(1-1/n)
muafit<-function(alpha,n,theta){return((1+(-alpha*theta)^n)^-(1-1/n))}

fit<-(1+(-alpha*seq(-6975,-1,length.out=5000))^n)^-(1-1/n)
plot(fit~log10(-seq(-6975,-1,length.out=5000)),type="l")
points(a~b)

plot(sarpy$k..cm.d.~sarpy$thetatilde)
kf<-max(sarpy$k..cm.d.)
m<-1-1/n
fun<-function(kf,theta,m){return(kf*sqrt(theta)*(1-(1-theta^(1/m))^m)^2)}

sarpy$k<-kf*sqrt(sarpy$thetatilde)*(1-(1-sarpy$thetatilde^(1/m))^m)^2

lines(fun(kf,sarpy$thetatilde,m)~sarpy$thetatilde)
mfit<-nls(sarpy$k..cm.d.~kf*sqrt(sarpy$thetatilde)*(1-(1-sarpy$thetatilde^(1/M))^M)^2,data=sarpy,start=list(M=1-1/n))
malt<-coef(mfit)
lines(fun(kf,sarpy$thetatilde,malt)~sarpy$thetatilde)
plot(muafit(alpha,(1/(1-malt)),sarpy$psi)~log10(-sarpy$Psi.cm),type="l")
points(a~b)
